package model.service.impl.pipeline

import akka.actor.{PoisonPill, Props}
import controllers.api.JsonImplicits._
import controllers.api.{PortCheckResult, ProgressReporter}
import model.entity.ComponentType.ComponentType
import model.entity._
import model.repository.PipelineDiscoveryRepository
import model.service.component.{Component, InternalComponent}
import model.service.{PartialPipeline, PipelineService, PortMapping, SessionScoped}
import play.api.Play.current
import play.api.db.slick.Session
import play.api.libs.concurrent.Akka
import play.api.libs.json.{JsObject, JsString, Json}
import scaldi.{Injectable, Injector}
import utils.CombinatoricsUtils

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}

object PipelineDiscoveryAlgorithm {
  val MaxIterations = 5
}

class PipelineDiscoveryAlgorithm(
  allComponentsByType: Map[ComponentType, Seq[SpecificComponentTemplate]],
  reporterProps: Props,
  maybeGeneralDs: Option[DataSourceTemplate] = None,
  maxIterations: Int = PipelineDiscoveryAlgorithm.MaxIterations
  )
  (implicit val inj: Injector, implicit val session: Session) extends SessionScoped with Injectable {

  val pipelineDiscoveryRepository = inject[PipelineDiscoveryRepository]
  val pipelineService = inject[PipelineService]
  val typesToBeAdded = Seq(ComponentType.Analyzer, ComponentType.Transformer, ComponentType.Visualizer)
  val discovery = PipelineDiscovery(None, isFinished = false, isSuccess = None, lastPerformedIteration = None, pipelinesDiscoveredCount = None)
  val discoveryId = pipelineDiscoveryRepository.save(discovery)(session)
  val reporter = Akka.system.actorOf(reporterProps)


  def discoverPipelines(dataSources: Seq[DataSourceTemplate])(implicit session: Session): PipelineDiscoveryId = {
    reportProgress(discovery.lastPerformedIteration, discovery.isFinished, discovery.pipelinesDiscoveredCount, discovery.isSuccess)
    iterate(0, pipelinesFromDataSources(dataSources), Seq())
    discoveryId
  }

  def iterate(iterationNumber: Int, givenPartialPipelines: Seq[PartialPipeline], givenCompletedPipelines: Seq[PartialPipeline]): Future[Seq[PartialPipeline]] = {

    reportMessage("****** Starting iteration " + iterationNumber + " with " + givenPartialPipelines.size + " partial pipeline(s)")

    val eventualCreatedPipelines = Future.sequence(tryAddAllComponents(givenPartialPipelines))
    eventualCreatedPipelines.map(_.flatten).flatMap { createdPipelines =>

      withSession { implicit session =>

        val createdPartialPartitions = createdPipelines.partition(_.componentInstances.last.hasOutput)
        val createdPartialPipelines = createdPartialPartitions._1
        val createdCompletedPipelines = createdPartialPartitions._2

        reportMessage("Created partial pipelines: " + createdPartialPipelines.size)

        val allCompleted = givenCompletedPipelines ++ createdCompletedPipelines

        reportMessage("All completed pipelines: " + allCompleted.size)

        try {
          val pipelinesToSave = if (maybeGeneralDs.isEmpty) {
            createdCompletedPipelines
          } else {
            createdCompletedPipelines.filter(_.componentInstances.exists(_.componentTemplateId == maybeGeneralDs.get.componentTemplateId))
          }
          pipelineService.saveDiscoveryResults(discoveryId, pipelinesToSave, reporter)

          reportMessage("Completed pipelines saved.")
          reporter ! Json.toJson(JsObject(Seq(("significantAction", JsString("pipelinesSaved")))))
        } catch {
          case e: Throwable => {
            e.printStackTrace()
          }
        }

        val usedPartialPipelines = givenPartialPipelines.map {
          case PartialPipeline(m, p, _) => PartialPipeline(m, p, used = true)
        }

        val allPartial = (usedPartialPipelines ++ createdPartialPipelines)

        reportMessage("All partial pipelines: " + allPartial.size)

        val stop = createdPartialPipelines.isEmpty || (iterationNumber > maxIterations)

        reportMessage("Stop?: " + stop)

        val futureResult = if (stop) {
          reportProgress(Some(iterationNumber), isFinished = true, Some(allCompleted.size), isSuccess = Some(true))
          reporter ! PoisonPill
          Future(allCompleted)
        } else {
          reportProgress(Some(iterationNumber), isFinished = false, Some(allCompleted.size), None)
          iterate(iterationNumber + 1, allPartial, allCompleted)
        }
        futureResult
      }
    }
  }

  private def reportProgress(iteration: Option[Int], isFinished: Boolean, pipelinesCount: Option[Int], isSuccess: Option[Boolean] = None) {
    val discovery = PipelineDiscovery(
      Some(discoveryId),
      isFinished,
      isSuccess,
      iteration,
      pipelinesCount
    )
    withSession { implicit session =>
      pipelineDiscoveryRepository.save(discovery)
    }
    reporter ! Json.toJson(discovery)
  }

  private def reportMessage(message: String): Unit = {
    reporter ! message
  }

  private def tryAddAllComponents(partialPipelines: Seq[PartialPipeline]): Seq[Future[Seq[PartialPipeline]]] = {
    withSession { implicit session =>
      reportMessage("Preparing components...")
      typesToBeAdded.flatMap { componentType =>
        reportMessage("Using " + componentType + "s")
        allComponentsByType(componentType).map { componentTemplateToAdd =>
          reportMessage("Will try to add <" + componentTemplateToAdd.componentTemplate.uri + ">")
          tryAdd(componentTemplateToAdd, partialPipelines)
        }
      }
    }
  }

  private def tryAdd(componentToAddSpecificTemplate: SpecificComponentTemplate, partialPipelines: Seq[PartialPipeline]): Future[Seq[PartialPipeline]] = {
    val promise = Promise[Seq[PartialPipeline]]()

    withSession { implicit session =>

      val componentToAdd = InternalComponent(componentToAddSpecificTemplate, ProgressReporter.props)
      val inputTemplates = componentToAddSpecificTemplate.componentTemplate.inputTemplates

      val eventualInputsCompatibility = tryBindAllInputs(inputTemplates, componentToAdd, partialPipelines, promise)
      eventualInputsCompatibility.onSuccess(inputBindingSuccessCallback(componentToAdd, promise))
      eventualInputsCompatibility.onFailure({
        case e => {
          reportMessage("ERROR: " + e)
          promise.tryFailure(e)
        }
      })

      promise.future
    }
  }

  private def tryBindAllInputs(inputTemplates: Seq[InputTemplate], componentToAdd: Component, partialPipelines: Seq[PartialPipeline], promise: Promise[Seq[PartialPipeline]])
    (implicit session: Session)
  : Future[Seq[Seq[Option[(PortMapping, PartialPipeline)]]]] = {

    val eventualPortResponses = inputTemplates.map { inputTemplate =>
      val futures = partialPipelines
        //.filter(!_.used)
        .filter(_.componentInstances.last.hasOutput)
        .collect {

        case partialPipeline if componentToAdd.componentInstance.componentTemplate.uri != partialPipeline.componentInstances.last.componentTemplate.uri =>


          val portUri = inputTemplate.dataPortTemplate.uri
          val lastComponent = InternalComponent(partialPipeline.componentInstances.last, ProgressReporter.props)

          reportMessage("Executing checks of <" + componentToAdd.componentInstance.componentTemplate.uri + ">")

          val future = componentToAdd.checkCouldBeBoundWithComponentViaPort(lastComponent, portUri, reporterProps)
          future.onFailure({ case e => {
            reportMessage("ERROR: " + e.getMessage)
            promise.tryFailure(e)
          }
          })
          future.collect({
            case true => {
              withSession { implicit session =>
                reportMessage("Able to bind <" + portUri + "> of <" + componentToAdd.componentInstance.componentTemplate.uri + "> to <" + lastComponent.componentInstance.componentTemplate.uri + ">")
                reporter ! PortCheckResult(true, portUri, componentToAdd.componentInstance.componentTemplate.uri, lastComponent.componentInstance.componentTemplate.uri)
                Some(PortMapping(partialPipeline.componentInstances.last, componentToAdd.componentInstance, portUri), partialPipeline)
              }
            }
            case false => {
              withSession { implicit session =>
                reportMessage("Unable to bind <" + portUri + "> of <" + componentToAdd.componentInstance.componentTemplate.uri + "> to <" + lastComponent.componentInstance.componentTemplate.uri + ">")
                reporter ! PortCheckResult(false, portUri, componentToAdd.componentInstance.componentTemplate.uri, lastComponent.componentInstance.componentTemplate.uri)
                None
              }
            }
          })
      }

      reportMessage("Waiting for " + futures.size + " response(s).")

      Future.sequence(futures)
    }

    reportMessage("Waiting for " + eventualPortResponses.size + " port(s) to respond.")

    Future.sequence(eventualPortResponses)
  }

  private def inputBindingSuccessCallback(componentToAdd: Component, promise: Promise[Seq[PartialPipeline]])
  : PartialFunction[Seq[Seq[Option[(PortMapping, PartialPipeline)]]], Unit] = {
    case f if f.exists(_.forall(_.isEmpty)) => {
      reportMessage("At least one port wasn't bound. Nothing from this branch.")
      promise.trySuccess(Seq())
    }
    case s =>
      val solutions = s.map(_.collect { case Some(y) => y })
      val solutionCombinations = CombinatoricsUtils.combine(solutions).filter { combination =>
        combination.exists { case (_, fragment) => !fragment.used } //at least one needs to be unused
      }

      reportMessage("Found " + solutionCombinations.size + " solution(s).")

      val createdPipelines = solutionCombinations.map { oneCombinationForAllPorts =>
        val pipelines = oneCombinationForAllPorts.map { case (_, pipeline) => pipeline }

        val componentInstances = pipelines.flatMap(_.componentInstances).:+(componentToAdd.componentInstance)
        val portMappings = pipelines.flatMap(_.portMappings) ++ oneCombinationForAllPorts.map { case (mapping, _) => mapping }
        PartialPipeline(componentInstances, portMappings)
      }

      reportMessage("Adding " + createdPipelines.size + " partial pipeline(s).")
      promise.trySuccess(createdPipelines)
  }

  private def pipelinesFromDataSources(componentTemplates: Seq[SpecificComponentTemplate]): Seq[PartialPipeline] = {
    componentTemplates.collect {
      case d: DataSourceTemplate => PartialPipeline(Seq(InternalComponent(d, ProgressReporter.props).componentInstance), Seq())
    }
  }

}
