package model.service.impl.pipeline

import akka.actor.Props
import model.entity._
import model.service.Connected
import model.service.component.{ResultRequest, ControlActor, InternalComponent, Run}
import play.api.Play.current
import play.api.db.slick._
import play.api.libs.concurrent.Akka
import scaldi.{Injector, Injectable}


class PipelineEvaluationAlgorithm(evaluation: PipelineEvaluation, reporterProps: Props)
  (implicit val session: Session, inj: Injector) extends Connected with Injectable {

  val logger = Akka.system.actorOf(reporterProps)
  logger ! evaluation.uuid.toString

  def run(bindingSet: DataPortBindingSet) {

    val recursivePipelineData = recursivePipeline(bindingSet, 0)

    val instancesById = recursivePipelineData._2.toMap
    val bindings = recursivePipelineData._1

    instancesById.map(i => println(i._1))

    bindings.map { binding =>
      val sourceInstanceId = binding.source.componentInstanceId
      val targetInstanceId = binding.target.componentInstanceId

      instancesById.get(targetInstanceId).map { case (targetInstance, targetHasOutput, _) =>
        instancesById.get(sourceInstanceId).map { case (sourceInstance, sourceHasOutput, _) =>
          targetInstance.requestDataFrom(sourceInstance, binding.targetId)
        }
      }
    }

    val visualizer = instancesById.find{ case (_, (c, hasOutput, _)) => !hasOutput }.get
    val visualizerInstance = visualizer._2._1.componentInstance
    val controlActor = Akka.system.actorOf(Props(new ControlActor(evaluation, reporterProps, visualizerInstance)))

    instancesById.foreach {
      case (_, (c, false, _)) => c.actor.tell(ResultRequest(), controlActor)
      case (_, (c, _, false)) => c.actor.tell(Run(), controlActor)
      case _ =>
    }
  }

  private def recursivePipeline(bindingSet: DataPortBindingSet, level: Integer): (Seq[DataPortBinding], Seq[(ComponentInstanceId, (InternalComponent, Boolean, Boolean))]) = {

    if (level > 100) {
      (Seq(), Seq())
    } else {

      val componentInstances = bindingSet.componentInstances

      val recursiveData = componentInstances.map { componentInstance =>

        val componentTemplate = componentInstance.componentTemplate
        val maybeNestedBindingSet = componentTemplate.nestedBindingSet

        maybeNestedBindingSet.map { nbs =>
          val nested = recursivePipeline(nbs, level + 1)

          val fixedBindings = nbs.nestedBindings.map { nestedBinding =>
            val sourcesId = nestedBinding.sourceInstance.map(i => Seq(i.id.get)).getOrElse {
              val portTemplateUri = nestedBinding.sourceTemplate.get.uri
              val portInstance = componentInstance.inputInstances.map(_.dataPortInstance).find(_.dataPortTemplate.uri == portTemplateUri)
              bindingSet.bindings.filter(_.targetId == portInstance.get.id.get).map(_.sourceId)
            }

            val targetIds = nestedBinding.targetInstance.map(i => Seq(i.id.get)).getOrElse {
              val nestedOutputPortInstanceId = componentInstance.outputInstance.get.dataPortInstance.id.get
              bindingSet.bindings.filter(_.sourceId == nestedOutputPortInstanceId).map(_.targetId)
            }

            sourcesId.flatMap { si =>
              targetIds.map { ti =>
                DataPortBinding(None, bindingSet.id.get, si, ti)
              }
            }
          }

          (nested._1 ++ fixedBindings.flatten, nested._2)
        }.getOrElse {

          (
            Seq(),
            Seq((
              componentInstance.id.get,
              (InternalComponent(componentInstance, reporterProps), componentInstance.hasOutput, componentInstance.hasInput)
              ))
            )

        }
      }

      (
        bindingSet.bindings ++ recursiveData.flatMap(_._1),
        recursiveData.flatMap(_._2)
        )
    }
  }

}