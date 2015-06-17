package controllers.api

import akka.actor._
import controllers.api.JsonImplicits._
import controllers.api.ProgressReporter._
import model.entity.{ComponentTemplateId, PipelineDiscoveryId, Pipeline, PipelineId}
import model.service.PipelineService
import play.api.db
import play.api.db.slick._
import play.api.libs.json._
import play.api.mvc.{WebSocket, Controller}
import scaldi.{Injectable, Injector}
import play.api.Play.current
import utils.PaginationInfo

class PipelineApiController(implicit inj: Injector) extends Controller with Injectable {

  val pipelineService = inject[PipelineService]

  def findById(id: Long) = Action.async { implicit rws =>
    pipelineService.findById(PipelineId(id)).map { pipeline =>
      Ok(Json.toJson(pipeline))
    }.getOrElse {
      NotFound
    }
  }

  def visualizationById(id: Long) = Action.async { implicit rws =>
    pipelineService.findById(PipelineId(id)).map { pipeline =>
      Ok(pipelineToJson(pipeline))
    }.getOrElse {
      NotFound
    }
  }

  def evaluations(id: Long, skip: Int = 0, take: Int = 50) = Action.async { implicit rws =>

    val pipelineId = PipelineId(id)

    val result = JsObject(Seq(
      "data" -> Json.toJson(pipelineService.lastEvaluations(pipelineId, PaginationInfo(skip, take)))
    ))

    Ok(result)
  }

  def list(skip: Int = 0, take: Int = 50, discoveryId: Option[Long] = None, visualizerId: Option[Long] = None) = Action.async { implicit rws =>

    val pipelineDiscoveryId = discoveryId.map(PipelineDiscoveryId.apply)
    val visualizerTemplateId = visualizerId.map(ComponentTemplateId.apply)

    val result = JsObject(Seq(
      "data" -> Json.toJson(pipelineService.findPaginatedFiltered(PaginationInfo(skip, take), pipelineDiscoveryId, visualizerTemplateId)()),
      "count" -> JsNumber(pipelineService.countAll)
    ))

    Ok(result)
  }

  def discover(dataSourceTemplateId: Option[Long], combine: Boolean = false) =  withWebSocket { logger => implicit session =>
    pipelineService.discover(logger, dataSourceTemplateId, combine)
  }

  def evaluate(id: Long) = withWebSocket { logger => implicit session =>
    pipelineService.evaluate(PipelineId(id))(logger)
  }

  def pipelineToJson(pipeline: Pipeline)(implicit session: Session) = {
    val set = pipeline.bindingSet

    val links = set.bindings.map { b =>
      val source = b.source.componentInstance
      val target = b.targetInputInstance.map(_.componentInstance)

      JsObject(Seq(
        "target" -> JsString(target.get.componentTemplate.title + " ["+target.get.id.map(_.id.toString).get+"]"),
        "source" -> JsString(source.componentTemplate.title + " ["+source.id.map(_.id.toString).get+"]"),
        "type" -> JsString("resolved")
      ))
    }

    JsArray(links)
  }
}