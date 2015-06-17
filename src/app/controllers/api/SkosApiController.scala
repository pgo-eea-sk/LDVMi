package controllers.api

import model.entity.PipelineEvaluation
import play.api.db.slick.Action.async
import play.api.db.slick._
import play.api.libs.json.{JsError, JsSuccess, Json, JsValue}
import play.api.mvc.{Action, Result}
import scaldi.Injector
import JsonImplicits._

class SkosApiController(implicit inj: Injector) extends ApiController {

  def schemes(id: Long) = Action.async { implicit rs =>
    withEvaluation(id) { evaluation =>
      Ok(Json.toJson(visualizationService.skosSchemes(evaluation)))
    }
  }

  def concepts(id: Long): Action[JsValue] = Action.async(parse.json(1024 * 1024 * 100)) { implicit rs =>
    val json: JsValue = rs.request.body

    withEvaluation(id, json) { case (evaluation, uris) =>
      Ok(Json.toJson(visualizationService.skosConcepts(evaluation, uris)))
    }
  }

  def scheme(id: Long, schemeUri: String) = Action.async { implicit rs =>
    withEvaluation(id) { evaluation =>
      Ok(Json.toJson(visualizationService.skosScheme(evaluation, schemeUri)))
    }
  }

  def conceptsCounts(id: Long): Action[JsValue] = Action.async(parse.json(1024 * 1024 * 100)) { implicit rs =>
    val json: JsValue = rs.request.body

    withEvaluation(id) { evaluation =>
      Ok(Json.toJson(visualizationService.skosConceptsCounts(
        evaluation,
        (json \ "propertyUri").as[String],
        (json \ "conceptUris").as[Seq[String]])
      ))
    }
  }

  private def withEvaluation(id: Long, json: JsValue)
    (func: (PipelineEvaluation, Seq[String]) => Result)
    (implicit rs: Session): Result = {

    (json \ "conceptUris").validate[Seq[String]] match {
      case s: JsSuccess[Seq[String]] =>

        withEvaluation(id) { evaluation =>
          func(evaluation, s.get)
        }

      case e: JsError => {
        UnprocessableEntity
      }
    }
  }

}
