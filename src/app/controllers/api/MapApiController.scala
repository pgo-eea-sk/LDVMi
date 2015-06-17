package controllers.api

import model.entity.PipelineEvaluation
import model.rdf.sparql.geo.MapQueryData
import play.api.db.slick.Action.async
import play.api.libs.json._
import play.api.mvc.Result
import scaldi.Injector
import JsonImplicits._
import play.api.db.slick._


class MapApiController(implicit inj: Injector) extends ApiController {

  def polygonEntities(id: Long) = simpleParsingFuture(id) { (pipelineEvaluation, queryData: MapQueryData, json) =>
    geoService.polygonEntities(pipelineEvaluation, queryData)
  } { json => json.validate[MapQueryData] } { entities => Json.toJson(entities) }

  def polygonEntitiesProperties(id: Long) = simpleFuture(id) { pipelineEvaluation =>
    geoService.polygonEntitiesProperties(pipelineEvaluation)
  } { entities => Json.toJson(entities) }

  def markers(id: Long) = Action.async(parse.json(1024 * 1024 * 100)) { implicit rs =>
    val json: JsValue = rs.request.body

    withEvaluation(id, json) { case (evaluation, query) =>
      Ok(Json.toJson(geoService.markers(evaluation, query)))
    }
  }

  def properties(id: Long) = Action.async { implicit rs =>
    withEvaluation(id) { evaluation =>

      val json = geoService.properties(evaluation).map(_.map { case (uri, maybeLabel) =>
        JsObject(Seq(
          "uri" -> JsString(uri),
          "label" -> maybeLabel.map(JsString).getOrElse(JsNull)
        ))
      })

      Ok(Json.toJson(json))
    }
  }



  private def withEvaluation(id: Long, json: JsValue)
    (func: (PipelineEvaluation, MapQueryData) => Result)
    (implicit rs: play.api.db.slick.Config.driver.simple.Session): Result = {

    json.validate[MapQueryData] match {
      case s: JsSuccess[MapQueryData] =>

        withEvaluation(id) { evaluation =>
          func(evaluation, s.get)
        }

      case e: JsError => {
        println(e)
        UnprocessableEntity
      }
    }

  }

}