package controllers.api

import controllers.api.JsonImplicits._
import model.entity.{ComponentTemplate, ComponentTemplateId}
import play.api.libs.json._
import play.api.mvc.Result
import scaldi.{Injectable, Injector}

class ComponentTemplateApiController(implicit inj: Injector) extends ApiController with Injectable {

  def delete(componentTemplateId: Long) = Action.async { implicit rws =>
    withComponentTemplate(componentTemplateId) { ct =>
      componentTemplateService.delete(ct)
      Ok
    }
  }

  def list(skip: Int = 0, take: Int = 50) = Action.async { implicit rws =>

    val componentTemplates = componentTemplateService.findPaginated(skip, take)()
    val ids = componentTemplates.map(_.id.get)

    val specificTemplates = componentTemplateService.findSpecificIn(ids)

    val specific = componentTemplates.map { ct =>
      (specificTemplates.find(_.componentTemplateId == ct.id.get), ct)
    }

    val result = JsObject(Seq(
      "data" -> Json.toJson(specific),
      "count" -> JsNumber(componentTemplateService.countAll)
    ))

    Ok(result)
  }

  def findById(id: Long) = Action.async { implicit rws =>
    withComponentTemplate(id) { componentTemplate =>
      Ok(Json.toJson(componentTemplate))
    }
  }

  def featuresById(id: Long) = Action.async { implicit rws =>
    withComponentTemplate(id) { componentTemplate =>
      Ok(Json.toJson(componentTemplate.features))
    }
  }

  def inputsById(id: Long) = Action.async { implicit rws =>
    withComponentTemplate(id) { componentTemplate =>
      Ok(Json.toJson(componentTemplate.inputTemplates.map(_.dataPortTemplate)))
    }
  }

  def outputById(id: Long) = Action.async { implicit rws =>
    withComponentTemplate(id) { componentTemplate =>
      Ok(Json.toJson(componentTemplate.outputTemplate.map(_.dataPortTemplate)))
    }
  }

  def descriptorsById(id: Long) = Action.async { implicit rws =>
    withComponentTemplate(id) { componentTemplate =>
      Ok(Json.toJson(componentTemplate.features.flatMap(_.descriptors)))
    }
  }

  def addDatasource = Action.async { implicit rws =>
    rws.request.body.asJson.flatMap { json =>
      val endpointUrl = (json \ "endpointUrl").as[String]
      val graphUris = (json \ "graphUris").as[Seq[String]]

      val maybeId = dataSourceService.createDataSourceFromUris(endpointUrl, graphUris)
      maybeId.map { id =>
        Ok(JsObject(Seq(
          "id" -> JsNumber(id.id)
        )))
      }
    }.getOrElse{
      BadRequest
    }
  }

  private def withComponentTemplate(id: Long)(func: ComponentTemplate => Result)(implicit session: Session): Result = {
    componentTemplateService.findById(ComponentTemplateId(id)).map(func)
  }.getOrElse {
    NotFound
  }
}