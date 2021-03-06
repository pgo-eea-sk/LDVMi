package model.service

import java.util.UUID

import model.entity.{DataSourceTemplateTable, DataSourceTemplate, DataSourceTemplateId}
import model.repository.DataSourceTemplateRepository
import play.api.db.slick.Session
import play.api.libs.Files
import play.api.mvc.MultipartFormData

trait DataSourceService extends CrudService[DataSourceTemplateId, DataSourceTemplate, DataSourceTemplateTable, DataSourceTemplateRepository]{

  def createDataSourceFromRemoteTtl(uris: Seq[String])(implicit session: Session): Option[DataSourceTemplateId]

  def createDataSourceFromFiles(files: Seq[MultipartFormData.FilePart[Files.TemporaryFile]], maybeUrn: Option[UUID] = None)(implicit session: Session): Option[DataSourceTemplateId]

  def createDataSourceFromUris(endpointUrl: String, graphUris: Seq[String])(implicit session: Session): Option[DataSourceTemplateId]

}
