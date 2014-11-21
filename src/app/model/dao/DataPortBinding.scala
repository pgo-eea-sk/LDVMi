package model.dao

import org.joda.time.DateTime
import org.virtuslab.unicorn.LongUnicornPlay._
import org.virtuslab.unicorn.LongUnicornPlay.driver.simple._

case class DataPortBindingId(id: Long) extends AnyVal with BaseId

object DataPortBindingId extends IdCompanion[DataPortBindingId]

case class DataPortBinding(
  id: Option[DataPortBindingId],
  startId: DataPortId,
  endId: InputId,
  var createdUtc: Option[DateTime] = None,
  var modifiedUtc: Option[DateTime] = None
  ) extends IdEntity


class DataPortBindingTable(tag: Tag) extends IdEntityTable[DataPortBindingId, DataPortBinding](tag, "dataport_bindings") {

  val dataPorts = TableQuery[DataPortTable]
  val inputs = TableQuery[InputTable]

  def start = foreignKey("fk_dpbt_dpt_id", startPortId, dataPorts)(_.id)

  def startPortId = column[DataPortId]("start_port_id", O.NotNull)

  def end = foreignKey("fk_dpbt_dpt_id", endInputId, inputs)(_.id)

  def endInputId = column[DataPortId]("end_input_id", O.NotNull)

  def * = (id, startPortId, endInputId, createdUtc, modifiedUtc) <>(DataPortBinding.tupled, DataPortBinding.unapply _)
}