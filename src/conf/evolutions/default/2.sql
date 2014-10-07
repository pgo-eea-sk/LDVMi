# --- !Ups

create table "VISUALIZERS" ("ID" BIGINT GENERATED BY DEFAULT AS IDENTITY(START WITH 1) NOT NULL PRIMARY KEY,"NAME" VARCHAR NOT NULL,"INPUT_SIGNATURE" VARCHAR NOT NULL,"CREATED" TIMESTAMP,"MODIFIED" TIMESTAMP);
create table "VISUALIZERS_COMPATIBILITY" ("ID" BIGINT GENERATED BY DEFAULT AS IDENTITY(START WITH 1) NOT NULL PRIMARY KEY,"VISUALIZATION_ID" BIGINT NOT NULL,"DATASOURCE_ID" BIGINT NOT NULL,"CREATED" TIMESTAMP,"MODIFIED" TIMESTAMP);
alter table "VISUALIZERS_COMPATIBILITY" add constraint "VC_DATA_SOURCE_FK" foreign key("DATASOURCE_ID") references "DATASOURCES"("ID") on update NO ACTION on delete NO ACTION;
alter table "VISUALIZERS_COMPATIBILITY" add constraint "VC_VISUALIZER_FK" foreign key("VISUALIZATION_ID") references "VISUALIZATIONS"("ID") on update NO ACTION on delete NO ACTION;


# --- !Downs

alter table "VISUALIZERS_COMPATIBILITY" drop constraint "VC_DATA_SOURCE_FK";
alter table "VISUALIZERS_COMPATIBILITY" drop constraint "VC_VISUALIZER_FK";
drop table "VISUALIZERS_COMPATIBILITY";
drop table "VISUALIZERS";