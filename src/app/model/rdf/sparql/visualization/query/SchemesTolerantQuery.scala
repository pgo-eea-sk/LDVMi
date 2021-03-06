package model.rdf.sparql.visualization.query

class SchemesTolerantQuery extends SchemesQuery {

  override def get: String =
    """
      | PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      |
      | CONSTRUCT {
      |   ?s a skos:ConceptScheme ;
      |      skos:prefLabel ?l .
      | }
      | WHERE {
      |   ?s a skos:ConceptScheme .
      |
      |   OPTIONAL {
      |     ?s skos:prefLabel ?l .
      |   }
      | }
    """.stripMargin
}
