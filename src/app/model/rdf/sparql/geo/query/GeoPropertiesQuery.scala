package model.rdf.sparql.geo.query

import model.rdf.sparql.query.SparqlQuery


class GeoPropertiesQuery extends SparqlQuery {

  def get: String = prefixes +
    """
      | SELECT DISTINCT ?p ?l ?spl ?sn WHERE {
      |   ?s s:geo [] ;
      |      ?p ?o .
      |   ?o a skos:Concept .
      |
      |   OPTIONAL { ?p skos:prefLabel ?spl . }
      |   OPTIONAL { ?p rdfs:label ?l . }
      |   OPTIONAL { ?p skos:notion ?sn . }
      |
      |   FILTER(?p != skos:prefLabel)
      |   FILTER(?p != <http://schema.org/geo>)
      |   FILTER(?p != <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>)
      |   FILTER(?p != <http://www.w3.org/2000/01/rdf-schema#seeAlso>)
      |
      | } LIMIT 1000
    """.stripMargin

  private def prefixes =
    """
      | PREFIX s: <http://schema.org/>
      | PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      | PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      |
    """.stripMargin

}


object GeoPropertiesQuery {

  object NodeVariables extends Enumeration {
    type NodeVariables = Value
    val VALUE_PROPERTY_VARIABLE = Value("p")
  }

  object LabelVariables extends Enumeration {
    type LabelVariables = Value
    val VALUE_NOTION_VARIABLE = Value("sn")
    val VALUE_PREFLABEL_VARIABLE = Value("spl")
    val VALUE_LABEL_VARIABLE = Value("l")
  }

}