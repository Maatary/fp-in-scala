package circe


/**
 * Revisit issue with empty value
 */
object TgCirceApp extends App {

  import io.circe.syntax._
  import TgDataTypes._


  /*SingleValuedAttribute("dateCreated", "10-16-2021", STRING).asJson*/
  /*MultiValuedAttribute("medScanId", List("medId2", "MedId4"), STRING).asJson*/


  val v1 =
    Vertex(
      "Resnet_Protein",
      "protein_1",
      List(
        SingleValuedAttribute("dateCreated", "10-16-2021", STRING),
        SingleValuedAttribute("dateModified", "10-05-2022", STRING),
        MultiValuedAttribute("medScanId", List("medId2", "MedId4"), STRING)
      )
    )

  val v2 =
    Vertex(
      "Resnet_Protein",
      "protein_2",
      List(
        SingleValuedAttribute("dateCreated", "10-18-2021", STRING),
        SingleValuedAttribute("dateModified", "10-05-2022", STRING),
        MultiValuedAttribute("keggId", List("keggId6", "keggId7"), STRING)
      )
    )



  /*println(v1.asJson(encodeNestedVertex).spaces2)*/
  /*println(v2.asJson(encodeNestedVertex).spaces2)*/



  val edgeAttributes1: List[TgAttribute] = List(SingleValuedAttribute("dateCreated", "10-18-2021", STRING), SingleValuedAttribute("effect", "positive", STRING), SingleValuedAttribute("mechanism", "direct interaction", STRING))
  val edgeAttributes2: List[TgAttribute] = List(SingleValuedAttribute("dateCreated", "10-16-2021", STRING), SingleValuedAttribute("effect", "negative", STRING), SingleValuedAttribute("mechanism", "cleavage", STRING))

  val e1: Edge                           = Edge("Resnet_Regulation", "Resnet_SmallMol_72057594038010064", "Resnet_SmallMol","Resnet_ClinicalParameter_72057594038000019","Resnet_ClinicalParameter", edgeAttributes1)
  val e2: Edge                           = Edge("Resnet_Regulation", "Resnet_SmallMol_72057594038010064", "Resnet_SmallMol","Resnet_ClinicalParameter_72057594038000018","Resnet_ClinicalParameter", edgeAttributes2)

  //println(e1.asJson.spaces2)

  val tgMsg1 = TgMessage(List(v1, v2), List(e1,e2))

  println(tgMsg1.asJson.spaces2)

}
