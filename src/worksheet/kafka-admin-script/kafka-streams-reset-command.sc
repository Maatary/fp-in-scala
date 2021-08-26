import cats.syntax.all._
import cats.effect.unsafe.implicits.global
import cats.effect.IO

/*val topics = """elsevier-raw-resnet-annotations
  |elsevier-raw-resnet-controlprops
  |elsevier-raw-resnet-controls
  |elsevier-raw-resnet-dict
  |elsevier-raw-resnet-dictcontrolprops
  |elsevier-raw-resnet-dictnodeprops
  |elsevier-raw-resnet-intcontrolprops
  |elsevier-raw-resnet-intnodeprops
  |elsevier-raw-resnet-links
  |elsevier-raw-resnet-membership
  |elsevier-raw-resnet-memocontrolprops
  |elsevier-raw-resnet-memonodeprops
  |elsevier-raw-resnet-nodeprops
  |elsevier-raw-resnet-nodes
  |elsevier-raw-resnet-numnodeprops
  |elsevier-raw-resnet-objecttypes
  |elsevier-raw-resnet-proptypes
  |elsevier-raw-resnet-strcontrolprops
  |elsevier-raw-resnet-strnodeprops""".stripMargin*/

val topics = """elsevier-dup-tmp-resnet-controlprops-with-types
               |elsevier-dup-tmp-resnet-controls-with-types-with-memberships
               |elsevier-dup-tmp-resnet-dictcontrolprops-by-sources
               |elsevier-dup-tmp-resnet-dictcontrolprops-with-types-with-idobjects
               |elsevier-dup-tmp-resnet-intcontrolprops-by-sources
               |elsevier-dup-tmp-resnet-intcontrolprops-with-types-with-idobjects
               |elsevier-dup-tmp-resnet-memocontrolprops-by-sources
               |elsevier-dup-tmp-resnet-memocontrolprops-with-types-with-idobjects
               |elsevier-dup-tmp-resnet-strcontrolprops-by-sources
               |elsevier-dup-tmp-resnet-strcontrolprops-with-types-with-idobjects""".stripMargin


 def buildCommand = (e:String) =>
   s"kafka-topics  --bootstrap-server localhost:9092 --delete  --topic $e  --force"

val resetCommand = for {

  topics <- IO.pure(topics)

  topicsArgs = topics.split("\n").mkString("\"", ",", "\"")

  } yield buildCommand(topicsArgs)

resetCommand.unsafeRunSync()