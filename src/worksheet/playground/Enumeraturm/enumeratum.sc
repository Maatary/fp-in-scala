
import enumeratum._


object ClassicADT {

  sealed trait Weekday extends EnumEntry
  object Weekday extends Enum[Weekday] {
    val values = findValues // mandatory due to Enum extension
    case object Monday extends Weekday
    case object Tuesday extends Weekday
    case object Wednesday extends Weekday
    case object Thursday extends Weekday
    case object Friday extends Weekday
    case object Saturday extends Weekday
    case object Sunday extends Weekday
  }
}

ClassicADT.Weekday.withName("Monday")

object ClassicADTWithSealedAbstractClass {

  sealed abstract class Weekday( val name: String,
                                 val abbreviation: String,
                                 val isWorkDay: Boolean) extends EnumEntry
  case object Weekday extends Enum[Weekday] {
    val values = findValues // mandatory due to Enum extension
    case object Monday extends Weekday("Monday", "Mo.", true)
    case object Tuesday extends Weekday("Tuesday", "Tu.", true)
    case object Wednesday extends Weekday("Wednesday", "We.", true)
    case object Thursday extends Weekday("Thursday", "Th.", true)
    case object Friday extends Weekday("Friday", "Fr.", true)
    case object Saturday extends Weekday("Saturday", "Sa.", false)
    case object Sunday extends Weekday("Sunday", "Su.", false)
  }

}

ClassicADTWithSealedAbstractClass.Weekday.withNameOption("Monday")