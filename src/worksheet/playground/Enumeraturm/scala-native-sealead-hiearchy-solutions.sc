/**
 * Classic ADT with Object
 */
object solutionClassicADTWithSealedTrait {

  sealed trait Weekday
  case object Monday extends Weekday
  case object Tuesday extends Weekday
  case object Wednesday extends Weekday
  case object Thursday extends Weekday
  case object Friday extends Weekday
  case object Saturday extends Weekday
  case object Sunday extends Weekday

}


/**
 * Classic ADT but using SealedTrait
 */
object solutionClassicADTWithAbstractClass {

  sealed abstract class Weekday(val name: String,
                                val abbreviation: String,
                                val isWeekDay: Boolean)

  case object Monday extends Weekday("Monday", "Mo.", true)
  case object Tuesday extends Weekday("Tuesday", "Tu.", true)
  case object Wednesday extends Weekday("Wednesday", "We.", true)
  case object Thursday extends Weekday("Thursday", "Th.", true)
  case object Friday extends Weekday("Friday", "Fr.", true)
  case object Saturday extends Weekday("Saturday", "Sa.", false)
  case object Sunday extends Weekday("Sunday", "Su.", false)

}