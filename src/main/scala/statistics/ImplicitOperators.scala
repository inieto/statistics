package statistics

/**
 * Created by inieto on 07/06/15.
 */
import scala.math.{pow, sqrt}

object ImplicitOperators {

  class MyRichDouble(d: Double) {
    def ^ (exp:Double) :Double = pow(d,exp)
  }

  implicit def doubleToSyntax(d: Double) = new MyRichDouble(d)
  def √(d: Double) : Double = sqrt(d)

}
