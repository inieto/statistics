package statistics.descriptive

/**
 * Created by inieto on 19/05/15.
 */
import org.scalatest.{FunSuite, Matchers}
import statistics.ImplicitConversions
import ImplicitConversions._

class EstadisticaDescriptiva extends FunSuite with Matchers {

  //Medidas de Posición
  test("Calculate Average Ῡ") {
    val values = List(1,2,3,4,5)
    val Ῡ = Stats.Metrics.average(values)
    Ῡ should be(3)
  }

  val p = 1/6D //probabilidad de cada cara del dado
  val dice = Map(1->p, 2->p, 3->p, 4->p, 5->p, 6->p)  //todas las caras con su probabilidad
  test("Calculate Expected Value 'm'") {
    val m = Stats.Metrics.expectation(dice)
    m should be(3.5)
  }

  val m = 3.5D
  test("Calculate Variance σ²") {
    val `σ²` = Stats.Metrics.variance(dice)
    `σ²` should be(2.916666666666666)
  }

  test("Calculate StandardDeviation σ") {
    val σ = Stats.Metrics.standardDeviation(dice)
    σ should be(1.707825127659933)
  }
}
