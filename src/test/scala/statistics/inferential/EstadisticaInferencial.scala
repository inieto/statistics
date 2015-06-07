package statistics.inferential

/**
 * Created by inieto on 19/05/15.
 */
import org.scalatest.{FunSuite, Matchers}
import statistics.descriptive.Stats
import statistics.ImplicitConversions._

class EstadisticaInferencial extends FunSuite with Matchers {

   //Medidas de Posición
   test("Calculate Average Ῡ") {
     val values = List(1,2,3,4,5)
     val Ῡ = Stats.average(values)
     Ῡ should be(3)
   }

   val p = 1/6D //probabilidad de cada cara del dado
   val dice = Map(1->p, 2->p, 3->p, 4->p, 5->p, 6->p)  //todas las caras con su probabilidad
   test("Calculate Expected Value 'm'") {
     val m = Stats.expectation(dice)
     m should be(3.5)
   }

   val m = 3.5D
   test("Calculate Variance σ²") {
     val `σ²` = Stats.variance(dice)
     `σ²` should be(2.916666666666666)
   }

   test("Calculate StandardDeviation σ") {
     val σ = Stats.standardDeviation(dice)
     σ should be(1.707825127659933)
   }
 }
