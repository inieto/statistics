package statistics.inferential

/**
 * Created by inieto on 19/05/15.
 */
import org.scalatest.{FunSuite, Matchers}
import statistics.ImplicitOperators

class Ejercicios extends FunSuite with Matchers {

  import ImplicitOperators._
   /** Ejercicio 8
    * De la observación de la variable (X) que representa el comportamiento diario de las rentabilidades del
    * activo MOLINOS se puede concluir que ésta es asimilable a una función Normal del tipo N(0.2793,4.4636)
   */
   test("Ejercicio (8.a) Calcular la probabilidad de que la" +
        "rentabilidad supere el valor de la volatilidad del activo") {
     val m = 0.2793
     val `σ²` = 4.4636
     val limit = 4.4636  //"el valor de la volatilidad del activo"
     new Normal(m,`σ²`).Probability.forYGreaterThan(limit) should be(0.023849999999999982)
   }

   test("Ejercicio (8.b) Calcular la probabilidad de que la rentabilidad" +
        "asuma valores comprendidos en un intervalo del tipo [m±σ]") {
     val m = 0.2793
     val `σ²` = 4.4636
     val σ = √(4.4636)
     new Normal(m,`σ²`).Probability.forYBetween(m-σ,m+σ) should be(0.68268)
   }

   test("Ejercicio (8.c) Determinar la rentabilidad tal que se pueda asegurar" +
        "que existe una probabilidad del 90% de que X no supere dicho valor") {
     //"Hallar Z, tal que P(x<Z) = 0,9
     val m = 0.2793
     val `σ²` = 4.4636
     new Normal(m,`σ²`).Z.greaterThanYWithGivenP(0.9) should be (2.9835859020451223)
   }

 }
