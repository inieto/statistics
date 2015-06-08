package statistics.inferential

/**
 * Created by inieto on 19/05/15.
 */
import org.scalatest.{FunSuite, Matchers}
import statistics.ImplicitOperators
import statistics.descriptive.Stats

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

   /** Ejercicio 9
    * De una población formada por infinitas variables aleatorias independientes entre sí,
    * todas con distribución de probabilidades N(m,1) se ha obtenido la siguiente muestra:
    * 2,3,1,1,1,2,2,1,2,1
   */
   test("Ejercicio (9.a) Calcular el estimador puntual del parámetro m") {
     Stats.Estimators.Ῡ(List(2,3,1,1,1,2,2,1,2,1)) should be (1.6)
   }

   test("Ejercicio (9.b) Construir un intervalo de confiabilidad para" +
        "el valor medio poblacional con un nivel de significación del 5%") {
     new Normal(0,1). //0 no importa, pero σ² sí porque es conocida, sino habría que usar t de Student
       confidenceInterval(
         List(2,3,1,1,1,2,2,1,2,1), 0.05
       ) should be (0.9801935786069977, 2.2198064213930024)
   }

   /** Ejercicio 10
    * De una población formada por infinitas variables aleatorias independientes entre sí,
    * todas con distribución de probabilidades N(m,σ²) se ha obtenido la siguiente muestra:
    * 1,1,3,3,3,4,2,2,1,1
    */
   test("Ejercicio (10.a) Calcular las esimaciones puntuales de los valores poblacionales m y σ²") {

   }

   test("Ejercicio (10.b) Construir un intervalo de confiabilidad para el valor medio poblacional," +
        "con un nivel de significatividad del 10%") {
     Student.confidenceInterval(
       List(1,1,3,3,3,4,2,2,1,1), 0.10
     ) should be (1.4620972723055656, 2.7379027276944345)
   }

 }
