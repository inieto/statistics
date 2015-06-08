package statistics.descriptive

/**
 * Created by inieto on 19/05/15.
 */
import org.scalatest.{FunSuite, Matchers}
import statistics.ImplicitConversions
import ImplicitConversions._

class Ejercicios extends FunSuite with Matchers {

  /** Ejercicio 1
   * De acuerdo con los registros históricos de una compañía de seguros, la variable aleatoria
   * que representa el número de accidentes de tránsito en la ciudad (X), se distribuye así:
   * X    = (  0,   1,    2,    3,    4,    5)
   * P(x) = (0.1, 0.2, 0.45, 0.15, 0.05, 0.05)
   * a) Construir la función de distribución -> ??? ¿regresión lineal?
   * c) Calcular la probabilidad de que, en un día cualquiera, el número de accidenes supere
   *    su valor esperado.
  */
  val x = Map(0->0.1, 1->0.2, 2->0.45, 3->0.15, 4->0.05, 5->0.05)

  test("Ejercicio (1.b) Calcular el número esperado de accidentes diarios") {
    val m = Stats.Metrics.expectation(x)
    m should be (2)
  }

  test("Ejercicio (1.d) Calcular el desvío estándar de la variable aleatoria X") {
    val σ = Stats.Metrics.standardDeviation(x)
    σ should be(1.1832159566199232)
  }

  /** Ejercicio 2
   * A partir de datos históricos, en una compañía de redes de cómputos, se desarrolló la
   * siguiente distribución de probabilidades de la variable aleatoria (X) que
   * representa el número de interrupciones del servicio por día:
   * X    = (   0,    1,    2,    3,    4,    5,    6)
   * P(x) = (0.32, 0.35, 0.18, 0.08, 0.04, 0.02, 0.01)
   */
  test("Ejercicio (2.a) Calcular el número esperado de interrupciones" +
       "Ejercicio (2.b) Calcular el desvío medio cuadrático y el desvío estandar") {

    val x = Map(0->0.32, 1->0.35, 2->0.18, 3->0.08, 4->0.04, 5->0.02, 6->0.01)

    val m = Stats.Metrics.expectation(x)
    m should be (1.2699999999999998)

    val `σ²` = Stats.Metrics.variance(x)
    `σ²` should be(1.6771000000000003)

    val σ = Stats.Metrics.standardDeviation(x)
    σ should be(1.295028957205205)
  }

  /** Ejercicio 3
    * Supóngase 3 alternativas de inversión, A, B y C, tales que las rentabilidades esperadas
    * de cada una de ellas, según tres posibles situaciones económicas figuran en el siguiente cuadro:
    *                          ___Rentabilidades_____
    * ________________________|__A___|__B___|___C_._|
    * Economía en declinación |  500 |-2000 | -7000 |
    * Economía estable        | 1000 | 2000 | -1000 |
    * Economía en expansión   | 2000 | 5000 | 20000 |
    * -----------------------------------------------
    * Del análisis de las variables macroeconómicas se puede concluir que:
    * P(declinación) = 0.7 -- P(estabilidad) = 0.2 -- P(crecimiento) = 0.1
    */
  val A = Map(500->0.7, 1000->0.2, 2000->0.1)
  val B = Map(-2000->0.7, 2000->0.2, 5000->0.1)
  val C = Map(-7000->0.7, -1000->0.2, 20000->0.1)

  test("Ejercicio (3.a) Decidir la elección de la alternativa de inversión," +
       "de acuedo con el criterio del valor esperado") {
    val mA = Stats.Metrics.expectation(A)
    val mB = Stats.Metrics.expectation(B)
    val mC = Stats.Metrics.expectation(C)
    mA should be (750)
    mB should be (-500)
    mC should be (-3100)
    //Elección de alternativa por criterio de maximización de Rentabilidad
    List(mA,mB,mC).max should be (mA)
  }

  test("Ejercicio (3.b) Calcular el desvío estándar de las variables que " +
       "representan cada una de las alternativas") {
    val σA = Stats.Metrics.standardDeviation(A)
    val σB = Stats.Metrics.standardDeviation(B)
    val σC = Stats.Metrics.standardDeviation(C)
    σA should be(460.9772228646444)
    σB should be(2418.677324489565)
    σC should be(8055.432949258531)
  }

  test("Ejercicio (3.c) Calcular los correspondientes coeficientes de variación") {
    val cvA = Stats.Metrics.variationCoefficient(A)
    val cvB = Stats.Metrics.variationCoefficient(B)
    val cvC = Stats.Metrics.variationCoefficient(C)
    cvA should be(0.6146362971528592)
    cvB should be(4.8373546489791295)
    cvC should be(2.598526757825333)
  }
}
