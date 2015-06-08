package statistics.descriptive

/**
 * Created by inieto on 06/06/15.
 */

import statistics.ImplicitOperators

import scala.math.abs

object Stats {

  import ImplicitOperators._

  object Metrics {

    /** Estadistica Descriptiva */

    // Medidas de Posición

    /* Promedio (Ῡ)*/
    def average(values: List[Double]): Double = {
      values.sum / values.size
    }

    /* Esperanza Matemática (m)*/
    def expectation(values: Map[Double,Double]): Double = {
      values.map {  //'y' valor, 'p' probabilidad de ocurrencia
        case (y, p) => y * p
      }.sum
    }

    /* Varianza (σ²)*/
    def variance(values: Map[Double, Double]) = {
      val m = expectation(values)
      values.map {  //'y' valor, 'p' probabilidad de ocurrencia
        case (y, p) => ((y - m) ^ 2) * p
      }.sum
    }

    /* Desvio Estandar (σ)*/
    def standardDeviation(values: Map[Double, Double]) = {
      √(variance(values))
    }

    /* Coeficiente de Variación (CV = σ/m)*/
    def variationCoefficient(values: Map[Double, Double]) = {
      abs(standardDeviation(values) / expectation(values))
    }

    /* Esperanza Matemática para Distribución Binomial (E(p))*/
    def binomialExpectation(p: Double, prize: Double, bet: Double): Double = {
      p * prize - (1 - p) * bet
    }
  }
  /** Estimadores puntuales */
  object Estimators {
    /* Estimador Puntual del Promedio Poblacional (Ῡ) */
    def Ῡ(values: List[Double]): Double = Metrics.average(values)

    /* Estimador Puntual de la Varianza Poblacional (S²) */
    def `S²`(values: List[Double]): Double = {
      val Ῡ = Estimators.Ῡ(values)
      val n = values.size
      values.map(y => (y - Ῡ) ^ 2).sum / (n-1)
    }
  }
  // Medidas de Posición

  // Series de Frecuencia
  //  def expectation(values: List[Double]): Double = {
  //    //Primero cada número con su cantidad de ocurrencias
  //    val frecuencies = values groupBy(Double => Double) mapValues(_.size)
  //    frecuencies.map {
  //      case (x, f) => x * f
  //    }.reduceLeft(_ + _) / values.size
  //  }

}
