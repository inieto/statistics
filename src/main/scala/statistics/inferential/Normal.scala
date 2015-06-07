package statistics.inferential

/**
 * Created by inieto on 06/06/15.
 */

import statistics.ImplicitOperators

import scala.math.abs

object Normal {

   import ImplicitOperators._

   /** Estadistica Descriptiva */

   // Medidas de Posición

   /* Promedio (Ῡ)*/
   def average(values: List[Double]): Double = {
     values.reduceLeft(_ + _) / values.size
   }

   /* Esperanza Matemática (E)*/
   def expectation(values: Map[Double,Double]): Double = {
     values.map {
       case (y, p) => y * p
     }.reduceLeft(_ + _)
   }

   /* Varianza (σ²)*/
   def variance(values: Map[Double, Double]) = {
     val m = expectation(values)
     values.map {
       case (y, p) => ((y - m) ^ 2) * p
     }.reduceLeft(_ + _)
   }

   /* Desvio Estandar (σ)*/
   def standardDeviation(values: Map[Double, Double]) = {
     √(variance(values))
   }

   /* Coeficiente de Variación (CV = σ/m)*/
   def variationCoefficient(values: Map[Double, Double]) = {
     abs(standardDeviation(values) / expectation(values))
   }

   // Series de Frecuencia
   //  def expectation(values: List[Double]): Double = {
   //    //Primero cada número con su cantidad de ocurrencias
   //    val frecuencies = values groupBy(Double => Double) mapValues(_.size)
   //    frecuencies.map {
   //      case (x, f) => x * f
   //    }.reduceLeft(_ + _) / values.size
   //  }

   /** Estadistica Inferencial (Variables Aleatorias) */

   /** Variable Normal */

 }
