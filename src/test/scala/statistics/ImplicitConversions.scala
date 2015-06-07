package statistics

/**
 * Created by inieto on 07/06/15.
 */
object ImplicitConversions {
  //Auxiliar conversor implícito de Int a Double
    implicit def intListToDoubleList(l: List[Int]): List[Double] =
      l.map(_.toDouble)

    implicit def intMapToDoubleMap(m: Map[Int,Double]): Map[Double,Double] =
      m.map {case(int:Int, double:Double) => (int.toDouble,double)}
}
