package regression



import com.cra.figaro.algorithm.learning.{EMWithBP, EMWithVE}
import com.cra.figaro.algorithm.sampling.{Importance, MetropolisHastings}
import com.cra.figaro.language.Apply
import com.cra.figaro.library.atomic.continuous._

object Regression extends App {
  /**
    * Generate Data for Linear Regression, according to the model:
    * Y = alpha * x + beta (+ gaussian noise)
    * @param alpha coefficient of predictor x
    * @param beta  constant term
    * @param sigma standard deviation for gaussian noise (mean is 0)
    */
  def dataset(alpha: Double, beta: Double,
              sigma: Double, size: Int) : Seq[(Double, Double)] = {
    val x = Normal(0, 1)
    val noise = Normal(0, sigma)
    for (_ <- 1 to size) yield {
      x.generate()
      noise.generate()
      (x.value, alpha * x.value + beta + noise.value)
    }
  }

  val data = dataset(22.33, 0, 0.1, 10)

  val alpha = Beta(20, 5)
//  val beta = Normal(0, 25)
//  val sigma = Dirichlet(2, 2)

//  val importance = Importance(100, alpha)
//  importance.start()
  println("Started Importance Sampling")

  val trials = for ((x, y) <- data) yield {
    val xv = Beta(0, 1)
    val pair = Apply(alpha, xv, (a: Double, x: Double) => (x, a*x))

    pair.observe((x, y))
    pair

//    val x_var = Normal(0, 1)
//    val y_var = Apply(alpha, beta, x_var, (a: Double, b: Double, x: Double) => a * x + b)
//
//    x_var.observe(x)
//    y_var.observe(y)
//    (x_var, y_var)
  }

  println(Importance.probability(alpha,22))

  //println(MetropolisHastings.probability())
//  println(s"Generated ${trials.length} trials.")
//  println(s"Alpha Estimate: ${importance.expectation(alpha, (x: Double) => x)}")
//  importance.kill()

}
