import scala.collection.mutable._
import scala.collection.immutable._
import scala.util._
import scala.math._
import scala.util.control.Breaks._

abstract class Figure(val side1: Double,
                      val side2: Double,
                      val side3: Double,
                      val side4: Double,
                      val angle: Double,
                      val radius: Double) {
  def area(): Double = {return math.pow(side1, 2)}
  def perimeter(): Double = { return side1}
}

class Quadrangle(override val side1: Double,
                 override val side2: Double,
                 override val side3: Double,
                 override val side4: Double,
                 override val angle: Double,
                 override val radius: Double)
  extends Figure(
  side1: Double,
  side2: Double,
  side3: Double,
  side4: Double,
  angle: Double,
  radius: Double) {
  override def area(): Double = {
    return (side1 * side2 * math.sin(math.toRadians(angle)))
  }

  override def perimeter(): Double = {
    return (2 * side1 + 2 * side2)
  }
}

class Circle(override val side1: Double,
             override val side2: Double,
             override val side3: Double,
             override val side4: Double,
             override val angle: Double,
             override val radius: Double)
  extends Figure(
  side1: Double,
  side2: Double,
  side3: Double,
  side4: Double,
  angle: Double,
  radius: Double){
  override def area(): Double = { return (math.Pi * math.pow(radius, 2)) }
  override def perimeter(): Double = { return (2 * math.Pi * radius) }
}

class Pentagon(override val side1: Double,
               override val side2: Double,
               override val side3: Double,
               override val side4: Double,
               override val angle: Double,
               override val radius: Double)
  extends Figure(
    side1: Double,
    side2: Double,
    side3: Double,
    side4: Double,
    angle: Double,
    radius: Double){
  val h: Double = (0.5*side1)/math.tan(math.Pi/(36.6))
  val tr: Double = 0.5 * side1 * h
  override def area(): Double =  { return (0.25*5*pow(side1,2)*(1/tan(Pi/5))) }
  override def perimeter(): Double = { return (5 * side1) }
}

class Hexagon(override val side1: Double,
              override val side2: Double,
              override val side3: Double,
              override val side4: Double,
              override val angle: Double,
              override val radius: Double)
  extends Figure(
    side1: Double,
    side2: Double,
    side3: Double,
    side4: Double,
    angle: Double,
    radius: Double){
  override def area(): Double = { return (0.25*6*pow(side1,2)*(1/tan(Pi/6)))}
  override def perimeter(): Double = { return (6 * side1) }
}

class Square(override val side1: Double,
             override val side2: Double,
             override val side3: Double,
             override val side4: Double,
             override val angle: Double,
             override val radius: Double)
  extends Quadrangle(
    side1: Double,
    side2: Double,
    side3: Double,
    side4: Double,
    angle: Double,
    radius: Double){
  override def area(): Double = { return (math.pow(side1, 2)) }
  override def perimeter(): Double = { return (4 * side1) }
}

class Rectangle(override val side1: Double,
                 override val side2: Double,
                 override val side3: Double,
                 override val side4: Double,
                 override val angle: Double,
                 override val radius: Double)
  extends Quadrangle(
    side1: Double,
    side2: Double,
    side3: Double,
    side4: Double,
    angle: Double,
    radius: Double){
  override def area(): Double = { return (side1 * side2 * 2) }
  override def perimeter(): Double = { return (2 * side1 + 2 * side2) }
}

class Rhombus(override val side1: Double,
              override val side2: Double,
              override val side3: Double,
              override val side4: Double,
              override val angle: Double,
              override val radius: Double)
  extends Quadrangle(
    side1: Double,
    side2: Double,
    side3: Double,
    side4: Double,
    angle: Double,
    radius: Double) {
  override def area(): Double = {
    return (math.pow(side1, 2) * (math.sin(math.toRadians(angle))))
  }

  override def perimeter(): Double = {
    return (4 * side1)
  }
}


object Figures {
  def main(args: Array[String]): Unit = {

    var fig_data = new ArrayBuffer[Double]
    val first_par = args(0).toList


    var sum: Int = 0
    for( elem <- first_par) {
      if (elem == 'p' || elem == 'h' || elem == 'c') {
        sum = sum + 1
      } else if ( elem == 'q') {
        sum = sum + 5
      } else {
        println("That figure can't be calculated in this program!")
        break
      }
    }
    if (sum != args.length-1) {
      println("To few/many parameters!")
      break
    }

    def isNumeric(str: String) = try {
      // if we can transform the string to int, we return true
      str.toInt
      true
    } catch {
      // on the other situation we return false
      case e: NumberFormatException =>
        false
    }
    // now we loop throught all of arguments of input
    for (i <- 0 to args.length-1) {
      // and we check if it is numeric
      if (isNumeric(args(i))) {
        // if yes, than
        // we set all input values as convertion string arguments to Integer
        fig_data.insert(i, args(i).toDouble)
      }
      else {
        fig_data.insert(i, 0.0)
      }
    }

    fig_data.remove(0)

    for (elem <- first_par) {
      if (elem == 'c') {
        val circle = new Circle(0,
          0,
          0,
          0,
          0,
          fig_data(0))
        println("Circle area: " + circle.area())
        println("Circle perimeter: " + circle.perimeter())
        fig_data.remove(0)

      }
      else if (elem == 'q') {
        if (fig_data(0) == fig_data(1) &
          fig_data(1) == fig_data(2) &
          fig_data(2) == fig_data(3) &
          fig_data(3) == fig_data(0) &
          fig_data(4) == 90.0) {
          val quadrangle = new Square(fig_data(0),
            fig_data(1),
            fig_data(2),
            fig_data(3),
            fig_data(4),
            0)
          println("Square area: " + quadrangle.area())
          println("Square perimeter: " + quadrangle.perimeter())
          fig_data.remove(4)
          fig_data.remove(3)
          fig_data.remove(2)
          fig_data.remove(1)
          fig_data.remove(0)
        }
        else if (fig_data(0) == fig_data(1) &
                fig_data(1) != fig_data(2) &
                fig_data(2) == fig_data(3) &
                fig_data(3) != fig_data(0) &
                fig_data(4) == 90.0) {
          val quadrangle = new Rectangle(fig_data(0),
            fig_data(1),
            fig_data(2),
            fig_data(3),
            fig_data(4),
            0)
          println("Rectangle area: " + quadrangle.area())
          println("Rectangle perimeter: " + quadrangle.perimeter())
          fig_data.remove(4)
          fig_data.remove(3)
          fig_data.remove(2)
          fig_data.remove(1)
          fig_data.remove(0)
        }
        else if (fig_data(0) == fig_data(1) &
                fig_data(1) == fig_data(2) &
                fig_data(2) == fig_data(3) &
                fig_data(3) == fig_data(0) &
                fig_data(4) != 90.0) {
          val quadrangle = new Rhombus(fig_data(0),
            fig_data(1),
            fig_data(2),
            fig_data(3),
            fig_data(4),
            0)
          println("Rhombus area: " + quadrangle.area())
          println("Rhombus perimeter: " + quadrangle.perimeter())
          fig_data.remove(4)
          fig_data.remove(3)
          fig_data.remove(2)
          fig_data.remove(1)
          fig_data.remove(0)
        }
      }
      else if (elem == 'p') {
        val pentagon = new Pentagon(fig_data(0),
          0,
          0,
          0,
          0,
          0)
        println("Pentagon area: " + pentagon.area())
        println("Pentagon perimeter: " + pentagon.perimeter())
        fig_data.remove(0)
      }
      else if (elem == 'h') {
        val hexagon = new Hexagon(fig_data(0),
          0,
          0,
          0,
          0,
          0)
        println("Hexagon area: " + hexagon.area())
        println("Hexagon perimeter: " + hexagon.perimeter())
        fig_data.remove(0)
      }
    }
  }
}

