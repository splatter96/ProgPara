

object MyOption {
  
  abstract sealed class MyOption[A]{
    def bind[B](f: (A) => MyOption[B]): MyOption[B]
  }
  
  case class MyNil[A]() extends MyOption[A]{
    def bind[B](f: (A) => MyOption[B]): MyOption[B] = {
      MyNil()
    }
  }
  
  case class MySome[A](v: A) extends MyOption[A]{
    def bind[B](f: (A) => MyOption[B]): MyOption[B] = {
      f(v)
    }
  }
  
  
  def main(args: Array[String]) {
    def sqr(x: Double): MyOption[Double] = {
      MySome(x*x)
    }
    
    def mkString(x: Double): MyOption[String] = {
      MySome("String " + x + "!")
    }    
    
    println("Monadengesetze")
    
    println("Gesetz 1")
    val x = 1.0
    println(x + " => " + MySome(x))    
    println(MySome(x).bind(sqr) + " = " + sqr(x))
    
    println("Gesetz 2")
    //TODO
    
    println("Gesetz 3")
    val mx = MySome(x)
    val left = mx.bind(sqr).bind(mkString)
    val right = mx.bind((x) => sqr(x).bind(mkString))
    println(left + " == " + right)
  }
}