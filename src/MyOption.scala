

object MyOption {
  
  abstract sealed class MyOption[A]{
    def bind[B](f: (A) => MyOption[B]): MyOption[B]
  }
  
  case class MyNone[A]() extends MyOption[A]{
    def bind[B](f: (A) => MyOption[B]): MyOption[B] = {
      MyNone()
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
    val mx = MySome(x)
    val mxb = mx.bind(x => MySome(x))
    println(mxb + " == " + mx)
    
    val mn = MyNone()
    val mnb = mn.bind(x => MySome())
    println(mnb + " == " + mn)
    
    
    println("Gesetz 3")
    val left = mx.bind(sqr).bind(mkString)
    val right = mx.bind((x) => sqr(x).bind(mkString))
    println(left + " == " + right)
    
    val leftn = MyNone().bind(sqr).bind(mkString)
    val rightn = MyNone().bind((x: Int) => sqr(x).bind(mkString))
    println(leftn + " == " + rightn)
  }
}