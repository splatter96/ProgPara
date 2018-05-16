
object Prakt1 {
  //Quicksort
  def qs(l: List[Int]):List[Int] = {
    l match{
      case head::tail =>
          val left = tail.filter(x => x <= head)
          val right = tail.filter(x => x < head)
          qs(left):::head::qs(right)
      case Nil => Nil
    }
  }
  
  //selbst definiertes Take ohne Tailrecursion
  def take(l: List[Int], a: Int): List[Int] = {
  	(l, a) match{
  		case (Nil, _) => Nil
  		case (_, 0) => Nil
  		case (h::t, a) => h::take(t, a-1)
  	}
  }
  
  //selbstdefiniertes Len mit Tailrecursion
  def myLen(l: List[Int], n: Int): Int = {
  	l match{
  		case Nil => n
  		case h::t => myLen(t, n+1)
  	}
  }
  
  //len mit Hilfe von foldLeft
  def myLenfold(l: List[Int]): Int = {
    l.foldLeft(0)((x,y)=>x+1)    
  }
  
  //max mit Hilfe von foldLeft
  def myMaxfold(l: List[Int]): Int = {
    l.tail.foldLeft(l.head)((x,y) => if(x > y) x else y )
  }
  
  //min mit Hilfe von foldLeft
  def myMinfold(l: List[Int]): Int = {
    l.tail.foldLeft(l.head)((x,y) =>  if(x < y) x else y )
  }
  
  //Hornerschema berechnen mit Hilfe von foldLeft
  def horn(l: List[Double], n: Double): Double = {
    l.tail.foldLeft(l.head)((x,y) => x*n +y)
  }
  
  //Bspl: Typparameter
  def foo[A,B](a: A, b: B) = (a, b)
  case class Point[T](x: T, y:T){
	  def foo = (x,y)
  }
     
   //Bspl. Option Monade
  def myMean(l: List[Option[Double]]): Option[Double] = {
    myMeanHelp(l, 0.0, 0)
  }
   def myMeanHelp(l: List[Option[Double]], sum: Double, n: Int):Option[Double] = {
     l match {
       case h::t => 
          h match {
            case Some(value) => myMeanHelp(t, sum+value, n+1);
            case None => myMeanHelp(t, sum, n);
          }
       case Nil =>
         n match {
           case 0 => None
           case _ => Some(sum/n)
         }
     }    
   }
   
   //And und or selbst gebaut
   def and(x: Boolean, y: Boolean):Boolean = {
     if(x){
       if(y){
         true
       }else {
         false
       }
     }else {
       false
     }
   }
   def or(x: Boolean, y: Boolean):Boolean = {
     if(x){
       true
     }else if(y) {
       true
     }else{
       false
     }
   }
   
   //lazy values
   lazy val x = 1/0
   
   //lazy evaluation
   def foo(cond: Boolean, x: => Int) = if(cond) x else 0
   
   //Streams
   def from(n: Int): Stream[Int] = n #::from(n+1)
   
   //currying
   def add(x: Int)(y: Int) = x+y
   
   //ifThenelse currying
   def ifThenElse[T](cond: Boolean)(th: => T)(el: => T): T = {
      if(cond) th else el 
   }
   
   //Fibonaccizahlen mit Stream
   def fibs: Stream[Int] = 1 #:: 1#:: fibs.zip(fibs.tail).map(x=> x._1 + x._2)
   
   //implizite Klassen (flatmap ist 'bind' von Scala)
   implicit class OptionSqr(o: Option[Int]){
     def sqr():Option[Int] =  o.flatMap(x => Some(x*x))
   } 
  
   def main(args: Array[String]){
     println(myLenfold(List(1,2,3,4,5,6)))
     println(myLenfold(List()))
     
     println(myMaxfold(List(4,-3,-2,1)))
     println(myMinfold(List(4,3,2,-1)))
     
     println(myMaxfold(List(-1,-2,3,4)))
     println(myMinfold(List(-2,-1,0,3,4)))
     
     println(horn(List(2,4,-2,-4), 1))
     println(horn(List(1,0,1), 1))
     
     val l = List(Some(1.0), Some(2.0), Some(3.0)) 
     println(myMean(l))
     println(myMean(List()))
     println(myMean(List(Some(1.0), None, Some(2.0))))
     println(myMean(List(None, Some(1.0), None)))
     println(myMean(Nil))
     
     println("\nand und or")
     println(and(true, false))
     println(and(true, true))
     println(and(false, true))
     println(and(false, false))
     
     println(or(true, false))
     println(or(true, true))
     println(or(false, false))
     println(or(false, true))
     
     val x = 0
     println("Test von division durch null")
     //println(and(x>0, 1/x > 0))
     
     val k = 0
     val m = foo(false, 1/k)
     println(m)
     
     println("Currying")
     println(add(1)(2))
     val add3 = add(3)(_)
     println(add3)
     println(add3(3))
     
     println("Polymorph ifThenElse")
     val y = 0
     val z = ifThenElse[Int](y>0){val z = 1/y
                                  z+1
                                 }{0}
     println(z)
     
     //Fibonaccizahlen
     println(fibs.take(20).toList)
     
     println("implizite Klassen")
     println(Some(2).sqr)
     println(Some(5).sqr)
     println(None.sqr)
   }

  
}