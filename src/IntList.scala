
object IntList {
  abstract sealed class IntList {
    def cons(v: Int): IntList
    def tail(): IntList
    def head(): Int
    def contains(v: Int): Boolean
    def toString(): String
  }
  
  case object Empty extends IntList {
    def cons(v: Int) = {
      Cons(v,  Empty) 
    }
    def tail() = {
      Empty
    }
    def head() = {
      throw new NoSuchElementException
    }
    def contains(v: Int) = {
      false
    }
    override def toString() = {
      ""
    }
  }
  
  case class Cons(value: Int, n: IntList) extends IntList {
    def cons(v: Int) = {
      Cons(v,  Cons(value, n)) 
    }
    def tail() = {
      n
    }
    def head() = {
      value
    }
    def contains(v: Int) = {
      v == value || n.contains(v)
    }
    override def toString() = {
      value.toString().concat(", ".concat(n.toString()))
    }
  }
  
  def main(args: Array[String]){
    val t = Cons(1, Cons(2, Cons(3, Cons(4, Empty))))
    println(t.head())
    println(t.tail())
    println(t.contains(2))
    println(t.contains(5))
    
    println("Liste 2")
    val t2 = t.cons(0)
    println(t2)
    println(t2.tail())
    println(t2.contains(0))
    
    println(t2.toString())
  }
}