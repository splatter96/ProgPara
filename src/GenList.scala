
object GenList {
  abstract sealed class GenList[T] {
    def tail(): GenList[T]
    def head(): T
    def contains(v: T): Boolean
    def toString(): String
  }
  
  case class Empty[T]() extends GenList[T] {
    def tail() = {
      Empty[T]
    }
    def head() = {
      throw new NoSuchElementException
    }
    def contains(v: T) = {
      false
    }
    override def toString() = {
      ""
    }
  }
  
  case class Cons[T](value: T, n: GenList[T]) extends GenList[T] {
    def tail() = {
      n
    }
    def head() = {
      value
    }
    def contains(v: T) = {
      v == value || n.contains(v)
    }
    override def toString() = {
      value.toString().concat(", ".concat(n.toString()))
    }
  }
  
  def main(args: Array[String]){
    val t = Cons(1, Cons(2, Cons(3, Cons(4, Empty()))))
    println(t.head())
    println(t.tail())
    println(t.contains(2))
    println(t.contains(5))
    
    println("Liste 2:")
    val t2 = Cons(1.23, Cons(2.2, Cons(4.3, Empty())))
    println(t2)
    println(t2.tail())
    println(t2.contains(2))
    println(t2.contains(5))
    
  }
}