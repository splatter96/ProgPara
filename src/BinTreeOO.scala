

object BinTreeOO {
  abstract sealed class Btree {
    def find(v: Int): Boolean
  }
  
  case object Empty extends Btree {
    def find(v: Int) = false
  }
  
  case class Node(value: Int, l: Btree, r: Btree) extends Btree {
    def find(v: Int): Boolean = {
      v == value || l.find(v) || r.find(v)
    }
  }
  
  def main(args: Array[String]){
    val t = Node(3, Node(1, Empty, Empty), Node(5, Empty, Empty))
    println(t.find(5))
  }
}