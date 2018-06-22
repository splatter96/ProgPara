

object ParserMonade {
  
  case class Parser[A](parse: String => List[(A, String)]) {
    def flatMap[B](f: A => Parser[B]): Parser[B] =
      Parser{ s: String =>
        parse(s).flatMap({case (a,t) => f(a).parse(t)})     
      }
    
    // => heisst lazy evaluation, da sonst versucht parser this  auszuwerten, wenn noch kein argument parse vorhanden ist
    def >>[B](p: => Parser[B]): Parser[B] = this.flatMap(_ => p)
    
    def apply(s: String) = parse(s)
    
    def or(p: Parser[A]) = Parser {s: String => 
      parse(s) ::: p.parse(s)
    }    
   }
  
  //Parser fuer ausdruecke der form a^nb^n
  case class Parser1(input: String){
    def matchS(w: String): Parser[String] = Parser { s: String =>
      if(s.startsWith(w)) List((w, s.stripPrefix(w)))
      else Nil
    }
    
    val eps: Parser[String] = Parser{ s: String => List(("", s))}
    
    val start: Parser[String] = 
      matchS("a") >> start >> matchS("b") or eps
      
    def apply(): List[(String, String)] = (start >> matchS("#"))(input + "#")
  }
  
  //Parser fuer balancierte Klammern
  case class Parser2(input: String){    
    def matchS(w: String): Parser[String] = Parser { s: String =>
      if(s.startsWith(w)) List((w, s.stripPrefix(w)))
      else Nil
    }
    
    val eps: Parser[String] = Parser{ s: String => List(("", s))}
        
    val start: Parser[String] = 
      matchS("(") >> start >> matchS(")") >> start or eps
    
    /*
    val start: Parser[String] =
      matchS("(") >> start >> matchS(")") >> startStrich or eps
      
    val startStrich: Parser[String] =
        start >> startStrich or eps
    */
    
    def apply(): List[(String, String)] = (start >> matchS("#"))(input + "#")
  }
  
  def main(args: Array[String]){
    println(Parser1("aaabbb")())
    println(Parser2("(()())")())
  }
}