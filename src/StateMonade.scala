

object StateMonade {
  case class State[S, A](run: S => (A,S)){
    
    //Class kann wie Funktion ausgefuerht werden (State(x) statt State.run(x))
    def apply(s: S) = run(s)
    
    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State {s: S =>
        val (a, t) = run(s)
        f(a).run(t)      
    }
    
    def >>[B](s: State[S, B]): State[S, B] = this.flatMap(_ => s) //f wird durch rechte Monade (s) ersetzt, letzte zeile in flatMap damit zu s.run(t)
  }
  
  //Beispiele
  val st = State{s: Int =>
    if(s%2 ==0) ("even", s+1)
    else ("odd", s+1)
  }
    
  //Beispiel push und pop fuer Stack
  def push(n: Int): State[List[Int], Unit] = State{s: List[Int] =>
    ((), n::s)
  }
  
  def pop(): State[List[Int], Option[Int]] = State{s: List[Int] =>   
    s match {
      case a::xs => (Some(a), xs)
      case Nil => (None, Nil)
    }
  }
  
  def main(args: Array[String]): Unit = {
    //Einfache Anwendung
    val a = st.run(1)
    println(a)
    
    //Verkettung
    val b = st.run(st.run(1)._2)
    println(b)
    
    //Verkettung der Run-Funktionen, aehnlich wie Funktinosverkettungsoperator aus Mathematik (Kringel)
    val c = st.flatMap(_ => st).flatMap(_ => st)(1)
    println(c)
    
    //val d = push(10).run(List(1,2,3))
    val d = push(10)(List(1,2,3))
    println(d)
    
    //val e = (push(10) >> push(11) >> push(13) >> pop()).run(List(1, 2, 3))
    val e = (push(10) >> push(11) >> push(13) >> pop())(List(1, 2, 3))
    println(e)
    
  }
}