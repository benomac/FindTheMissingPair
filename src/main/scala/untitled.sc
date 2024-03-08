val list = List(2, 3)

val string = "beeeb"
def isItPall(s: String): Boolean = {
  if (s.length == 1) true
  else if (s.head == s.last && s.length == 2) true
  else if (s.head == s.last)
    isItPall(s.drop(1).dropRight(1))
  else false

}
isItPall(string)

def isItPallMatch(s: String): Boolean = {
  s match {
    case s if s.length == 1 => true
    case s if s.head == s.last && s.length == 2 => true
    case s if s.head == s.last => isItPall(s.drop(1).dropRight(1))
    case _ => false
  }
}
isItPallMatch(string)

