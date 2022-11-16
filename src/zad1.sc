import scala.annotation.tailrec

def composingFun[A](list:List[A=>A]):A=>A = {
  if list.isEmpty then throw Error(s"brak zawartosci")
  @tailrec
  def composingFun_rec(xs:List[A=>A], res:A=>A):A=>A = {
    if xs.isEmpty then res
    else composingFun_rec(xs.tail, res.compose(xs.head))
  }
  composingFun_rec(list.tail, list.head)
}

val functionsComposed = List(((x:Double)=>x+x), (x:Double)=>x*x, (x:Double)=> 1/x);

composingFun(functionsComposed)(3);


def composingFun_fold[A](list:List[A=>A]):A=>A = {
  list.tail.foldLeft(list.head)( (m:A=>A, n:A=>A)=>{m.compose(n)})
}

composingFun_fold(functionsComposed)(3)