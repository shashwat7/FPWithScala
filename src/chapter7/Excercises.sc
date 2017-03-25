import chapter7.Par

// PARALLELIZED SUM
def sum(ints: IndexedSeq[Int]): Par[Int] = {
  if(ints.length <= 1)
    Par.unit(ints.headOption.getOrElse(0))
  else{
    val (l,r) = ints.splitAt(ints.length / 2)
    Par.map2(sum(l), sum(r))(_ + _)
  }
}