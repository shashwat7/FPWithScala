
val arr1 = Array[Int](1,1,1,1,1,1)
val arr2 = Array[Int](1,2,3,2,1)
val arr3 = Array[Int](3,2,1,2,3)


def breakIntoTwo(arr: Array[Int], left: Int, right: Int): Unit = {
  if(left > right || right > arr.length-1 || left < 0) println("No minima found!")
  else{
    val mid = (left + right)/2
    if(mid == arr.length-1 && arr(mid) < arr(mid-1)) println("Minima found at: " + mid)
    else if(mid == 0 && arr(mid) < arr(mid+1)) println("Minima found at: " + mid)
    else if(arr(mid) < arr(mid+1) && arr(mid) < arr(mid-1)){
      println("Minima found at: " + mid)
    } else if(arr(mid) > arr(mid + 1)) breakIntoTwo(arr, mid+1, right)
    else breakIntoTwo(arr, left, mid-1)
  }
}

def leftScan(arr: Array[Int], i: Int): Unit = {
  if(i < 0 || i > arr.length-1) println("No minima found!")
  else if(i == 0 && arr(i) < arr(i+1)) println("Minima found at: " + i)
  else if(i == arr.length-1 && arr(i) < arr(i-1)) println("Minima found at: " + i)
  else if(arr(i) < arr(i+1) && arr(i) < arr(i-1)) println("Minima found at: " + i)
  else leftScan(arr, i+1)
}

leftScan(arr1, 0)
breakIntoTwo(arr1,0,arr1.length-1)

leftScan(arr2, 0)
breakIntoTwo(arr2,0,arr2.length-1)

leftScan(arr3, 0)
breakIntoTwo(arr3,0,arr3.length-1)
