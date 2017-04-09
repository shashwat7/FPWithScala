/*
a = [4, 5, 1, 2, 3], t = 4
return the index if it exists else return  -1
*/

val arr = Array(4,5,1,2,3)

def search(k: Int): Int = {
  var left = 0
  var right = arr.length - 1
  var mid = (left + right)/2
  var idx = -1
  while(left < mid && right > mid){
    if(arr(mid) == k) mid
    else if(arr(mid) < k){
      if(arr(right) > k){
        // on the right
        left = mid + 1
        mid = (left + right)/2
      }
    } else if(arr(mid) > k && arr(left) < k){
      // on the left
      right = mid - 1
      mid = (left + right)/2
    } else if()
  }
}