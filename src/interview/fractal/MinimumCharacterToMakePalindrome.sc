// String of length 1 is always a palindrome => 0
// String of length 2 is always a palindrome if both characters are same, else add one character either side will make it a palindrome => 0/1
// String of length 3, add one more character if your substring is palindrome

def minimumCharacterToMakePalindrome(input: String): Int = {
  val len = input.length
  if(input.length <=1) 0
  else if(isPalindrome(input)) 0
  else {
    val left = input.substring(0, len -1) // Add a character in the back
    val right = input.substring(1, len) // Add a character in the front
    Math.min(
      minimumCharacterToMakePalindrome(left),
      minimumCharacterToMakePalindrome(right)
    ) + 1
    //Complexity: T(n) = 2.T(n-1) + 1
  }
}

def isPalindrome(str: String) = str.reverse == str // O(n) complexity

minimumCharacterToMakePalindrome("aba")