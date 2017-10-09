def sum(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, f(a) + acc)
  }
  loop(a, 0)
}

sum(x => x*x)(1, 5)

def gcd(a:Int, b:Int) : Int = {
  if(b==0) a else gcd(b, a%b)
}

gcd(15,3)
gcd(3,15)
gcd(0,10)
