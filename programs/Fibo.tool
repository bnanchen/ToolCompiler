program Fibonacci {
  println("hello");
  println(new Fibo().fib(2));
}

class Fibo {
  def fib(n: Int): Int = {
    var result: Int;
    if (n == 0) {
      result = n;
    }
    if (n == 1) {
      result = n;
    } else {
      result = (this.fib(n - 1) + this.fib(n - 2));
    }
    return result;
  }
}

