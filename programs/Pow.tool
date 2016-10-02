program Pow {
  do(new Power().pow(2, 4));
}

class Power {
  def pow(x: Int, n: Int): Int = {
    var sum: Int;
    var bool: Boolean;
    sum = 1;
    bool = true;
    while (bool) {
      sum = sum * x;
      n = n - 1;
      bool = !(n == 0);
    }
    println(sum);
    return sum;
  }
}

