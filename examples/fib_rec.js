var fib = function(num) {
    if (num <= 1) return num;

    return fib(num - 1) + fib(num - 2);
}

var c = 0;
while (c < 10) {
    var res = fib(c);
    print("fib(" + c + ") -> " + res);
    c = c + 1;
}
