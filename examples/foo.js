var fib = function(num) {
    if (num <= 1) return 1;

    return fib(num - 1) + fib(num - 2);
}
fib(5)
