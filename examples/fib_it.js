function fib(num){
    var a = 1, b = 0, temp;

    while (num >= 0){
        temp = a;
        a += b;
        b = temp;
        num = num--;
    }

    return b;
}

var c = 0;
while (c < 10) {
    var res = fib(c);
    print("fib(" + c + ") -> " + res);
    c = c + 1;
}
