function add(a, b) {
    return a + b;
};

function foo(fun, a, b) {
    return fun(a, b);
};

var a = 1, b = 2;
print("foo(" + a + ", " + b + ") = " + foo(add, a, b));
