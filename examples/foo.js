document.cookie = "secret";

var i = 0;
print("cookie.length: " + document.cookie.length);
while (i < document.cookie.length) {
    print(document.cookie[i]);
    i += 1;
}
