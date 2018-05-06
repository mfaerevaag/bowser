print("cookie: " + document.cookie);
print("cookie.length: " + document.cookie.length);

var c, i = 0, result = "";
while (i < document.cookie.length) {
    c = document.cookie[i];

    result += c;

    i += 1;
}

print("found: " + result);
