print(tainted);

var evil = 0;

print("before: ", evil);

evil += tainted;

print("after", evil);
