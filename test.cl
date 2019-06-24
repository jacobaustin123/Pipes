def map(a, b):
    pass

def filter(a, b):
    pass

ls = ["hello.txt"]

for i in range(5):
    print(i)
    print(i + 1)

def foo(x):
    return x + 1


def bar(x):
    def baz(y):
        return y
    
    return baz

a = bar(3)

z = lambda x : x + 1
z(5)

print(list(ls | filter (lambda x : x.endswith(".txt")) | map (lambda x : x.split(".")[0])))

