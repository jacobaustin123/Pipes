import functools

ls = ["hello.txt"]
for i in range (5) :
	print(i)
	print(i + 1)

def foo(x):
	return x + 1

def bar(x):
	def baz(y):
		return y

	return baz

a = bar(3)
z = lambda x: x + 1
print(z(5))
print(list(map(lambda x: x.split(".")[0], filter(lambda x: x.endswith(".txt"), ls))))
