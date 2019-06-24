import functools
def foo(x, y):
	return x

z = functools.partial(foo, 3)
z(3, 4)