import functools
import os
def ls():
	return os.listdir()

def cd(path):
	os.chdir(path)

def pwd():
	return os.getcwd()
filter(lambda x: x.endswith(".py"), ["hello.py"])




print(list(map(lambda x: x.split(".")[0] + ".csv", filter(lambda x: x.endswith(".py"), ls()))))