import functools
import os
def ls():
	return os.listdir()

def cd(path):
	os.chdir(path)

def pwd():
	return os.getcwd()
ls()
cd(src)