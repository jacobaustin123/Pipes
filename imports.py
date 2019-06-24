import functools
import os

@macro
def ls():
    return os.listdir()

@macro
def cd(path):
    os.chdir(path)

@macro
def pwd():
    return os.getcwd()

