import os

print(os.getcwd)


def add(x, y):
    return x + y


def subtract(x, y):
    return x - y


def multiply(x, y):
    return x * y


def divide(x, y):
    if y == 0:
        raise ZeroDivisionError("You can't have 0 in denominator")
    else:
        return x / y


print(divide(5, 0))
