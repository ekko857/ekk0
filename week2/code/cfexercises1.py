
def foo_1(x):
    return x ** 0.5

def foo_2(x, y):
    if x > y:
        return x
    return y

def foo_3(x, y, z):
    if x > y:
        tmp = y
        y = x
        x = tmp
    if y > z:
        tmp = z
        z = y
        y = tmp
    return [x, y, z]

def foo_4(x):
    result = 1
    for i in range(1, x + 1):
        result = result * i
    return result

def foo_5(x): # a recursive function that calculates the factorial of x
    if x == 1:
        return 1
    elif x == 0:
        return 1
    return x * foo_5(x - 1)
     
def foo_6(x): # Calculate the factorial of x in a different way; no if statement involved
    facto = 1
    if x == 0:
        return 1
    while x >= 1:
        facto = facto * x
        x = x - 1
    return facto

print(foo_1(10))
print(foo_1(20))
print(foo_2(3,4))
print(foo_2(7,6))
print(foo_3(7,8,9))
print(foo_3(5,7,3))
print(foo_4(13))
print(foo_4(11))
print(foo_5(5))
print(foo_5(0))
print(foo_6(0))
print(foo_6(7))
