def buggyfunc(x):
    y = x
    for i in range(x):
        try:
            y = y-1
            z = x/y
        except:
            print()
    return z

buggyfunc(20)

