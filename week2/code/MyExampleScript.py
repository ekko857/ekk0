#######1
x=11
for i in range(x):
    if i > 3: 
        print(i)


######2



#####
a = range(10)
print(a)

#####
for i in range(1,10):
    print(i)
    

######iterable
my_iterable = [1,2,3]
type(my_iterable)

my_iterator = iter(my_iterable)
type(my_iterator)

next(my_iterator)

###for loops
for i in range(5):
    print (i)

my_list = [0, 2, "geronimo", 3.0, True, False]
for k in my_list:
    print(k)

total = 0
summands = [0, 1, 11, 111, 1111]
for s in summands:
    total = total + s
    print(total)

####White loop
z = 0
while z < 100:
    z = z + 1
    print(z)

###########
###########

#function
def foo(x):
    x = x*x
    print(x)
    return x 
    


y = foo(2)
y
type(y)





def foo(x):
    x *= x # same as x = x*x
    print(x)

foo(2)



##########################
##########################
x= 0 ; y= 2

if x< y:
    print('yes')

if x:
    print('yes')

if x==0:
    print('yes')

if y:
    print('yes')

if y== 2:
    print('yes')

x = True

if x:
    print('yes')

if x == True:
    print('yes')


#####
def foo_1(x):
    return x**0.5
foo_1(4)

def foo_2(x, y):
    if x > y:
        return x
    return y
foo_2(5, 2)

def foo_3(x, y, z):
    if x > y:
        tmp = y
        y = x
        x = tmp
    if y > z:
        tmp = z
        z = y
        y = tmp
    return [x, y ,z]
foo_3(4,5,1)
a=range(1,8)
print(a)

def hello_1(x):
    for j in range (x):
        if j % 3 == 0:
            print('hello')
        else:
            print('none')
hello_1(10)

########
def hello_2(x):
    for j in range(x):
        if j % 5 ==3:
            print("hello")
        elif j % 4 == 3:
            print('bye')
    print('end')
hello_2(12)



def hello_4(x):
    while x != 15:
        print('hello')
        x = x + 3
    print('yes')

hello_4(0)

def hello_5(x):
    while x < 100:
        if x == 31:
            for k in range(7):
                print('hello')
        elif x == 18:
            print('hello')
        x = x + 1
    print(' ')


#### comprehensions
x = [i for i in range(10)]
print(x)

x = []
for i in range(10):
    x.append(i)
print(x)

x = [i.lower() for i in ["LIST","COMPREHENSIONS","ARE","COOL"]]
print(x)

matrix = [[1,2,3],[4,5,6],[7,8,9]]
flattened_matrix = []
for row in matrix:
    for n in row:
        flattened_matrix.append(n)
print(flattened_matrix)

words = ["These", "are", "some", "words"]
first_letters = set()
for w in words:
    first_letters.add(w[0])
print(first_letters)

i = 1
x = 0
for i in range(10):
    x += 1
    print(i)
print(x)

i = 1
x = 0
def a_function(y):
    x = 0
    for i in range(y):
        x += 1
    return x
a_function(10)


_a_global = 10 # a global variable

if _a_global >= 5:
    _b_global = _a_global + 5 # also a global variable
    
print("Before calling a_function, outside the function, the value of _a_global is", _a_global)
print("Before calling a_function, outside the function, the value of _b_global is", _b_global)

def a_function():
    _a_global = 4 # a local variable
    
    if _a_global >= 4:
        _b_global = _a_global + 5 # also a local variable
    
    _a_local = 3
    
    print("Inside the function, the value of _a_global is", _a_global)
    print("Inside the function, the value of _b_global is", _b_global)
    print("Inside the function, the value of _a_local is", _a_local)
    
a_function()

print("After calling a_function, outside the function, the value of _a_global is (still)", _a_global)
print("After calling a_function, outside the function, the value of _b_global is (still)", _b_global)
print("After calling a_function, outside the function, the value of _a_local is ", _a_global)

def modify_list_1(some_list):
    print('got', some_list)
    some_list = [1, 2, 3, 4]
    print('set to', some_list)


my_iterable = [1,2,3]

type(my_iterable)

list

my_iterator = iter(my_iterable)



def modify_list_1(some_list):
    print('got', some_list)
    some_list = [1, 2, 3, 4]
    print('set to', some_list)

my_list = [1, 2, 3]

print('before, my_list =', my_list)

modify_list_1(my_list)



f = open('../sandbox/test.txt', 'r')
for line in f:
    print(line)

f.close()

f = open('../sandbox/test.txt', 'r')
for line in f:
    if len(line.strip()) > 0:
        print(line)

f.close()

