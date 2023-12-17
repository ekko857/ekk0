import csv
import sys
import doctest

#Define function
def is_an_oak(name):
    """ Returns True if name is starts with 'quercus' 
    >>> is_an_oak('Fagus sylvatica')
    False
    >>> is_an_oak('Quercus')
    True
    >>> is_an_oak('Quercussyiuohohyiou')
    False
    """
    return name.lower() == ('quercus')

def main(argv): 
    f = open('../data/TestOaksData.csv','r')
    g = open('../data/JustOaksData.csv','w')
    taxa = csv.reader(f)
    csvwrite = csv.writer(g)
    oaks = set()

    is_header = True
        for row in taxa:
            if is_header:
                is_header = False
                continue
        # import ipdb; ipdb.set_trace()
        print(row)
        print ("The genus is: ") 
        print(row[0] + '\n')
        if is_an_oak(row[0]):
            print('FOUND AN OAK!\n')
            csvwrite.writerow([row[0], row[1]])    

    return 0
    
if (__name__ == "__main__"):
    status = main(sys.argv)

#run as python3 -m doctest -v oaks_debugme.py 