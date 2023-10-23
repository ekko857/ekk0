taxa = [ ('Myotis lucifugus','Chiroptera'),
         ('Gerbillus henleyi','Rodentia',),
         ('Peromyscus crinitus', 'Rodentia'),
         ('Mus domesticus', 'Rodentia'),
         ('Cleithrionomys rutilus', 'Rodentia'),
         ('Microgale dobsoni', 'Afrosoricida'),
         ('Microgale talazaci', 'Afrosoricida'),
         ('Lyacon pictus', 'Carnivora'),
         ('Arctocephalus gazella', 'Carnivora'),
         ('Canis lupus', 'Carnivora'),
        ]

# Write a python script to populate a dictionary called taxa_dic derived from
# taxa so that it maps order names to sets of taxa and prints it to screen.
# 
# An example output is:

#  
# 'Chiroptera' : set(['Myotis lucifugus']) ... etc. 
# OR, 
# 'Chiroptera': {'Myotis  lucifugus'} ... etc

#### Your solution here #### 
taxa_dic = {}
for species, order in taxa:
    if order in taxa_dic:
        taxa_dic[order].add(species)
    else:
        taxa_dic[order] = {species}
for order_name, species_set in taxa_dic.items():
    print(f"{order_name}:{species_set}")



# Now write a list comprehension that does the same (including the printing after the dictionary has been created)  


#### Your solution here #### 

taxa_dic = {order_name: {species for species, order in taxa if order == order_name} 
            for order_name in set(order for species, order in taxa)}

for order_name, species_set in taxa_dic.items():
    print(f"{order_name}:{species_set}")
