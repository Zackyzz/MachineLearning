import math, random

N = 100
ITERATIONS = 50
PRECISION = 16
MAX = 5

def f(x):
    return math.sinh(math.cos(x)*math.cos(x)+1)
    
 
def codify(n):
    return int(n*(1<<PRECISION))
    
    
def decodify(n):
    return n/(1<<PRECISION)
    
    
def initialize_population(n):
    return list(map(lambda x: x*random.random(), [MAX]*n))

    
def fitnessed_population(population):
    return list(zip(population, (list(map(f,population)))))
   

def sort_fitness(f_population):                    
    return sorted(f_population, key = lambda x: x[1])
 
   
def select(sf_population):
    f_sum = sum(1/x[1] for x in sf_population)
    selected = []
    
    while len(selected) < 2:
        rnd = random.random()*f_sum
        proportion = 0.0
        for i in sf_population:
            proportion += 1/i[1]
            if rnd < proportion and i[0] not in selected:
                selected.append(i[0])
                break
    return selected
    
    
def crossover(selected_chromosomes):
    temp1 = codify(selected_chromosomes[0])
    temp2 = codify(selected_chromosomes[1])
    
    digits = PRECISION + len(bin(MAX)[2:])
    
    length = 1 + random.randint(0,digits)
    position = random.randint(0,digits//2)    
    mask = ((1 << length) - 1) << position
    
    new_c1 = decodify(temp1 ^ (mask & temp2))
    new_c2 = decodify(temp2 ^ (mask & temp1)) 
    if new_c1 < MAX or new_c2 < MAX:
        return [new_c1, f(new_c1)], [new_c2, f(new_c2)]
    else:
        return crossover(selected_chromosomes)

    
def mutate(chromosome):
    codified_chromosome = codify(chromosome)
    digits = PRECISION + len(bin(int(chromosome))[2:])
    
    for i in range(0, random.randint(1,4)):
        rnd = random.randint(0, digits)
        codified_chromosome ^= 1<<rnd
        
    mutated_choromosome = decodify(codified_chromosome)  
    if mutated_choromosome < MAX:
        return [mutated_choromosome, f(mutated_choromosome)]
    else:
        return mutate(chromosome)
  

if __name__ == "__main__":
    population = initialize_population(N)
    fitnessed = sort_fitness(fitnessed_population(population))
    print("The initial minimum is:\nf(",fitnessed[0][0],") =", fitnessed[0][1],"\n")
    best_p = fitnessed[:N//2]
    bestest = (float('inf'), float('inf'))
    
    test = False
    epoch = 1
    while epoch <= ITERATIONS or not test:
        while len(best_p) < N:
            XY=select(fitnessed[:N//2])
            xy=crossover(XY)
            best_p.append(xy[0])
            best_p.append(xy[1])
            
        for i in range (N//2+1,N//2+N//10+2):
            best_p[i]=mutate(best_p[i][0])
            
        fitnessed = sort_fitness(best_p)
        best_p = fitnessed[:N//2]        
        last = fitnessed[0]
        
        if last[1] == bestest[1]:
            test = True
        else:
            test = False
            print("The minimum at epoch", epoch, "is:\nf(",last[0],") =", last[1])
            
        if bestest[1] > last[1]:
           bestest = last
        epoch += 1    
    print("\nThe minimum found is:\nf(",bestest[0],") =", bestest[1],"\n")