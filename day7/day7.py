
import queue as Q

pq = Q.PriorityQueue()

def fuelcost(x, y):
    steps = abs(x-y)
    cost = (steps * (steps + 1))/2
  
    return cost


with open('input.txt', 'r') as f:
    input = f.read().split(',')
    
    inputArr = []
    for i in input:
        inputArr.append(int(i))
    maxValue = max(inputArr)
    for x in range(0,maxValue):
        fuelCost = []
        for y in inputArr:
            #fuelCost.append(abs(x-y))
            fuelCost.append(fuelcost(x,y))

        pq.put(sum(fuelCost))



    print(pq.get())