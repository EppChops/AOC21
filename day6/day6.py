import queue
q = queue.Queue()

with open('input.txt', 'r') as f:
    input = f.read().split(',')
    fish1 = 0
    fish2 = 0
    fish3 = 0
    fish4 = 0
    fish5 = 0
    fish6 = 0
    fish7 = 0
    for w in input:
        if (int(w) == 1):
            fish1 += 1
        elif (int(w) == 2):
            fish2 += 1
        elif (int(w) == 3):
            fish3 += 1
        elif (int(w) == 4):
            fish4 += 1
        elif (int(w) == 5):
            fish5 += 1
        elif(int(w) == 6):
            fish6 += 1
        elif (int(w) == 7):
            fish7 +=1
    
    q.put(0)
    q.put(fish1)
    q.put(fish2)
    q.put(fish3)
    q.put(fish4)
    q.put(fish5)
    q.put(fish6)
    q.put(fish7)
    q.put(0)

    i = 0
    end = 256
    while(i < end):
        fish0 = q.get()
        fish1 = q.get()
        fish2 = q.get()
        fish3 = q.get()
        fish4 = q.get()
        fish5 = q.get()
        fish6 = q.get()
        fish7 = q.get()
        fish8 = q.get()
        q.put(fish1)
        q.put(fish2)
        q.put(fish3)
        q.put(fish4)
        q.put(fish5)
        q.put(fish6)
        q.put(fish7 + fish0)
        q.put(fish8)
        q.put(fish0)
        i+= 1

    
    
    sum = 0
    sum += q.get()
    sum += q.get()
    sum += q.get()
    sum += q.get()
    sum += q.get()
    sum += q.get()
    sum += q.get()
    sum += q.get()
    sum += q.get()

    #x = 0
    #while(x < 8):
     #   sum += q.get()
  #      print(q.get())
      #  x += 1

    print(sum)
