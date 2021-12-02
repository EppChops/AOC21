
with open('input.txt', 'r') as f:
        input = f.read().split('\n')
        inputasint = []
        for i in input:
            inputasint.append(int(i))

        lowestValue = inputasint[0];
        count = 0;

        for row in inputasint:
            if row > lowestValue:
                count += 1
            lowestValue = row
        
        initialvalue = inputasint[0] + inputasint[1] + inputasint[2]
        count2 = 0
        tst = inputasint[0:3]
        i = 0
        for i in range(0, len(inputasint)-2):
            if (initialvalue >= inputasint[i] + inputasint[i+1] + inputasint[i+2]):
                count2 += 1
            else:
                initialvalue = inputasint[i] + inputasint[i+1] + inputasint[i+2]

print(count)
print(count2)

