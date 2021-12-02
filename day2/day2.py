
with open('input.txt', 'r') as f:
    input = f.read().split('\n')

    depth = 0
    horizontal = 0
    aim = 0
    for row in input:
        if(row[0] == 'f'):
            horizontal += int(row[-1])
            depth += int(row[-1]) * aim
        elif(row[0] == 'u'):
            aim -= int(row[-1])
        elif(row[0] == 'd'):
            aim += int(row[-1])
print(depth)
print(horizontal)
print(depth * horizontal)