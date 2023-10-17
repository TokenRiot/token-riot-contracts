import random



trails = []
for t in range(1000):
    lst = [1 for _ in range(8500)] + [2 for _ in range(4420)] + [3 for _ in range(2550)] + [4 for _ in range(1360)] + [6 for _ in range(170)]

    value = []
    counter = 0
    target = 6
    for i in range(100000):
        if len(lst) == 0 or sum(lst) < target:
            break
        potential = random.sample(lst, 1)
        value += potential
        if sum(value) == target:
            for item in value:
                if item in lst:
                    lst.remove(item)
            counter += 1
            continue
        elif sum(value) > target:
            value = []
        else:
            pass
    trails.append(counter)

print(min(trails))
print(max(trails))
print(sum(trails)/len(trails))