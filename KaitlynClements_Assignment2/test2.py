#2.1
def loopingTriangle():
    num = 1
    while num <=10:
       print ("x"*num)
       num = num+1
#2.2
def fizzBuzz():
    for num in range(1, 29):
        if ((num%4 == 0) and (num %7 == 0)):
            print("Divisible by both 4 and 7")
        elif num%4 == 0:
            print("Divisible by 4")
        elif num%7 == 0:
            print("Divisible by 7, but not 4")
        else:
            print(num)
#2.3
def nxnGrid (size):
    print("Grid size = ", size)
    grid = ""
    for i in range(size):
        for j in range(size):
            if (i+j)%2 == 0:
                grid = grid + ' '
            else:
                grid = grid + '*'
        grid = grid + "\n"
    print(grid)
    
nxnGrid(8)
nxnGrid(14)