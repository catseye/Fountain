# A simplified "Kennel Story" mocked-up in Python.

def kennel():
    x = 0
    dx = 1
    lx = 1
    i = 0
    while i < 11111:
        if dx > 0:
            if x == 0:
                print("The dog ran away.")
            else:
                print("The dog ran further away.")
            x += dx
            if x == lx:
                dx = -1 * dx
        else:
            x += dx
            if x == 0:
                print("The dog came back home.")
            else:
                print("The dog journeyed back.")
            if x == 0:
                dx = -1 * dx
        i += 1
        if x == 0:
            print("Time passed.")
            lx += 1


if __name__ == '__main__':
    kennel()
