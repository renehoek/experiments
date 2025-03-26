# Add two lists using map and lambda

def main():
    numbers1 = [1, 2, 3]
    numbers2 = [4, 5, 6]

    result = map(lambda x, y: x + y, numbers1, numbers2)
    print(list(result))

if __name__ == '__main__':
    main()