
def create_list_from_tuples(a):
    new_list = [j for i in a for j in i]
    return new_list

    # new_list = []
    # for i in a:
    #     for j in i:
    #         new_list.append(j)
    #
    # return new_list

def main():
    l = [(1,5,4),(1,2),(8,5,19,0)]
    print(create_list_from_tuples(l))
    # [1,5,4,1,2,8,5,19,0]

    

if __name__ == '__main__':
    main()