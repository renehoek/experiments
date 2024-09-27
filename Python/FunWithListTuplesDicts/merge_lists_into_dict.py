def merge_lists_into_dictionary(l1, l2):
    result = dict(zip(l1, l2))
    return result

    # result = {}
    # for cnt, k in enumerate(l1):
    #     result[k] = l2[cnt]
    #
    # return result

def main():
    keys = ['red', 'green', 'blue']
    values = ['#FF0000','#008000', '#0000FF']
    result = merge_lists_into_dictionary(keys, values)
    print(result) #{'red': '#FF0000', 'green': '#008000', 'blue': '#0000FF'}

if __name__ == '__main__':
    main()