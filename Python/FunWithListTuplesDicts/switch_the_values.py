def switch_the_values(l):
    #Write the code

    # # It works, but have to manually keep track of a lookup index.
    # new_dict = {}
    # index = 0
    # for item in l:
    #   lookupindex = 0 - index - 1
    #   new_dict[list(item.keys())[0]] = list(l[lookupindex].values())[0]
    #   index += 1
    #
    # return new_dict

    # Optimized, with zip pairs are made of the list and a revered version of the list
    # new_dict = {}
    # for k, v in zip(l, reversed(l)):
    #     new_dict[list(k.keys())[0]] = list(v.values())[0]
    # return new_dict

    # Same as above, but more concise
    new_dict = {list(k.keys())[0]: list(v.values())[0] for k, v in zip(l, reversed(l))}
    return new_dict

def main():
    x = [{'Math':81}, {'Physics':83}, {'Chemistry':87}, {'English': 42}]
    print(switch_the_values(x))
    #[{'Math':42}, {'Physics':87}, {'Chemistry':83}, {'English': 81}]
    x = [{'a':'b'}, {'c':'d'}]
    print(switch_the_values(x))
    #[{'a':'d'}, {'c':'b'}]

if __name__ == '__main__':
    main()