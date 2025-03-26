def create_pass_list(l: dict):
    cleaned_list = list(filter(lambda x: x[1][1] == "Pass", l.items()))
    cleaned_list = [x[0] for x in cleaned_list]
    return cleaned_list

def create_fail_list(l: dict):
    cleaned_list = list(filter(lambda x: x[1][1] == "Fail", l.items()))
    cleaned_list = [x[0] for x in cleaned_list]
    return cleaned_list

def get_pass_and_fail_lists(l: dict):
    pass_list = create_pass_list(l)
    fail_list = create_fail_list(l)

    return {'Pass': pass_list, 'Fail': fail_list}


def main():
    x = {'Math': (81,"Pass"), 'Physics': (50,"Fail"), 'Chemistry': (90,"Pass"), 'English': (42,"Fail")}
    print(create_pass_list(x))
    #["Math", "Chemistry"]
    print(get_pass_and_fail_lists(x))

    x2 = {'Soccer': (9, "Pass"), 'Snowboarding': (3, "Fail"), 'Tennis': (7, "Pass")}
    print(create_pass_list(x2))
    print(get_pass_and_fail_lists(x2))


if __name__ == '__main__':
    main()