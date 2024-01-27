# This is a sample Python script.

# Onderstaande variabele is gedeclareerd op global scope. Dat nooit doen, is een bron van bugs
this_variable_is_declared_on_global_scope_and_will_cause_bugs = []

def main():
    # Jouw programma start hier. Alle variabelen die je nodig hebt vanaf
    # hier declareren.
    print('Hello World')

    my_grocery_list = ["bread", "milk", "coffee"]
    print(my_grocery_list)


if __name__ == '__main__':
    main()
