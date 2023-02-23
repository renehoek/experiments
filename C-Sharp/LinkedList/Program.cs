namespace LinkedList;

interface IList<T> {
    public void Add(T t);
    public T? Next();
}

class LinkedList<T> : IList<T> {
    private Node? head;
    private Node? current;

    class Node {
        public Node? next;
        public T? obj;

        public Node(T obj) {
            this.next = null;
            this.obj = obj;
        }
    }
    
    public LinkedList() {
        this.head = null;
        this.current = null;
    }

    public void Add(T t) {
        if (this.head is null) {
            this.head = new Node(t);
        } else {
            bool atTheEnd = false;
            Node cur_node = this.head;
            while (!atTheEnd) {
                if (cur_node.next is null) {
                    cur_node.next = new Node(t);
                    atTheEnd = true;
                } else {
                    cur_node = cur_node.next;
                }
            }
        }
    }

    public T? Next() {
        if (this.head is null) {
            return default(T);
        }

        if (this.current is null) {
            this.current = this.head;
        } else {
            this.current = this.current.next;
        }

        if (this.current is null) {
            return default(T);
        } else {
            return this.current.obj;
        }
    }
    
}
    



class Car {
    public Car(string brand) {
        this.brand = brand;
    }
    public string brand {get; set;}
}

class Program
{
    static void Main(string[] args)
    {
        LinkedList<Car> my_list = new LinkedList<Car>();
        my_list.Add(new Car("Toyota"));
        my_list.Add(new Car("Opel"));
        my_list.Add(new Car("VW"));  


        bool hasCar = true;
        while (hasCar is true) {
            Car? car = my_list.Next();
            if (car is not null) {
                Console.WriteLine(car.brand);
            } else {
                Console.WriteLine("No more cars, at the end of the list");
                hasCar = false;
            }
        }
        
    }
}
