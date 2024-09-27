using System.ComponentModel.DataAnnotations;

interface IList<C> {
    public void Add(C t);
    public C? Next();
}

class Node<C> {
    public Node<C>? next;
    public C? obj;

    public Node(C obj) {
        this.next = null;
        this.obj = obj;
    }
}

class LinkedList<C> : IList<C> {
    private Node<C>? head; //The start of the linked list
    private Node<C>? current; //Holds the current node

    public LinkedList() {
        this.head = null;
        this.current = null;
    }

    public void Add(C t) {
        if (this.head is null) { //If head is null, the list is empty
            // Add our first inital Node
            this.head = new Node<C>(t);
        } else {
            // Add items to the end of the list
            Node<C> cur_node = this.head;
            while (cur_node.next != null) { // While there is a Node behind us.
                // Jump to that Node.
                cur_node = cur_node.next;
            }
            // Add the last Node of the list, so set the 'next' to the new Node.
            cur_node.next = new Node<C>(t);
        }
    }

    public C? Get() {
        return this.current != null ? current.obj : default(C);
    }
    public C? Next() {
        /* Returns the Node behind this current Node */

        if (this.head is null) { // If head is null, the list is empty
            return default(C); // Return the default value for C in that case.
        }

        if (this.current is null) { // If current is not set  
            this.current = this.head; // set it to the start of the list
        } else {
            this.current = this.current.next; // else set it to the Node after this one 
        }

        if (this.current is null) { // If current node is null
            return default(C); // return the default value of C in that case
        } else {
            return this.current.obj; // else return the object in that Node
        }
    }

    public void GotoStart(){
        this.current = this.head;
    }

    public R? Find<R, T>(Func<C, T, R?> searchCriteria, T searchFor) {
        this.GotoStart();
        C? obj = this.Get();
        while (obj != null) {
            R? r = searchCriteria(obj, searchFor);
            if ( !(r == null)) {
                return r;
            }
            obj = this.Next();
        }
        return default(R);
    }

    public void Modify(Action<C> action) {
        this.GotoStart();
        C? obj = this.Get();
        while (obj != null) {
            action(obj);
            obj = this.Next();
        }
    }
}