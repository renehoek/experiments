namespace BinaryTree;

public class Node {
    public Node? right {get; set;}
    public Node? left {get; set;}
    public int value {get; set;}

    public Node(int value) {
        this.right = null;
        this.left = null;
        this.value = value;
    }
} 

public class Tree {
    public Node? root = null;

    public void Insert(int value) {
        this.root = this.Insert(value, this.root);
    }

    private Node Insert(int value, Node? node) {
        if (node == null) {
            Node n = new Node(value);
            return n;
        }
        if (value < node.value) {
            node.left = Insert(value, node.left);
        }
        else if (value > node.value) {
            node.right = Insert(value, node.right);
        }
        return node;
    }

    public bool HasValue(int value) {
        return this.HasValue(value, this.root);
    }

    private bool HasValue(int value, Node? node) {
        if (node == null) {
            return false;
        }
        if (node.value == value) {
            return true;
        }
        
        if (value < node.value ) {
            return this.HasValue(value, node.left);
        } else if (value > node.value) {
            return this.HasValue(value, node.right);
        } else {
            return false;
        }        
    }
}

class Program
{
    static void Main(string[] args)
    {
        Tree tree = new Tree();
        tree.Insert(8);
        tree.Insert(6);
        tree.Insert(10);
        tree.Insert(9);

        List<int> findValues = new List<int>
        {
            2,
            10,
            6,
            5
        };

        foreach(int value in findValues) {
            bool hasValue = tree.HasValue(value);
            System.Console.WriteLine($"Tree has value {value}: {hasValue}");
        }
    }
}
