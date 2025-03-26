using System.Xml;

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

    public void Delete(int value) {                
        this.root = this.Delete(value, this.root);
    }

    private Node? Delete(int value, Node? node) {
        if (node == null) {
            return null;
        }
        if (value < node.value) {
            node.left = this.Delete(value, node.left);
        } else if (value > node.value) {
            node.right = this.Delete(value, node.right);
        } else if (node.value == value) { // Node gevonden
            if (node.left == null) {
                return node.right;
            }
            if (node.right == null) {
                return node.left;
            }

            // Node met twee kinderen
            Node? nodeSuccessor = FindSuccessor(node);
            node.value = nodeSuccessor.value;

            // Verwijder de opvolger (kleinste waarde rechts)
            node.right = this.Delete(nodeSuccessor.value, node.right);
        }
        return node;
        
        
    }
    private Node? FindSuccessor(Node node) {
        return this.FindSuccessorRecur(node.right);
    }
    private Node? FindSuccessorRecur(Node node) {        
        
        if (node.left == null) {
            return node;
        } else {
            return this.FindSuccessorRecur(node.left);
        }
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

    public void WalkInOrder() {
        this.WalkInOrder(this.root);
    }

    private void WalkInOrder(Node? node) {
        if (node == null) {
            return;
        }
        WalkInOrder(node.left);
        System.Console.WriteLine(node.value);
        WalkInOrder(node.right);
    }

    public void PrintAsciiTree()
    {
        PrintAsciiTree(this.root, "", "root");
    }

    private void PrintAsciiTree(Node node, string indent, string position)
    {
        if (node != null)
        {
            // Eerst rechterkant printen, zodat links lager staat
            PrintAsciiTree(node.right, indent + "     ", "R");

            // Print huidige node
            Console.WriteLine($"{indent}[{position}]-- {node.value}");

            // Daarna linkerkant printen
            PrintAsciiTree(node.left, indent + "     ", "L");
        }
    }
}

class Program
{
    static void Main(string[] args)
    {
        Tree tree = new Tree();
        tree.Insert(20);
        tree.Insert(10);
        tree.Insert(30);
        tree.Insert(25);
        tree.Insert(35);
        tree.Insert(27);
        tree.Insert(3);

        tree.PrintAsciiTree();
        tree.WalkInOrder();
        
        tree.Delete(25);

        System.Console.WriteLine("\nAfter delete, Walk in order:");
        tree.PrintAsciiTree();
        tree.WalkInOrder();

        List<int> findValues = new List<int>{8, 6, 10, 9};

        foreach(int value in findValues) {
            bool hasValue = tree.HasValue(value);
            System.Console.WriteLine($"Tree has value {value}: {hasValue}");
        }

        
    }
}
