namespace Sheet9Implementation;

class Part {
    private string _name;
    public Part(string name) {
        this._name = name;
    }

    public string getName() {
        return this._name;
    }

}

class Element: Part {
    public Element(string name) : base(name) {

    }

}

class Component: Part {
    private List<Part> _parts;

    public Component(string name) : base(name) {
        this._parts = new List<Part>();
    }

    public void addPart(Part p) {
        this._parts.Add(p);
    }
    public void printParts(){
        foreach(Part p in this._parts) {
            Console.WriteLine(p.getName());    
        }
    }
}


class Program
{
    static void Main(string[] args)
    {

        Component x = new Component("Component x");
        Component y = new Component("Component y");
        Element e = new Element("Element e");
        x.addPart(e);
        y.addPart(x);
        y.addPart(e);

        x.printParts();
        Console.WriteLine("===");
        y.printParts();
    }
}
