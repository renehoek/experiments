namespace PartsList;
using System;
using System.Collections.Generic;

public class Sports
{
   private string[] types = { "Baseball", "Basketball", "Football",
                              "Hockey", "Soccer", "Tennis",
                              "Volleyball" };

   public string this[int i]
   {
      get => types[i];
      set => types[i] = value;
   }
}

public class SortByName : IComparer<Part>
{
    public enum SortOrder {
        asc,
        desc
    }

    private SortOrder _sortorder = SortOrder.asc;
    public SortByName() {}
    public SortByName(SortOrder sorting) {
        this._sortorder = sorting;
    }

    public static IComparer<Part> SortByNameAsc() {
        return new SortByName(SortByName.SortOrder.asc);
    }

    public static IComparer<Part> SortByNameDesc() {
        return new SortByName(SortByName.SortOrder.desc);
    }

    public int Compare(Part? x, Part? y)
    {
        if (x is null && y is null) {
            return 0;
        }
        
        if (x is null) {return (this._sortorder == SortOrder.asc ? 1 : -1); };
        if (y is null) {return (this._sortorder == SortOrder.asc ? -1 : 1); };
        
        int rvalue = x.PartName.CompareTo(y.PartName);
        return (this._sortorder == SortOrder.asc ? rvalue : -rvalue);
    }
}


// Simple business object. A PartId is used to identify the type of part
// but the part name can change.
public class Part : IEquatable<Part>, IComparable<Part> {        
    public string PartName { get; set; } = string.Empty;
    public int PartId { get; set; }

    public bool onStock {get; set;} = false; 

    public Part(string name, int partId) {                       
        this.PartName = name;
        this.PartId = partId;
    }

    public override string ToString()
    {            
        return "ID: " + PartId.ToString() + "   Name: " + PartName;
    }

    public int CompareTo(Part? other) {
        /* Implementation of the IComparable Interface This is the default sorter */
        if (other is null) {
            return 1;
        }
        return this.PartId.CompareTo(other.PartId);
    }

    public bool Equals(Part? other) {
    /* Implementation of the IEquatable Interface */    
        if (other is null) return false;
        return (this.PartId.Equals(other.PartId));
    }

   
    
    public override bool Equals(object? obj)
    /* Base class override */
    {
        if (obj is null) return false;
        if (obj is not Part) return false;

        Part? objAsPart = obj as Part;
        if (objAsPart is null) {
            return false;
        }
        else { 
            return Equals(objAsPart);
        }
    }
    public override int GetHashCode()
    {
        return PartId;
    }
   

    public static bool operator ==(Part left, Part right){
        return left.Equals(right);
    }
    public static bool operator !=(Part left, Part right){
        return !(left.Equals(right));
    }

}



public class Example
{
    public static void Main()
    {
        // Create a list of parts.
        List<Part> parts = new List<Part>();

        // Add parts to the list.
        parts.Add(new Part("crank arm", 1334));
        parts.Add(new Part("chain ring", 1234));
        parts.Add(new Part("regular seat", 1434));
        parts.Add(new Part("banana seat", 1444));
        parts.Add(new Part("cassette", 1634));
        parts.Add(new Part("shift lever", 1534));

        // Write out the parts in the list. This will call the overridden ToString method
        // in the Part class.
        Console.WriteLine();
        foreach (Part aPart in parts)
        {
            Console.WriteLine(aPart);
        }

        // Check the list for part #1734. This calls the IEquatable.Equals method
        // of the Part class, which checks the PartId for equality.
        Console.WriteLine("\nContains(\"1734\"): {0}",
        parts.Contains(new Part("", 1734)));

        // Insert a new item at position 2.
        Console.WriteLine("\nInsert(2, \"1834\")");
        parts.Insert(2, new Part("brake lever", 1834));

        //Console.WriteLine();
        foreach (Part aPart in parts)
        {
            Console.WriteLine(aPart);
        }

        Console.WriteLine("\nParts[3]: {0}", parts[3]);

        Console.WriteLine("\nRemove(\"1534\")");

        // This will remove part 1534 even though the PartName is different,
        // because the Equals method only checks PartId for equality.
        parts.Remove(new Part("cogs", 1534));

        Console.WriteLine();
        foreach (Part aPart in parts)
        {
            Console.WriteLine(aPart);
        }
        Console.WriteLine("\nRemoveAt(3)");
        // This will remove the part at index 3.
        parts.RemoveAt(3);

        Console.WriteLine();
        foreach (Part aPart in parts)
        {
            Console.WriteLine(aPart);
        }


        Part p1 = new Part("test", 45);           
        Part p2 = new Part("testxx", 45);

        Console.WriteLine(p1 == p2); 
        
        parts.Sort();
        Console.WriteLine();
        foreach (Part aPart in parts)
        {
            Console.WriteLine(aPart);
        }

        parts.Sort(SortByName.SortByNameAsc());
        Console.WriteLine();
        foreach (Part aPart in parts)
        {
            Console.WriteLine(aPart);
        }

        parts.Sort(SortByName.SortByNameDesc());
        Console.WriteLine();
        foreach (Part aPart in parts)
        {
            Console.WriteLine(aPart);
        }

        
        Sports s = new Sports();
        Console.WriteLine(s[0]);
        Console.WriteLine(s[3]);
    }
}
