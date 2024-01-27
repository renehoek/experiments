namespace HelloWorld;


class Program
{
    static void WriteHello(string name) {
        DateTime currentDate = DateTime.Now;
        Console.WriteLine($"{Environment.NewLine} Welcome {name} at this beautiful day: {currentDate:d} at {currentDate:t}");
    }

    static void Main(string[] args)
    {
        Console.WriteLine("Hello, please enter your name");
        string myName = Console.ReadLine() ?? "Unknown";

        WriteHello(myName);
        WriteHello("Rene");
        WriteHello(Convert.ToString(5));

       
        Console.Write($"{Environment.NewLine} Press any key to continue");
        Console.ReadKey(true);

    }
}
