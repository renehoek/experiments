namespace HelloWorld;
class Program
{
    static void Main(string[] args)
    {
        Console.WriteLine("Hello, please enter your name");
        String? myName = Console.ReadLine();
        DateTime currentDate = DateTime.Now;
        Console.WriteLine($"{Environment.NewLine} Welcome {myName} at this beautiful day: {currentDate:d} at {currentDate:t}");

        Console.Write($"{Environment.NewLine} Press any key to continue");
        Console.ReadKey(true);

    }
}
