namespace LogToDifferentTargets;



class Program
{
    static void Main(string[] args)
    {
        //Lets log to console.
        LogToConsole logger = new LogToConsole();
        
        //Lets log to file.
        //LogToFile logger = new LogToFile();

        Bookings bookings = new Bookings(logger);

        bookings.reserveRoom(5, "Thomas");
        bookings.reserveRoom(3, "Marie");
        bookings.reserveRoom(4, "Kees");
        
    }
}
