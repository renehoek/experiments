namespace LinkedList;

    
class Program
{
    static Car? FindWithSpeed(Car car, int topSpeed) {
        if (car.topSpeed == topSpeed) {
            return car;
        } else {
            return null;
        }
    }

    static void FillTrunkSpace(Car car) {
        car.trunkSpaceInUse = car.trunkSpace * 0.5;
    }

    static void Main(string[] args)
    {
        LinkedList<Car> my_list = new LinkedList<Car>();
        my_list.Add(new Car("Opel", 170, 215.3));
        my_list.Add(new Car("Toyota", 200, 180.0));
        my_list.Add(new Car("VW", 130, 200.3));

        Func<Car, int, Car?> SearchFor = FindWithSpeed;  

        Car? foundCar = my_list.Find<Car, int>(SearchFor, 200);
        if (foundCar != null) {
            Console.WriteLine($"The foundcar is: {foundCar.brand}");
        } else {
            Console.WriteLine("Could not find a car");
        }

        Action<Car> SetInUse = Program.FillTrunkSpace;
        my_list.Modify(SetInUse);

        my_list.GotoStart();
        Car? myCar = my_list.Get();
        while(myCar != null) {
            Console.WriteLine($"{myCar.brand} Trunk space avail {myCar.trunkSpaceInUse} of {myCar.trunkSpace}");
            myCar = my_list.Next();
        }
    }
}
