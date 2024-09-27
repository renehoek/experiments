class Car {
    public Car(string brand, int topSpeed, double trunkSpace) {
        this.brand = brand;
        this.topSpeed = topSpeed;
        this.trunkSpace = trunkSpace;
        this.trunkSpaceInUse = 0;
    }
    public string brand {get; set;}
    public int topSpeed {get;}
    public double trunkSpace {get;}
    public double trunkSpaceInUse {get; set;}
}