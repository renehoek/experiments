namespace OrderFullFillment;

public class AwsResponseMessageQueue : IInputQueue {    
    public void receive() {System.Console.WriteLine("Receiving with Aws");}
}

public class AwsMessageQueue : IOutputQueue {    
    public void send() { System.Console.WriteLine("Sending with Aws"); }
}

class AmazonConnection : OrderShipmentProcessor {
    protected override IInputQueue getInputQueue()
    {
        return new AwsResponseMessageQueue();
    }

    protected override IOutputQueue getOutputQueue()
    {
        return new AwsMessageQueue();
    }

}