namespace OrderFullFillment;

public class AzureResponseMessageQueue : IInputQueue {    
    public void receive() {System.Console.WriteLine("Receiving with Azure");}
}

public class AzureMessageQueue : IOutputQueue {    
    public void send() { System.Console.WriteLine("Sending with Azure"); }
}

class AzureConnection : OrderShipmentProcessor {
    protected override IInputQueue getInputQueue()
    {
        return new AzureResponseMessageQueue();
    }

    protected override IOutputQueue getOutputQueue()
    {
        return new AzureMessageQueue();
    }

}