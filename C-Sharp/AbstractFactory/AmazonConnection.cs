namespace OrderFullFillment;

public class AwsResponseMessageQueue : IInputQueue {
    /* This is a concrete class the concreate AWS factory will create */
    public void receive() {System.Console.WriteLine("Receiving with Aws");}
}

public class AwsMessageQueue : IOutputQueue {
    /* This is a concrete class the concreate AWS factory will create */
    public void send() { System.Console.WriteLine("Sending with Aws"); }
}


public class AwsServiceBusFactory : AbstractConnectionFactory {
    /* This is the concrete factory for the Amazon web service */

    public override IInputQueue createInputQueue(string name) {
        return new AwsResponseMessageQueue();

    }
    public override IOutputQueue createOutputQueue(string name) {
        return new AwsMessageQueue();

    }
}