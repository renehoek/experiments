namespace OrderFullFillment;

public class AzureResponseMessageQueue : IInputQueue {
    /* This is a concrete class the concrete Azure factory will create */
    public void receive() {System.Console.WriteLine("Receiving with Azure");}
}

public class AzureMessageQueue : IOutputQueue {
    /* This is a concrete class the concrete Azure factory will create */
    public void send() { System.Console.WriteLine("Sending with Azure"); }
}


public class AzureServiceBusFactory : AbstractConnectionFactory {
    /* This is the concrete factory for the Azure web service */

    public override IInputQueue createInputQueue(string name) {
        return new AzureResponseMessageQueue();

    }
    public override IOutputQueue createOutputQueue(string name) {
        return new AzureMessageQueue();

    }
}