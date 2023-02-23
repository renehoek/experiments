namespace OrderFullFillment;

public interface IInputQueue {
    /* This is one of the interfaces which the abstract factory will return */
    public void receive();
}

public interface IOutputQueue {
    /* This is one of the interfaces which the abstract factory will return */
    public void send();
}

public abstract class AbstractConnectionFactory {
    /* This is the abstract factory. It declares two method signatues which each return a interface. 
    The two returned interfaces will belong to the same 'familily' of related objects. */
    public abstract IInputQueue createInputQueue(string name);
    public abstract IOutputQueue createOutputQueue(string name);
}

class OrderShipmentProcessor {
    /* This is the client. It is only aware of the abstract factory and the interfaces the factory returns 
    So it is possible to introduce new concrete factories which will create concrete objects which this client can use 
    without touching the code of this client */
    
    IInputQueue inputQueue;
    IOutputQueue outputQueue;

    public OrderShipmentProcessor(AbstractConnectionFactory acf) {
        this.inputQueue = acf.createInputQueue("queue_1");
        this.outputQueue = acf.createOutputQueue("queue_2");
    }

    public void makeOrder() {
        this.outputQueue.send();
        this.inputQueue.receive();
    }

}

class Program
{
    static void Main(string[] args)
    {
        OrderShipmentProcessor osp1 = new OrderShipmentProcessor(new AzureServiceBusFactory());
        osp1.makeOrder();

        OrderShipmentProcessor osp2 = new OrderShipmentProcessor(new AwsServiceBusFactory());
        osp2.makeOrder();
        
    }
}
