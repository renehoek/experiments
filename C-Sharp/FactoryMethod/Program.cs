namespace OrderFullFillment;

public interface IInputQueue {    
    public void receive();
}

public interface IOutputQueue {    
    public void send();
}

abstract class OrderShipmentProcessor {
    /* This is the client. It is only aware of the two interfaces returned by the two 'get...Queue' methods implemented
    in derived classes. So it is possible to make new classes who inherit from this base class and they will 
    create and return concrete objects through the overriden method. This client can use these without 
    further touching the code of this client */
   
    protected abstract IInputQueue getInputQueue();
    protected abstract IOutputQueue getOutputQueue();

    private IInputQueue inputQueue;
    private IOutputQueue outputQueue;

    public OrderShipmentProcessor() {        
        this.inputQueue = this.getInputQueue();
        this.outputQueue = this.getOutputQueue();
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
        
        AmazonConnection aws = new AmazonConnection();
        aws.makeOrder();

        AzureConnection azure = new AzureConnection();
        azure.makeOrder();
        
    }
}
