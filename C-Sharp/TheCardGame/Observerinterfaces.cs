namespace TheCardGame;
public interface IObserver
{
    // Receive update from subject
    void Update(ISubject subject, string eventname);
}

public interface ISubject
{
    // Attach an observer to the subject.
    void Attach(IObserver observer);

    // Detach an observer from the subject.
    void Detach(IObserver observer);

    // Notify all observers about an event.
    void Notify(string eventname);
}