namespace TheCardGame;


public class CardEventArgs : EventArgs {
    public List<IEffect> effects {get; set;}
    public CardEventArgs(List<IEffect> effects){
        this.effects = effects;
    }
}

