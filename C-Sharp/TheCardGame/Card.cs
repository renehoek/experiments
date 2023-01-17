namespace TheCardGame;

public struct CardColour {
    public CardColour(string colourName) {
        this.colourname = colourName;
    }

    public string colourname { get; }
}


public interface IEffect {
    
    public void onPlacedOnBoard(Card sourcecard, Player currentTurnPlayer, Player opponentPlayer, GameBoard theBoard) {}
    public void onAttack(Card sourcecard, Player currentTurnPlayer, Player opponentPlayer, GameBoard theBoard) {}
    public void onDisposed(Card sourcecard, Player currentTurnPlayer, Player opponentPlayer, GameBoard theBoard) {}
}



public abstract class Card {
    public CardColour colour;    
    public List<IEffect> effects;

    int energyCost = 0; // The amount of energy required to play this card.
    string description; 
    private string cardId; /* The unique id of this card in the game. */

    public event EventHandler<CardEventArgs> OnIsSummoned = delegate { };

    public Card(string cardId, CardColour colour) {
        this.colour = colour;
        this.cardId = cardId;
        this.effects = new List<IEffect>();
        this.description = string.Empty;
    }

    public string getId() {
        return this.cardId;
    }

    // public virtual void IsSummoned() {
    //     OnIsSummoned.Invoke(this, new CardEventArgs(this.effects));

    // }
}

public abstract class LandCard : Card
{
    /* Provides the energy to play the other cards */
    private int _energyLevel = 0;
    private bool _energyInUse = false;

    public LandCard(string cardId, CardColour colour) : base(cardId, colour)
    {
    
    }

    public int tapEnergy(){
        this._energyInUse = true;
        return this._energyLevel;
    }
    public void unTapEnergy(){
        this._energyInUse = false;
    }

    public bool isTapped(){
        return this._energyInUse;
    }

    public int energyLevel() {
        return this._energyLevel;
    }

    
}

public abstract class SorceryCard : Card
{

    public SorceryCard(string cardId, CardColour colour) : base(cardId, colour)
    {
       
    }

    enum Spelllifetime {
        Instantaneous,
        Permanent
    };
    Spelllifetime duration;

}

public abstract class CreatureCard : Card {
    /* Used to attack opponenent (decrease opponent lifePoint) or for defense.
    
    */
    int attackValue = 0;
    int defenseValue = 0;
    bool useAsDefenseForDeclaredAttack = false;
    public CreatureCard(string cardId, CardColour colour, int attackValue, int defenseValue) : base(cardId, colour)
    {
        this.attackValue = attackValue;
        this.defenseValue = defenseValue;
    }

    public event EventHandler<AttackEventArgs> OnDeclareAttack = delegate {};
    public event EventHandler<AttackEventArgs> OnPeformAttack = delegate {};
    public event EventHandler<DefenseExhaustedArgs> OnDefenseExhausted = delegate {};

    public void doDeclareAttack() {
        AttackValue av = new AttackValue(this.attackValue);
        this.OnDeclareAttack(this, new AttackEventArgs(av));
    }

    public virtual void calculateActualAttackValue(List<CreatureCard> defenseCards) {
        
    }

    public void doPeformAttack() {
        AttackValue av = new AttackValue(this.attackValue);
        this.OnPeformAttack(this, new AttackEventArgs(av));
    }

    public void doDefenseExhausted() {
        this.OnDefenseExhausted(this, new DefenseExhaustedArgs());
    }

    public void useAsDefense(){
        this.useAsDefenseForDeclaredAttack = true;
    }

    public bool isDefender() {
        return this.useAsDefenseForDeclaredAttack;
    }

    public void resetIsDefender() {
        this.useAsDefenseForDeclaredAttack = false;
    }

    public int getAttackValue() {return this.attackValue;}
    public int getDefenseValue() {return this.defenseValue;}
}


public class AttackValue {
    private int _attackValue;
    public AttackValue(int initialValue) {
        this._attackValue = initialValue;
    }
    public void alterValue(int alterValue) {
        this._attackValue = this._attackValue + alterValue;
    }
    public int getValue() {
        return this._attackValue;
    }
}

public class AttackEventArgs : EventArgs {
    private AttackValue _attackValue;
    public AttackEventArgs(AttackValue attackValue){
        this._attackValue = attackValue;
    }
}

public class DefenseExhaustedArgs: EventArgs {}

