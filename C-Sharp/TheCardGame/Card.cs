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

public interface IDetermineAttackValue {
    public void calculateActualAttackValue(List<CreatureCard> defenseCards);
}


public abstract class Card {
    public CardColour colour;    
    public List<IEffect> effects;

    private int energyCost = 0; // The amount of energy required to play this card.
    private string description; 
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

    public int getEnergyCost(){
        return this.energyCost;
    }

    public void setEnergyCost(int energyCost) {
        this.energyCost = energyCost;
    }

    public virtual bool canBePlayed(int iAvailEnergyLevel) {
        bool bResult = this.energyCost <= iAvailEnergyLevel;
        if (!bResult) {
            System.Console.WriteLine($"Card {this.cardId} can't be played: (energyNeeded/energyTapped) {this.getEnergyCost()}/{iAvailEnergyLevel}");
        }
        return bResult;
    }
}

public abstract class LandCard : Card
{
    /* Provides the energy to play the other cards */
    private int _energyLevel = 0;
    private bool _energyInUse = false;

    public LandCard(string cardId, CardColour colour, int energyLevel) : base(cardId, colour)
    {
        this._energyLevel = energyLevel;
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

public abstract class CreatureCard : Card, IDetermineAttackValue {
    /* Used to attack opponenent (decrease opponent lifePoint) or for defense.
    
    */
    private int attackValue = 0; /* The attackValue defined on this card*/
    private int actualAttackValue = 0; /* The attackValue for this attack after defense cards came into action */
    private int defenseValue = 0;
    private bool useAsDefenseForDeclaredAttack = false;
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
        this.actualAttackValue = this.attackValue;
        System.Console.WriteLine($"{this.getId()} Declared attack.");
        this.OnDeclareAttack(this, new AttackEventArgs(av));
    }

    public void doPeformAttack() {
        AttackValue av = new AttackValue(this.attackValue);
        System.Console.WriteLine($"{this.getId()} Peforms attack.");
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

    public void decreaseDefenseValue(int iNumber) {
        System.Console.WriteLine($"Card {this.getId()} (oldDefenseValue/decrease/newDefenseValue) {this.defenseValue}/{iNumber}/{this.defenseValue - iNumber}");
        
        this.defenseValue -= iNumber;
        if (this.defenseValue <= 0) {
            this.doDefenseExhausted();
        }
    }

    public void decreaseActualAttackValue(int iNumber) {
        this.actualAttackValue -= iNumber;
    }

    public virtual void calculateActualAttackValue(List<CreatureCard> defenseCards) {
        if (defenseCards.Count == 0) {
            this.actualAttackValue = this.attackValue;
        }
        
        foreach(CreatureCard defenseCard in defenseCards) {
            System.Console.WriteLine($"Card {defenseCard.getId()} is defending with value {defenseCard.getAttackValue()}, card {this.getId()} is attacking with value {this.attackValue}.");
            defenseCard.decreaseDefenseValue(this.attackValue);            
            this.decreaseDefenseValue(defenseCard.getAttackValue());
            //Once a defense has taken place, the impact on the attacked player is 0.
            this.actualAttackValue = 0;
        }
    }

    public int getAttackValue() {return this.attackValue;}
    public int getActualAttackValue() {return this.actualAttackValue;}
    public int getDefenseValue() {return this.defenseValue;}
}



public class CardEventArgs : EventArgs {
    public List<IEffect> effects {get; set;}
    public CardEventArgs(List<IEffect> effects){
        this.effects = effects;
    }
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

