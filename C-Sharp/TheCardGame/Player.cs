namespace TheCardGame;

public class PlayerDiedEventArgs : EventArgs {
    private int _health;
    private string _reason;
    public PlayerDiedEventArgs(int health, string reason){
        this._health = health;
        this._reason = reason;
    }

    public int getHealth() {
        return this._health;
    }
    public string getReason() {
        return this._reason;
    }
}


public class Player  {
    public List<Card> deck;
    public List<Card> discardPile;
    public List<Card> inHand;    
    private int healthValue;
    private string name = string.Empty;

    private List<string>? defenseCardIdsForDeclaredAttack;
    
    public Player(string name, int initialLife) {        
        this.deck = new List<Card>();
        this.discardPile = new List<Card>();
        this.inHand = new List<Card>();
        this.healthValue = initialLife;        
        this.name = name;
    }

    public event EventHandler<PlayerDiedEventArgs> OnDied = delegate {};

    public void setDeck(List<Card> deck)  {
        this.deck = deck;
    }

    public string getName() {
        return this.name;
    }

    public void decreaseHealthValue(int iValue){
        this.healthValue -= iValue;
        if (this.healthValue <= 0) {
            this.OnDied(this, new PlayerDiedEventArgs(this.healthValue, "Health below or is zero"));
        }
    }    
    public int getHealthValue() {
        return this.healthValue;
    }

    public void setDefenseCards(List<string> defenseCardIds) {
        this.defenseCardIdsForDeclaredAttack = defenseCardIds;
    }
    public void resetDefenseCards() {
        this.defenseCardIdsForDeclaredAttack = null;
    }

    //Events to process
    public virtual void absorbAttack(object? sender, AttackEventArgs e) {
        CreatureCard? attackCard = sender as CreatureCard;
        if (attackCard is not null) {
            int iPrevHealth = this.getHealthValue();
            int iAttackValue = attackCard.getActualAttackValue();
            this.decreaseHealthValue(iAttackValue);
            System.Console.WriteLine($"Player {this.getName()} absorbed attack (prevHealth/AttackValue/currentHealth): {iPrevHealth}/{iAttackValue}/{this.getHealthValue()}");            
        }
    }

    public virtual void prepareDefense(object? sender, AttackEventArgs e) {
        
        List<CreatureCard> defenseCards = new List<CreatureCard>();
        GameBoard board = GameBoard.GetInstance();
        List<Card> cardsOnBoard = board.getCardsOnBoard(this);
        if (this.defenseCardIdsForDeclaredAttack is not null) {
            foreach(string cardId in this.defenseCardIdsForDeclaredAttack) {
                Card foundCard;
                int iPos;

                try {
                    (foundCard, iPos) = Support.findCard(cardsOnBoard, cardId);
                } catch (CardNotFoundException) {
                    continue;
                } 
                CreatureCard? creatureDefenseCard = foundCard as CreatureCard;
                if (creatureDefenseCard is not null) {
                    defenseCards.Add(creatureDefenseCard);
                }
            }
        }

        CreatureCard? attackCard = sender as CreatureCard;
        if (attackCard is not null) {
            attackCard.calculateActualAttackValue(defenseCards);
        }
    }

    /* Take the first card from his deck and put it in his hand */
    public Card? takeCard(){
        Card? cardTaken = Support.moveCard(this.deck, this.inHand, 0);

        if (cardTaken is null) {
            this.OnDied(this, new PlayerDiedEventArgs(this.healthValue, "No more cards in deck"));
        }
        return cardTaken;
    }


    /* Draw a card from his hand */
    public Card? drawCard(string cardId) {
        Card card;
        int iPos;
        
        try {
            (card, iPos) = Support.findCard(this.inHand, cardId);
        } catch (CardNotFoundException) {
            return null;
        }

        Support.removeCard(this.inHand, card.getId());
        return card;                        
    }


    public Card disposeRandomCard() {
        Random rd = new Random();
        int r_ = rd.Next(0, this.inHand.Count);
        GameBoard gb = GameBoard.GetInstance();
        Card cardDisposed = Support.moveCard(this.inHand, this.discardPile, r_);
        foreach(IEffect effect in cardDisposed.effects) {
            effect.onDisposed(cardDisposed, gb.getCurrentTurnPlayer(), gb.getOpponentPlayer(), gb);
        }
        return cardDisposed;        
    }

    public void trimCards(int maxCards) {
        if (this.inHand.Count <= maxCards) {
            System.Console.WriteLine($"{this.getName()} trimmed 0 cards into discard pile.");
            return;
        }
        
        GameBoard gb = GameBoard.GetInstance();
        bool stillToTrim = this.inHand.Count > maxCards;
        int cnt = 0;
        while (stillToTrim) {
            Card cardDisposed = Support.moveCard(this.inHand, this.discardPile, 0);                        
            cnt++;
            foreach(IEffect effect in cardDisposed.effects) {
                effect.onDisposed(cardDisposed, gb.getCurrentTurnPlayer(), gb.getOpponentPlayer(), gb);
            }
            stillToTrim = this.inHand.Count > maxCards;
        }        
        System.Console.WriteLine($"{this.getName()} trimmed {cnt} cards into discard pile.");
    }    
}
