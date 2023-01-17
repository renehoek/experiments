namespace TheCardGame;

public class Player  {
    public List<Card> deck;
    public List<Card> discardPile;
    public List<Card> inHand;    
    private int healthValue;
    private string name = string.Empty;

    private List<string>? defenseCardIdsForDeclaredAttack;
    
    public Player(string name, int initialLife, List<Card> deck) {        
        this.deck = deck;
        this.discardPile = new List<Card>();
        this.inHand = new List<Card>();
        this.healthValue = initialLife;        
        this.name = name;
    }

    public string getName() {
        return this.name;
    }

    public void decreaseHealthValue(int iValue){
        this.healthValue -= iValue;
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
        CreatureCard? creatureCard = sender as CreatureCard;
        if (creatureCard is not null) {
            int iPrevHealth = this.getHealthValue();
            int iAttackValue = creatureCard.getAttackValue();
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
                (Card foundCard, int iPos) = Support.findCard(cardsOnBoard, cardId);
                CreatureCard? creatureDefenseCard = foundCard as CreatureCard;
                if (creatureDefenseCard is not null) {
                    defenseCards.Add(creatureDefenseCard);
                }
            }
        }

        CreatureCard? creatureCard = sender as CreatureCard;
        if (creatureCard is not null) {
            creatureCard.calculateActualAttackValue(defenseCards);
        }
    }

    /* Take the first card from his deck and put it in his hand */
    public Card? takeCard(){
        Card? cardTaken = Support.moveCard(this.deck, this.inHand, 0);
        return cardTaken;
    }


    /* Draw a card from his hand */
    public Card? drawCard(string cardId) {
        foreach(Card card in this.inHand) {
            if (card.getId() == cardId ) {
                Support.removeCard(this.inHand, card.getId());
                return card;
            }
        }
        return null;
    }


    public Card disposeRandomCard() {
        Random rd = new Random();
        int r_ = rd.Next(0, this.inHand.Count);
        Card cardDisposed = Support.moveCard(this.inHand, this.discardPile, r_);
        return cardDisposed;        
    }

    public void trimCards(int maxCards) {
        if (this.inHand.Count <= maxCards) {
            return;
        }
        
        bool stillToTrim = this.inHand.Count > maxCards;
        while (stillToTrim) {
            Support.moveCard(this.inHand, this.discardPile, 0);
            stillToTrim = this.inHand.Count > maxCards;
        }        
    }    
}
