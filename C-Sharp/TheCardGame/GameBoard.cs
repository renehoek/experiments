namespace TheCardGame;



public class GameBoard {
    private static GameBoard? instance;
    private static object locker = new object();
    private Player player1;
    private Player player2;
    private Player currentTurnPlayer;
    private Player opponentPlayer;

    private List<Card> player1_cards;
    private List<Card> player2_cards;
    private List<Card> currentTurnPlayerCards;
    private List<Card> opponentPlayerCards;

    protected GameBoard() {              
        this.player1_cards = new List<Card>();
        this.player2_cards = new List<Card>();

        this.player1 = new Player("dummy1", 0, new List<Card>());
        this.player2 = new Player("dummy2", 0, new List<Card>());
        this.currentTurnPlayer = this.player1;
        this.opponentPlayer = this.player2;
        this.currentTurnPlayerCards = new List<Card>();
        this.opponentPlayerCards = new List<Card>();
    }

    public static GameBoard GetInstance()
    {
        if (instance == null)
        {
            lock (locker)
            {
                if (instance == null)
                {
                    instance = new GameBoard();
                }
            }
        }
        return instance;
    }

    public void setPlayers(Player player1, Player player2, Player currentTurnPlayer){
        this.player1 = player1;
        this.player2 = player2;
        if (this.player1.getName() == this.player2.getName()) {
            throw new System.InvalidOperationException("The two players should have a unique name.");
        }
        this.currentTurnPlayer = currentTurnPlayer;
        if (currentTurnPlayer.getName() == player1.getName()) {
            this.currentTurnPlayerCards = this.player1_cards;
            this.opponentPlayer = this.player2;            
            this.opponentPlayerCards = this.player2_cards;
        } else {
            this.currentTurnPlayerCards = this.player2_cards;
            this.opponentPlayer = this.player1;
            this.opponentPlayerCards = this.player1_cards;            
        }
    }

    private void SwapPlayer() {
        if (this.currentTurnPlayer.getName() == this.player1.getName()) {
            this.currentTurnPlayer = this.player2;
            this.opponentPlayer = this.player1;
            this.currentTurnPlayerCards = this.player2_cards;
            this.opponentPlayerCards = this.player1_cards;
        } else {
            this.currentTurnPlayer = this.player1;
            this.opponentPlayer = this.player2;
            this.currentTurnPlayerCards = this.player1_cards;
            this.opponentPlayerCards = this.player2_cards;
        }
    }

    public void newTurn() {

    }

    public void endTurn() {

        this.SwapPlayer();
    }

    /* returns the current cards on the board for the specififed player
    The returned list is *not* a deep-copy! */
    public List<Card> getCardsOnBoard(Player player) {
        if (player.getName() == this.opponentPlayer.getName()) {
            return this.opponentPlayerCards;
        } else {
            return this.currentTurnPlayerCards;
        }
    }

    public bool takeCard() {
        
        Card? card = this.currentTurnPlayer.takeCard();
        if (card == null) {
            System.Console.WriteLine($"{this.currentTurnPlayer.getName()} could not take card.");
            return false;
        } else {            
            System.Console.WriteLine($"{this.currentTurnPlayer.getName()} took card {card.getId()} from deck into hand.");
            return true;
        }
    }

    public bool drawCard(string cardId) {
        Card? card = this.currentTurnPlayer.drawCard(cardId);
        if (card is null) {
            System.Console.WriteLine($"{this.currentTurnPlayer.getName()} could not draw card.");
            return false;
        } 
        
        this.currentTurnPlayerCards.Add(card);
        System.Console.WriteLine($"{this.currentTurnPlayer.getName()} draw card {card.getId()}.");
        foreach(IEffect effect in card.effects) {
            effect.onPlacedOnBoard(card, this.currentTurnPlayer, this.opponentPlayer, this);
        }

        CreatureCard? creatureCard = card as CreatureCard;
        if (creatureCard is not null) {
            creatureCard.OnDeclareAttack += this.opponentPlayer.prepareDefense;
            creatureCard.OnPeformAttack += this.opponentPlayer.absorbAttack;
            creatureCard.OnDefenseExhausted += this.defenseExhausted;
        }

        return true;
        
    }

    public bool declareAttack(string cardId, List<string> opponentDefenseCardIds) {
        (Card? card, int iPos) = Support.findCard(this.currentTurnPlayerCards, cardId);
        CreatureCard? creatureCard = card as CreatureCard;
        if (creatureCard is not null) {
            this.opponentPlayer.setDefenseCards(opponentDefenseCardIds);
            creatureCard.doDeclareAttack();
            return true;
        } else {
            return false;
        }
    }
    public bool peformAttack(string cardId) {
        Card? card;
        int iPos;
        try {
            (card, iPos) = Support.findCard(this.currentTurnPlayerCards, cardId);
        } catch (CardNotFoundException) {
            card = null;
            iPos = -1;
        }
        
        CreatureCard? creatureCard = card as CreatureCard;
        bool attackDone = false;
        if (creatureCard is not null) {
            creatureCard.doPeformAttack();
            attackDone = true;
        } 

        this.opponentPlayer.resetDefenseCards();
        return attackDone;
    }

    /* Tap Energry from a land-card currently on the board 
    Returns the energy-level tapped.*/
    public int tapFromCard(string cardId) {
        Card cardFound;
        int iPos;

        try {
            (cardFound, iPos) = Support.findCard(this.currentTurnPlayerCards, cardId);
        } catch (CardNotFoundException) {
            return 0;
        }
        
        LandCard? landCardFound = cardFound as LandCard;
        if (landCardFound is not null && !landCardFound.isTapped()) {
            return landCardFound.tapEnergy();            
        } else {
            return 0;
        }        
    }

    public void unTapFromAll() {
        foreach(Card card in this.currentTurnPlayerCards) {            
            LandCard? landCard = card as LandCard;
            if (landCard is not null && landCard.isTapped()) {
                landCard.unTapEnergy();
            }            
        }
    }

    public int energyTapped(){
        int iSumEnergy = 0;
        foreach(Card card in this.currentTurnPlayerCards) {            
            LandCard? landCard = card as LandCard;
            if (landCard is not null && landCard.isTapped()) {
                iSumEnergy += landCard.energyLevel();
            }            
        }
        return iSumEnergy;
    }

    public void defenseExhausted(object? sender, DefenseExhaustedArgs e) {

        CreatureCard? creatureCard = sender as CreatureCard;
        if (creatureCard is not null) {
            creatureCard.OnDeclareAttack -= this.opponentPlayer.prepareDefense;
            creatureCard.OnPeformAttack -= this.opponentPlayer.absorbAttack;
            creatureCard.OnDefenseExhausted -= this.defenseExhausted;
            string cardId = creatureCard.getId();
            System.Console.WriteLine($"{cardId} defense exchausted.");
            if (Support.isOnList(this.opponentPlayerCards, cardId)) {
                Support.moveCard(this.opponentPlayerCards, this.opponentPlayer.discardPile, cardId);
            } else if (Support.isOnList(this.currentTurnPlayerCards, cardId)) {
                Support.moveCard(this.currentTurnPlayerCards, this.currentTurnPlayer.discardPile, cardId);        
            }
        }
    }

    public void logCurrentSituation() {
        System.Console.WriteLine("==== Current situation");
        System.Console.WriteLine($"Current turn-player: {this.currentTurnPlayer?.getName()}");
        System.Console.WriteLine($"Player {this.player1.getName()}: Health: {this.player1.getHealthValue()}");
        System.Console.WriteLine($"Player {this.player2.getName()}: Health: {this.player2.getHealthValue()}");

        System.Console.WriteLine($"Player {this.player1.getName()}: (indeck/inhand/inpile) {this.player1.deck.Count}/{this.player1.inHand.Count}/{this.player1.discardPile.Count}");
        System.Console.WriteLine($"Player {this.player2.getName()}: (indeck/inhand/inpile) {this.player2.deck.Count}/{this.player2.inHand.Count}/{this.player2.discardPile.Count}");
        
        System.Console.Write($"Player {this.player1.getName()} in hand: ");
        foreach(Card card in this.player1.inHand) {
            System.Console.Write($"{card.getId()}, ");
        }
        System.Console.Write("\n");

        System.Console.Write($"Player {this.player1.getName()} on the board: ");
        foreach(Card card in this.player1_cards) {
            System.Console.Write($"{card.getId()}, ");
        }        
        System.Console.Write("\n");
        System.Console.Write($"Player {this.player1.getName()} on the discard-pile: ");
        foreach(Card card in this.player1.discardPile) {
            System.Console.Write($"{card.getId()}, ");
        }        
        System.Console.Write("\n");

        System.Console.Write($"Player {this.player2?.getName()} in hand: ");
        foreach(Card card in this.player2.inHand) {
            System.Console.Write($"{card.getId()}, ");
        } 
        System.Console.Write("\n");

        System.Console.Write($"Player {this.player2.getName()} on the board: ");
        foreach(Card card in this.player2_cards) {
            System.Console.Write($"{card.getId()}, ");
        }        
        System.Console.Write("\n");
        System.Console.Write($"Player {this.player2.getName()} on the discard-pile: ");
        foreach(Card card in this.player2.discardPile) {
            System.Console.Write($"{card.getId()}, ");
        }        
        System.Console.Write("\n");
        System.Console.WriteLine("==== END Current situation");
    }


   
}
