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

    private ITakeCardFromDeckStrategy takeCardFromDeckStategy;
    private IDrawCardStrategy drawCardStrategy_player1;
    private IDrawCardStrategy drawCardStrategy_player2;

    private int iTurnCnt;
    private bool gameEnded;

    protected GameBoard() {              
        this.player1_cards = new List<Card>();
        this.player2_cards = new List<Card>();

        this.player1 = new Player("dummy1", 0);
        this.player2 = new Player("dummy2", 0);
        this.currentTurnPlayer = this.player1;
        this.opponentPlayer = this.player2;
        this.currentTurnPlayerCards = new List<Card>();
        this.opponentPlayerCards = new List<Card>();
        this.iTurnCnt = 0;
        this.gameEnded = false;

        this.takeCardFromDeckStategy = new TakeFirstCardStrategy();
        this.drawCardStrategy_player1 = new DrawCardStrategy();
        this.drawCardStrategy_player2 = new DrawCardStrategy();
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

        this.player1.OnDied += this.playerDied;
        this.player2.OnDied += this.playerDied;

    }

    public void setTakeCardStrategy(ITakeCardFromDeckStrategy s) {
        this.takeCardFromDeckStategy = s;
    }

    /* Sets the DrawCard strategy to use. For convenience it returns the strategy previously in effect.*/
    public IDrawCardStrategy? setDrawCardStrategy(IDrawCardStrategy s, Player p)  {
        IDrawCardStrategy? currentStrategy = null;
        if (p.getName() == this.player1.getName()) {
            currentStrategy = this.drawCardStrategy_player1;
            this.drawCardStrategy_player1 = s;
        } else if (p.getName() == this.player2.getName()) {
            currentStrategy = this.drawCardStrategy_player2;
            this.drawCardStrategy_player2 = s;
        }
        return currentStrategy;
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

    public bool newTurn() {
        if (this.gameEnded) {
            return false;
        }
        this.iTurnCnt++;

        foreach(Card card in this.currentTurnPlayerCards) {
            foreach(IEffect effect in card.effects) {
                effect.onNewTurn(card, this.currentTurnPlayer, this.opponentPlayer, this);
            }
        }
        foreach(Card card in this.opponentPlayerCards) {
            foreach(IEffect effect in card.effects) {
                effect.onNewTurn(card, this.currentTurnPlayer, this.opponentPlayer, this);
            }
        }

        this.takeCardFromDeckStategy.takeCard(this);        
        return true;
    }

    public void endTurn() {

        this.opponentPlayer.resetDefenseCards();
        this.unTapFromAll();
        this.currentTurnPlayer.trimCards(7);

        foreach(Card card in this.currentTurnPlayerCards) {
            foreach(IEffect effect in card.effects) {
                effect.onEndTurn(card, this.currentTurnPlayer, this.opponentPlayer, this);
            }
        }
        foreach(Card card in this.opponentPlayerCards) {
            foreach(IEffect effect in card.effects) {
                effect.onEndTurn(card, this.currentTurnPlayer, this.opponentPlayer, this);
            }
        }

        this.SwapPlayer();
    }

    /* returns the current cards on the board for the specififed player */
    public List<Card> getCardsOnBoard(Player player) {
        if (player.getName() == this.opponentPlayer.getName()) {
            return this.opponentPlayerCards;
        } else {
            return this.currentTurnPlayerCards;
        }
    }

    public Player getCurrentTurnPlayer() {
        return this.currentTurnPlayer;
    }
    public Player getOpponentPlayer() {
        return this.opponentPlayer;
    }
    public int getCurrentTurn() {
        return this.iTurnCnt;
    }
    
    public bool drawCard(string cardId) {
        if (this.currentTurnPlayer.getName() == this.player1.getName()) {
            return this.drawCardStrategy_player1.drawCard(this, cardId);
        } else if (this.currentTurnPlayer.getName() == this.player2.getName()) {
            return this.drawCardStrategy_player2.drawCard(this, cardId);
        } else {
            return false;
        }
    }

    /* Returns the card-ids which can be used in a attack regarding the avaiable energy obtained from the land-cards. */
    public List<string> energyLevelSufficientForCards() {
        int iAvailEnergyLevel = this.energyTapped();
        List<string> cardIds = new List<string>();
        foreach(Card card in this.currentTurnPlayerCards) {
            if (card is LandCard) {
                continue;
            }
            if (card.canBePlayed(iAvailEnergyLevel)) {
                cardIds.Add(card.getId());
            }
        }
        return cardIds;
    }

    public bool declareAttack(string cardId, List<string> opponentDefenseCardIds) {
        this.opponentPlayer.setDefenseCards(opponentDefenseCardIds);
        return this.declareAttack(cardId);
    }

    public bool declareAttack(string cardId) {
        Card card;
        int iPos;
        try {
            (card, iPos) = Support.findCard(this.currentTurnPlayerCards, cardId);
        } catch (CardNotFoundException) {
            System.Console.WriteLine($"Player {this.currentTurnPlayer.getName()} could not declare attack with card {cardId}. Not on the board.");
            return false;
        }

        CreatureCard? creatureCard = card as CreatureCard;
        this.logEnergyTapped();

        bool attackDeclared = false;
        if (creatureCard is not null) {
            if (creatureCard.canBePlayed(this.energyTapped())) {                                
                creatureCard.doDeclareAttack();
                attackDeclared = true;
            }            
        } 

        return attackDeclared;
    }

    public bool peformAttack(string cardId) {
        Card? card;
        int iPos;
        try {
            (card, iPos) = Support.findCard(this.currentTurnPlayerCards, cardId);
        } catch (CardNotFoundException) {
            System.Console.WriteLine($"Player {this.currentTurnPlayer.getName()} could not peform attack with card {cardId}. Not on the board.");
            card = null;
            iPos = -1;
        }
        
        CreatureCard? creatureCard = card as CreatureCard;
        bool attackDone = false;
        if (creatureCard is not null) {
            this.logEnergyTapped();

            if (creatureCard.canBePlayed(this.energyTapped())) { 
                creatureCard.doPeformAttack();
                foreach(IEffect effect in creatureCard.effects) {
                    effect.onAttack(creatureCard, this.currentTurnPlayer, this.opponentPlayer, this);
                }
                attackDone = true;
            }
        } 

        return attackDone;
    }

    /* Tap Energry from a land-card currently on the board 
    Returns the energy-level tapped.*/
    public int tapFromCard(string cardId) {
        Card cardFound;
        int iPos;
        int iTappedEnergy = 0;

        try {
            (cardFound, iPos) = Support.findCard(this.currentTurnPlayerCards, cardId);
        } catch (CardNotFoundException) {
            System.Console.WriteLine($"Player {this.currentTurnPlayer.getName()} could not tap from card {cardId}. Not on the board.");
            return 0;
        }
        
        LandCard? landCardFound = cardFound as LandCard;
        if (landCardFound is not null && !landCardFound.isTapped()) {
            iTappedEnergy = landCardFound.tapEnergy();            
        } else {
            iTappedEnergy = 0;
        }        
        System.Console.WriteLine($"Player {this.currentTurnPlayer.getName()} tapped from card {cardId} energy: {iTappedEnergy}");
        return iTappedEnergy;
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
            Player owner = creatureCard.owner;
            string cardId = creatureCard.getId();
            System.Console.WriteLine($"{cardId} defense exchausted.");
            if (owner.getName()== this.opponentPlayer.getName()) {
                Support.moveCard(this.opponentPlayerCards, this.opponentPlayer.discardPile, cardId);
            } else if (owner.getName()== this.currentTurnPlayer.getName()) {
                Support.moveCard(this.currentTurnPlayerCards, this.currentTurnPlayer.discardPile, cardId);        
            }
            foreach(IEffect effect in creatureCard.effects) {
                effect.onDisposed(creatureCard, this.currentTurnPlayer, this.opponentPlayer, this);
            }
        }
    }

    public void playerDied(object? sender, PlayerDiedEventArgs e) {
        Player? player = sender as Player;
        if (player is not null) {
            player.OnDied -= this.playerDied;
            System.Console.WriteLine($"Player {player.getName()} died. Health: {e.getHealth()}, {e.getReason()}");
            if (player.getName() == this.player1.getName()) {
                System.Console.WriteLine($"Player {this.player2.getName()} is the winner!");
            } else {
                System.Console.WriteLine($"Player {this.player1.getName()} is the winner!");
            }
            this.gameEnded = true;
        }
    }

    /* These are methods just for Demo stuff */
    public void setupACurrentSituation() {
        
        for(int cnt = 0; cnt < 6; cnt++) {
            this.player1.takeCard();            
        }
        for(int cnt = 0; cnt < 6; cnt++) {
            this.player2.takeCard();            
        }
    }

    public void logEnergyTapped() {
        System.Console.WriteLine($"Current turn-player: {this.currentTurnPlayer.getName()}, energy-tapped: {this.energyTapped()}");
    }

    public void logCurrentSituation() {
        System.Console.WriteLine("==== Current situation");
        System.Console.WriteLine($"Current turn-player: {this.currentTurnPlayer.getName()}, Turn: {this.iTurnCnt}, energy-tapped: {this.energyTapped()}");
        System.Console.WriteLine($"Player {this.player1.getName()}: Health: {this.player1.getHealthValue()}");
        System.Console.WriteLine($"Player {this.player2.getName()}: Health: {this.player2.getHealthValue()}");

        System.Console.WriteLine($"Player {this.player1.getName()}: (ontheboard/indeck/inhand/indiscard-pile) {this.player1_cards.Count}/{this.player1.deck.Count}/{this.player1.inHand.Count}/{this.player1.discardPile.Count}");        
        System.Console.WriteLine($"Player {this.player1.getName()} on the board: " + Support.CardIdsHumanFormatted(this.player1_cards));
        System.Console.WriteLine($"Player {this.player1.getName()} in deck: " + Support.CardIdsHumanFormatted(this.player1.deck));        
        System.Console.WriteLine($"Player {this.player1.getName()} in hand: " + Support.CardIdsHumanFormatted(this.player1.inHand));        
        System.Console.WriteLine($"Player {this.player1.getName()} on the discard-pile: " + Support.CardIdsHumanFormatted(this.player1.discardPile));

        System.Console.WriteLine($"Player {this.player2.getName()}: (ontheboard/indeck/inhand/indiscard-pile) {this.player2_cards.Count}/{this.player2.deck.Count}/{this.player2.inHand.Count}/{this.player2.discardPile.Count}");
        System.Console.WriteLine($"Player {this.player2.getName()} on the board: " + Support.CardIdsHumanFormatted(this.player2_cards));
        System.Console.WriteLine($"Player {this.player2.getName()} in deck: " + Support.CardIdsHumanFormatted(this.player2.deck));        
        System.Console.WriteLine($"Player {this.player2.getName()} in hand: " + Support.CardIdsHumanFormatted(this.player2.inHand));        
        System.Console.WriteLine($"Player {this.player2.getName()} on the discard-pile: " + Support.CardIdsHumanFormatted(this.player2.discardPile));
                
        System.Console.WriteLine("==== END Current situation");
    }
}
