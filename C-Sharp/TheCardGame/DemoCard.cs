namespace TheCardGame;

class RemoveRandomCardWhenPlacedOnBoardEffect: IEffect {
    public void onPlacedOnBoard(Card sourcecard, Player currentTurnPlayer, Player opponentPlayer, GameBoard theBoard) {
        Card disposedCard = opponentPlayer.disposeRandomCard();
        System.Console.WriteLine($"{opponentPlayer.getName()} disposed random card: {disposedCard?.getId()}");
    }
}

class RemoveRandomCardWhenDrawnEffect: IEffect {
    public void onTookFromDeck(Card sourcecard, Player currentTurnPlayer, Player opponentPlayer, GameBoard theBoard) {
        Card disposedCard = opponentPlayer.disposeRandomCard();
        System.Console.WriteLine($"{opponentPlayer.getName()} disposed random card: {disposedCard?.getId()}");
    }
}

class OppositePlayerMayNotDrawCardOnceEffect: IEffect {
    private int placedOnBoardAtTurn;
    private int revokeAtTurn;
    private IDrawCardStrategy? previousStrategy = null;
    
    public void onPlacedOnBoard(Card sourcecard, Player currentTurnPlayer, Player opponentPlayer, GameBoard theBoard) {
        this.placedOnBoardAtTurn = theBoard.getCurrentTurn();
        this.revokeAtTurn = this.placedOnBoardAtTurn + 2;
        if (sourcecard.owner.getName() == currentTurnPlayer.getName()) {        
            this.previousStrategy = theBoard.setDrawCardStrategy(new DrawNoCardStategy(), opponentPlayer);
        } else {
            this.previousStrategy = theBoard.setDrawCardStrategy(new DrawNoCardStategy(), currentTurnPlayer);
        }
    }

    public void onNewTurn(Card sourcecard, Player currentTurnPlayer, Player opponentPlayer, GameBoard theBoard) {
        if (theBoard.getCurrentTurn() == this.revokeAtTurn) {
            IDrawCardStrategy restoreThisStrategy = this.previousStrategy is not null ? this.previousStrategy : new DrawCardStrategy();
            
            if (sourcecard.owner.getName() == currentTurnPlayer.getName()) {        
                theBoard.setDrawCardStrategy(restoreThisStrategy, opponentPlayer);
            } else {
                theBoard.setDrawCardStrategy(restoreThisStrategy, currentTurnPlayer);
            }
        }
    }
}

public class DemoLandCard : LandCard {
    public DemoLandCard(string cardId, CardColour colour, Player owner, int energyLevel): base(cardId, colour, owner, energyLevel) {}

}

public class DemoSorceryCard : SorceryCard {
    public DemoSorceryCard(string cardId, CardColour colour, Player owner): base(cardId, colour, owner) {

    }
}

public class DemoCreatureCard : CreatureCard {
    public DemoCreatureCard(string cardId, CardColour colour, Player owner, int attackValue, int defenseValue) : base(cardId, colour, owner, attackValue, defenseValue) {}

}