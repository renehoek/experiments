namespace TheCardGame;

public interface ITakeCardFromDeckStrategy {
    public abstract bool takeCard(GameBoard gs);
}

public interface IDrawCardStrategy {
    public abstract bool drawCard(GameBoard gs, string cardId);
}

class TakeFirstCardStrategy : ITakeCardFromDeckStrategy {
    public bool takeCard(GameBoard gs) {    
        Card? card = gs.getCurrentTurnPlayer().takeCard();
        if (card == null) {
            System.Console.WriteLine($"{gs.getCurrentTurnPlayer().getName()} could not take card.");
            return false;
        } else {            
            System.Console.WriteLine($"{gs.getCurrentTurnPlayer().getName()} took card {card.getId()} from deck into hand.");

            foreach(IEffect effect in card.effects) {
                effect.onTookFromDeck(card, gs.getCurrentTurnPlayer(), gs.getOpponentPlayer(), gs);
            }
            return true;
        }
    }
}

class TakeNoCardStrategy: ITakeCardFromDeckStrategy {
    public bool takeCard(GameBoard gs) { 
        System.Console.WriteLine($"{gs.getCurrentTurnPlayer().getName()} Won't take a card.");
        return false;
    }
}

class DrawCardStrategy: IDrawCardStrategy {
    public bool drawCard(GameBoard gs, string cardId) {
        Player currentTurnPlayer = gs.getCurrentTurnPlayer();
        Player opponentPlayer = gs.getOpponentPlayer();
        Card? card = currentTurnPlayer.drawCard(cardId);
        if (card is null) {
            System.Console.WriteLine($"{currentTurnPlayer.getName()} could not draw card.");
            return false;
        } 
        
        gs.getCardsOnBoard(currentTurnPlayer).Add(card);        
        System.Console.WriteLine($"{currentTurnPlayer.getName()} draw card {card.getId()}.");
        foreach(IEffect effect in card.effects) {
            effect.onPlacedOnBoard(card, currentTurnPlayer, opponentPlayer, gs);
        }

        CreatureCard? creatureCard = card as CreatureCard;
        if (creatureCard is not null) {
            creatureCard.OnDeclareAttack += opponentPlayer.prepareDefense;
            creatureCard.OnPeformAttack += opponentPlayer.absorbAttack;
            creatureCard.OnDefenseExhausted += gs.defenseExhausted;
        }

        return true;
    }
}

class DrawNoCardStategy: IDrawCardStrategy {
    public bool drawCard(GameBoard gs, string cardId) {
        System.Console.WriteLine($"{gs.getCurrentTurnPlayer().getName()} Won't draw a card.");
        return false;
    }
}
