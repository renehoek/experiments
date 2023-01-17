namespace TheCardGame;

class RemoveRandomCardEffect: IEffect {
    public void onPlacedOnBoard(Card sourcecard, Player currentTurnPlayer, Player opponentPlayer, GameBoard theBoard) {
        Card disposedCard = opponentPlayer.disposeRandomCard();
        System.Console.WriteLine($"{opponentPlayer.getName()} disposed random card: {disposedCard?.getId()}");
    }
}


public class DemoLandCard : LandCard {
    public DemoLandCard(string cardId, CardColour colour, int energyLevel): base(cardId, colour, energyLevel) {}

}

public class DemoSorceryCard : SorceryCard {
    public DemoSorceryCard(string cardId, CardColour colour): base(cardId, colour) {

    }
}

public class DemoCreatureCard : CreatureCard {
    public DemoCreatureCard(string cardId, CardColour colour, int attackValue, int defenseValue) : base(cardId, colour, attackValue, defenseValue) {}

}