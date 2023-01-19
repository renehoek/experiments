namespace TheCardGame;

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