namespace TheCardGame;

abstract class CardFactory
{
    public abstract LandCard createLandCard(string cardId, CardColour colour);
    public abstract SorceryCard createSorceryCard(string cardId, CardColour colour);
    public abstract CreatureCard createCreatureCard(string cardId, CardColour colour, int attackValue, int defenseValue);

    public abstract LandCard createLandCard(string cardId, CardColour colour, List<IEffect> effects);
    public abstract SorceryCard createSorceryCard(string cardId, CardColour colour, List<IEffect> effects);
    public abstract CreatureCard createCreatureCard(string cardId, CardColour colour, List<IEffect> effects, int attackValue, int defenseValue);
}


class DemoGameFactory : CardFactory
{
    public override LandCard createLandCard(string cardId, CardColour colour) {
        return new DemoLandCard(cardId, colour);

    }
    public override SorceryCard createSorceryCard(string cardId, CardColour colour) {
        return new DemoSorceryCard(cardId, colour);

    }
    public override CreatureCard createCreatureCard(string cardId, CardColour colour, int attackValue, int defenseValue) {
        return new DemoCreatureCard(cardId, colour, attackValue, defenseValue);

    }

    public override LandCard createLandCard(string cardId, CardColour colour, List<IEffect> effects) {
        DemoLandCard card = new DemoLandCard(cardId, colour);
        card.effects = effects;
        return card;
                

    }
    public override SorceryCard createSorceryCard(string cardId, CardColour colour, List<IEffect> effects) {
        DemoSorceryCard card = new DemoSorceryCard(cardId, colour);
        card.effects = effects;
        return card;

    }
    public override CreatureCard createCreatureCard(string cardId, CardColour colour, List<IEffect> effects, int attackValue, int defenseValue) {
        DemoCreatureCard card = new DemoCreatureCard(cardId, colour, attackValue, defenseValue);
        card.effects = effects;
        return card;
    }
}
