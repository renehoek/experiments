namespace TheCardGame;

public struct CardConfig {
    public int attackValue;
    public int defenseValue;
    public int energyCost;

    public CardConfig(int attackValue, int defenseValue, int energyCost) {
        this.attackValue = attackValue;
        this.defenseValue = defenseValue;
        this.energyCost = energyCost;
    }
}

abstract class CardFactory
{
    public abstract LandCard createLandCard(string cardId, CardColour colour, int energyLevel);
    public abstract SorceryCard createSorceryCard(string cardId, CardColour colour, CardConfig cfg);
    public abstract CreatureCard createCreatureCard(string cardId, CardColour colour, CardConfig cfg);

    public abstract LandCard createLandCard(string cardId, CardColour colour, List<IEffect> effects, int energyLevel);
    public abstract SorceryCard createSorceryCard(string cardId, CardColour colour, List<IEffect> effects, CardConfig cfg);
    public abstract CreatureCard createCreatureCard(string cardId, CardColour colour, List<IEffect> effects, CardConfig cfg);
}


class DemoGameFactory : CardFactory
{
    public override LandCard createLandCard(string cardId, CardColour colour, int energyLevel) {
        return new DemoLandCard(cardId, colour, energyLevel);
    
    }
    public override SorceryCard createSorceryCard(string cardId, CardColour colour, CardConfig cfg) {
        DemoSorceryCard card = new DemoSorceryCard(cardId, colour);
        card.setEnergyCost(cfg.energyCost);
        return card;

    }
    public override CreatureCard createCreatureCard(string cardId, CardColour colour, CardConfig cfg) {
        DemoCreatureCard card = new DemoCreatureCard(cardId, colour, cfg.attackValue, cfg.defenseValue);
        card.setEnergyCost(cfg.energyCost);
        return card;
    }

    public override LandCard createLandCard(string cardId, CardColour colour, List<IEffect> effects, int energyLevel) {
        DemoLandCard card = new DemoLandCard(cardId, colour, energyLevel);
        card.effects = effects;
        return card;
                

    }
    public override SorceryCard createSorceryCard(string cardId, CardColour colour, List<IEffect> effects, CardConfig cfg) {
        DemoSorceryCard card = new DemoSorceryCard(cardId, colour);
        card.effects = effects;
        card.setEnergyCost(cfg.energyCost);
        return card;

    }
    public override CreatureCard createCreatureCard(string cardId, CardColour colour, List<IEffect> effects, CardConfig cfg) {
        DemoCreatureCard card = new DemoCreatureCard(cardId, colour, cfg.attackValue, cfg.defenseValue);
        card.effects = effects;
        card.setEnergyCost(cfg.energyCost);
        return card;
    }
}
