namespace TheCardGame;

public struct CardConfig {
    public int attackValue;
    public int defenseValue;
    public int energyCost;
    public int energyLevel;
    public Player owner;

    public CardConfig(int[] values, Player owner) {
        this.attackValue = values[0];
        this.defenseValue = values[1];
        this.energyCost = values[2];
        this.energyLevel = values[3];
        this.owner = owner;
    }
}

abstract class CardFactory
{
    public abstract LandCard createLandCard(string cardId, CardColour colour, CardConfig cfg);
    public abstract SorceryCard createSorceryCard(string cardId, CardColour colour, CardConfig cfg);
    public abstract CreatureCard createCreatureCard(string cardId, CardColour colour, CardConfig cfg);

    public abstract LandCard createLandCard(string cardId, CardColour colour, List<IEffect> effects, CardConfig cfg);
    public abstract SorceryCard createSorceryCard(string cardId, CardColour colour, List<IEffect> effects, CardConfig cfg);
    public abstract CreatureCard createCreatureCard(string cardId, CardColour colour, List<IEffect> effects, CardConfig cfg);
}


class DemoGameFactory : CardFactory
{
    public override LandCard createLandCard(string cardId, CardColour colour, CardConfig cfg) {
        DemoLandCard card = new DemoLandCard(cardId, colour, cfg.owner, cfg.energyLevel);
        return card;
    
    }
    public override SorceryCard createSorceryCard(string cardId, CardColour colour, CardConfig cfg) {
        DemoSorceryCard card = new DemoSorceryCard(cardId, colour, cfg.owner);
        card.setEnergyCost(cfg.energyCost);
        return card;

    }
    public override CreatureCard createCreatureCard(string cardId, CardColour colour, CardConfig cfg) {
        DemoCreatureCard card = new DemoCreatureCard(cardId, colour, cfg.owner, cfg.attackValue, cfg.defenseValue);
        card.setEnergyCost(cfg.energyCost);
        return card;
    }

    public override LandCard createLandCard(string cardId, CardColour colour, List<IEffect> effects, CardConfig cfg) {
        DemoLandCard card = new DemoLandCard(cardId, colour, cfg.owner, cfg.energyLevel);
        card.effects = effects;
        return card;
                

    }
    public override SorceryCard createSorceryCard(string cardId, CardColour colour, List<IEffect> effects, CardConfig cfg) {
        DemoSorceryCard card = new DemoSorceryCard(cardId, colour, cfg.owner);
        card.effects = effects;
        card.setEnergyCost(cfg.energyCost);
        return card;

    }
    public override CreatureCard createCreatureCard(string cardId, CardColour colour, List<IEffect> effects, CardConfig cfg) {
        DemoCreatureCard card = new DemoCreatureCard(cardId, colour, cfg.owner, cfg.attackValue, cfg.defenseValue);
        card.effects = effects;
        card.setEnergyCost(cfg.energyCost);
        return card;
    }
}
