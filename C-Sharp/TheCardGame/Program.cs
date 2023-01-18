namespace TheCardGame;
class Program
{
    static void setupPlayersAndCards() {

        Player player1 = new Player("player1", 10);
        Player player2 = new Player("player2", 10);

        CardConfig cfg_ec03_p1 = new CardConfig(new int[] {0, 0, 3, 0}, player1);
        CardConfig cfg_el09_p1 = new CardConfig(new int[] {0, 0, 0, 9}, player1);
        CardConfig cfg_el10_p1 = new CardConfig(new int[] {0, 0, 0, 10}, player1);
        CardConfig cfg_el11_p1 = new CardConfig(new int[] {0, 0, 0, 11}, player1);
        CardConfig cfg_a04d10ec3_p1 = new CardConfig(new int[] {4, 10, 3, 0}, player1);
        CardConfig cfg_a05d04ec3_p1 = new CardConfig(new int[] {5, 4, 3, 0}, player1);
        CardConfig cfg_a10d04ec3_p1 = new CardConfig(new int[] {10, 4, 3, 0}, player1);

        CardConfig cfg_ec03_p2 = new CardConfig(new int[] {0, 0, 3, 0}, player2);
        CardConfig cfg_el08_p2 = new CardConfig(new int[] {0, 0, 0, 8}, player2);
        CardConfig cfg_el09_p2 = new CardConfig(new int[] {0, 0, 0, 9}, player2);
        CardConfig cfg_el10_p2 = new CardConfig(new int[] {0, 0, 0, 10}, player2);
        CardConfig cfg_el11_p2 = new CardConfig(new int[] {0, 0, 0, 11}, player2);
        CardConfig cfg_el12_p2 = new CardConfig(new int[] {0, 0, 0, 12}, player2);
        CardConfig cfg_a03d07ec3_p2 = new CardConfig(new int[] {4, 7, 3, 0}, player2);
        CardConfig cfg_a04d10ec3_p2 = new CardConfig(new int[] {4, 10, 3, 0}, player2);
        CardConfig cfg_a05d04ec3_p2 = new CardConfig(new int[] {5, 4, 3, 0}, player2);
        CardConfig cfg_a06d03ec3_p2 = new CardConfig(new int[] {6, 3, 3, 0}, player2);
        CardConfig cfg_a10d04ec3_p2 = new CardConfig(new int[] {10, 4, 3, 0}, player2);
        
        DemoGameFactory factory = new DemoGameFactory();

        List<Card> player1_deck = new List<Card>();
        player1_deck.Add(factory.createSorceryCard("sorcery-1", new CardColour("red"), cfg_ec03_p1));
        
        List<IEffect> effects = new List<IEffect>();
        effects.Add(new RemoveRandomCardWhenDrawnEffect());

        List<IEffect> effects1 = new List<IEffect>();
        effects1.Add(new OppositePlayerMayNotDrawCardOnceEffect());

        player1_deck.Add(factory.createSorceryCard("sorcery-2", new CardColour("blue"), cfg_ec03_p1));       
        player1_deck.Add(factory.createSorceryCard("sorcery-3", new CardColour("gray"), cfg_ec03_p1));                
        player1_deck.Add(factory.createLandCard("land-1", new CardColour("gray"), cfg_el09_p1));
        player1_deck.Add(factory.createLandCard("land-2", new CardColour("red"), cfg_el10_p1));
        player1_deck.Add(factory.createCreatureCard("creature-1", new CardColour("gray"), cfg_a05d04ec3_p1));
        player1_deck.Add(factory.createCreatureCard("creature-2", new CardColour("red"), cfg_a04d10ec3_p1));
        player1_deck.Add(factory.createSorceryCard("sorcery-7", new CardColour("red"), effects, cfg_ec03_p1));
        player1_deck.Add(factory.createSorceryCard("sorcery-8", new CardColour("blue"), cfg_ec03_p1));
        player1_deck.Add(factory.createSorceryCard("sorcery-9", new CardColour("gray"), cfg_ec03_p1));
        player1_deck.Add(factory.createLandCard("land-5", new CardColour("gray"), cfg_el10_p1));
        player1_deck.Add(factory.createLandCard("land-6", new CardColour("red"), cfg_el11_p1));
        player1_deck.Add(factory.createCreatureCard("creature-5", new CardColour("gray"), cfg_a10d04ec3_p1));
        player1_deck.Add(factory.createCreatureCard("creature-6", new CardColour("red"), cfg_a10d04ec3_p1));

        List<Card> player2_deck = new List<Card>();
        player2_deck.Add(factory.createSorceryCard("sorcery-4", new CardColour("red"), cfg_ec03_p2));
        player2_deck.Add(factory.createSorceryCard("sorcery-5", new CardColour("blue"), cfg_ec03_p2));
        player2_deck.Add(factory.createSorceryCard("sorcery-6", new CardColour("gray"), cfg_ec03_p2));
        player2_deck.Add(factory.createLandCard("land-3", new CardColour("gray"), effects1, cfg_el08_p2));
        player2_deck.Add(factory.createLandCard("land-4", new CardColour("red"), cfg_el12_p2));
        player2_deck.Add(factory.createCreatureCard("creature-3", new CardColour("gray"), cfg_a03d07ec3_p2));
        player2_deck.Add(factory.createCreatureCard("creature-4", new CardColour("red"), cfg_a04d10ec3_p2));
        player2_deck.Add(factory.createSorceryCard("sorcery-10", new CardColour("red"), cfg_ec03_p2));
        player2_deck.Add(factory.createSorceryCard("sorcery-11", new CardColour("blue"), cfg_ec03_p2));
        player2_deck.Add(factory.createSorceryCard("sorcery-12", new CardColour("gray"), cfg_ec03_p2));
        player2_deck.Add(factory.createLandCard("land-7", new CardColour("gray"), cfg_el08_p2));
        player2_deck.Add(factory.createLandCard("land-8", new CardColour("red"), cfg_el09_p2));
        player2_deck.Add(factory.createCreatureCard("creature-7", new CardColour("gray"), cfg_a10d04ec3_p2));
        player2_deck.Add(factory.createCreatureCard("creature-8", new CardColour("red"), cfg_a10d04ec3_p2));

        player1.deck = player1_deck;
        player2.deck = player2_deck;

        GameBoard gb = GameBoard.GetInstance();
        gb.setPlayers(player1, player2, player1);

    }

    static void setupACurrentSituation() {
        GameBoard gb = GameBoard.GetInstance();
        gb.setupACurrentSituation();        
    }
    

    static void RunADemoGame() {
        GameBoard gb = GameBoard.GetInstance();
        System.Console.WriteLine("== Starting the demo game!");

        //Player 1 - Turn 1        
        gb.newTurn();
        System.Console.WriteLine($"New Turn: {gb.getCurrentTurn()}");        
        gb.drawCard("land-1");
        gb.logCurrentSituation();       
        gb.endTurn();
       

        //Player 2  - Turn 2
        gb.newTurn();
        System.Console.WriteLine($"New Turn: {gb.getCurrentTurn()}");        
        gb.drawCard("land-3");
        gb.logCurrentSituation();
        gb.endTurn();
        

        //Player 1  - Turn 3
        gb.newTurn();
        System.Console.WriteLine($"New Turn: {gb.getCurrentTurn()}");        
        gb.drawCard("creature-2");
        gb.logCurrentSituation();
        gb.endTurn();
        

        //Player 2  - Turn 4
        gb.newTurn();
        System.Console.WriteLine($"New Turn: {gb.getCurrentTurn()}");
        gb.tapFromCard("land-3");
        gb.drawCard("creature-4");
        gb.declareAttack("creature-4", new List<string>(){"creature-2"});
        gb.peformAttack("creature-4");
        gb.logCurrentSituation();
        gb.endTurn();
        

        //Player 1  - Turn 5
        gb.newTurn();
        System.Console.WriteLine($"New Turn: {gb.getCurrentTurn()}");
        gb.drawCard("creature-1");
        gb.declareAttack("creature-1");
        gb.peformAttack("creature-1");
        gb.logCurrentSituation();
        gb.endTurn();
        

    }
    
    static void Main(string[] args)
    {
       setupPlayersAndCards();
       setupACurrentSituation();
       GameBoard gb = GameBoard.GetInstance();
       gb.logCurrentSituation();

       RunADemoGame();
    
    }
}
