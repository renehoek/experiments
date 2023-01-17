namespace TheCardGame;
class Program
{
    static void setupPlayersAndCards() {
        DemoGameFactory factory = new DemoGameFactory();

        List<Card> player1_deck = new List<Card>();
        player1_deck.Add(factory.createSorceryCard("sorcery-1", new CardColour("red"), new CardConfig(0, 0, 3)));
        
        List<IEffect> effects = new List<IEffect>();
        effects.Add(new RemoveRandomCardEffect());        
        player1_deck.Add(factory.createSorceryCard("sorcery-2", new CardColour("blue"), effects, new CardConfig(0, 0, 3)));
       
        player1_deck.Add(factory.createSorceryCard("sorcery-3", new CardColour("gray"), new CardConfig(0, 0, 3)));                
        player1_deck.Add(factory.createLandCard("land-1", new CardColour("gray"), 15));
        player1_deck.Add(factory.createLandCard("land-2", new CardColour("red"), 10));
        player1_deck.Add(factory.createCreatureCard("creature-1", new CardColour("gray"), new CardConfig(5, 4, 3)));
        player1_deck.Add(factory.createCreatureCard("creature-2", new CardColour("red"), new CardConfig(4, 10, 3)));
        player1_deck.Add(factory.createSorceryCard("sorcery-7", new CardColour("red"), new CardConfig(0, 0, 3)));
        player1_deck.Add(factory.createSorceryCard("sorcery-8", new CardColour("blue"), new CardConfig(0, 0, 3)));
        player1_deck.Add(factory.createSorceryCard("sorcery-9", new CardColour("gray"), new CardConfig(0, 0, 3)));
        player1_deck.Add(factory.createLandCard("land-5", new CardColour("gray"), 10));
        player1_deck.Add(factory.createLandCard("land-6", new CardColour("red"), 11));
        player1_deck.Add(factory.createCreatureCard("creature-5", new CardColour("gray"), new CardConfig(10, 4, 3)));
        player1_deck.Add(factory.createCreatureCard("creature-6", new CardColour("red"), new CardConfig(10, 4, 3)));

        List<Card> player2_deck = new List<Card>();
        player2_deck.Add(factory.createSorceryCard("sorcery-4", new CardColour("red"), new CardConfig(0, 0, 3)));
        player2_deck.Add(factory.createSorceryCard("sorcery-5", new CardColour("blue"), new CardConfig(0, 0, 3)));
        player2_deck.Add(factory.createSorceryCard("sorcery-6", new CardColour("gray"), new CardConfig(0, 0, 3)));
        player2_deck.Add(factory.createLandCard("land-3", new CardColour("gray"), 8));
        player2_deck.Add(factory.createLandCard("land-4", new CardColour("red"), 12));
        player2_deck.Add(factory.createCreatureCard("creature-3", new CardColour("gray"), new CardConfig(6, 3, 3)));
        player2_deck.Add(factory.createCreatureCard("creature-4", new CardColour("red"), new CardConfig(4, 10, 3)));
        player2_deck.Add(factory.createSorceryCard("sorcery-10", new CardColour("red"), new CardConfig(0, 0, 3)));
        player2_deck.Add(factory.createSorceryCard("sorcery-11", new CardColour("blue"), new CardConfig(0, 0, 3)));
        player2_deck.Add(factory.createSorceryCard("sorcery-12", new CardColour("gray"), new CardConfig(0, 0, 3)));
        player2_deck.Add(factory.createLandCard("land-7", new CardColour("gray"), 8));
        player2_deck.Add(factory.createLandCard("land-8", new CardColour("red"), 9));
        player2_deck.Add(factory.createCreatureCard("creature-7", new CardColour("gray"), new CardConfig(10, 4, 3)));
        player2_deck.Add(factory.createCreatureCard("creature-8", new CardColour("red"), new CardConfig(10, 4, 3)));

        Player player1 = new Player("player1", 10, player1_deck);
        Player player2 = new Player("player2", 10, player2_deck);

        GameBoard gb = GameBoard.GetInstance();
        gb.setPlayers(player1, player2, player1);

    }

    static void setupACurrentSituation() {
        GameBoard gb = GameBoard.GetInstance();
        for(int cnt = 0; cnt < 6; cnt++) {
            gb.takeCard();
        }
        gb.endTurn();
        for(int cnt = 0; cnt < 6; cnt++) {
            gb.takeCard();
        }
        gb.endTurn();

    }
    

    static void RunADemoGame() {
        GameBoard gb = GameBoard.GetInstance();
        System.Console.WriteLine("== Starting the demo game!");

        //Player 1 - Turn 1
        gb.newTurn();
        gb.takeCard();
        gb.drawCard("land-1");
        gb.endTurn();
        gb.logCurrentSituation();

        //Player 2  - Turn 2
        gb.newTurn();
        gb.takeCard();
        gb.drawCard("land-3");
        gb.endTurn();
        gb.logCurrentSituation();

        //Player 1  - Turn 3
        gb.newTurn();
        gb.takeCard();
        gb.drawCard("creature-2");
        gb.endTurn();
        gb.logCurrentSituation();

        //Player 2  - Turn 4
        gb.newTurn();
        gb.tapFromCard("land-3");
        gb.drawCard("creature-4");
        gb.declareAttack("creature-4", new List<string>(){"creature-2"});
        gb.peformAttack("creature-4");
        gb.endTurn();
        gb.logCurrentSituation();

        //Player 1  - Turn 5
        gb.newTurn();
        gb.drawCard("creature-1");
        gb.declareAttack("creature-1");
        gb.peformAttack("creature-1");
        gb.endTurn();
        gb.logCurrentSituation();

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
