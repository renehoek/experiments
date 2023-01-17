namespace TheCardGame;

class CardNotFoundException : System.Exception {
    public CardNotFoundException(){}
    public CardNotFoundException(string message) : base(message){}
    public CardNotFoundException(string message, Exception inner) : base(message, inner){}
};

static class Support {
    /*Moves the card at index iPos from source to the target list. 
    For convenience it returns the Card instance moved. */
    static public Card moveCard(List<Card> sourceList, List<Card> targetList, int iPos) {
        if (sourceList.Count <= iPos) {
            throw new IndexOutOfRangeException("iPos is out of range");
        }

        Card card = sourceList[iPos];
        targetList.Add(card);
        sourceList.RemoveAt(iPos);
        return card;
    }

    /* Moves the card with the cardId from source to the target list.
    For convenience it returns the Card instance moved. */
    static public Card moveCard(List<Card> sourceList, List<Card> targetList, string cardId) {
        try {
            (Card cardFound, int iPos) = Support.findCard(sourceList, cardId);
            targetList.Add(cardFound);
            sourceList.RemoveAt(iPos);        
            return cardFound;
        } catch (CardNotFoundException e) {
            throw e;
        }   
    }

    /*Removes the card with the given id from the source list.
    For convenience it returns the Card instance removed */
    static public Card removeCard(List<Card> sourceList, string cardId) {            
        try {
            (Card cardFound, int iPos) = Support.findCard(sourceList, cardId);
            sourceList.RemoveAt(iPos);        
            return cardFound;
        } catch (CardNotFoundException e) {
            throw e;
        }                 
    }

    /* returns the specified card. Raise CardNotFoundException if card is not there. */
    static public (Card, int) findCard(List<Card> sourceList, string cardId) {
        int iPos = 0;
        Card? cardFound = null;
        foreach(Card card in sourceList) {
            if (card.getId() == cardId) {
                cardFound = card;
                break;
            }
            iPos++;
        }

        if (cardFound == null) {
            throw new CardNotFoundException($"Card with id: '{cardId}' not found");
        }

        return (cardFound, iPos);
    }

    static public bool isOnList(List<Card> sourceList, string cardId) {
        try {
            (Card cardFound, int iPos) = Support.findCard(sourceList, cardId);
            return true;
        } catch (CardNotFoundException) {
            return false;
        }
    }
}