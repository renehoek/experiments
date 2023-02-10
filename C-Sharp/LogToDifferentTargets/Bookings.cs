class Bookings {

    private ILogger _logger;
    public Bookings(ILogger logger) {
        this._logger = logger;
    }
    
    private bool bookRoom(int roomNumber, string person_name) {
        /* Imagine some implementation to check that room is available and if so mark the room as reserved */
        Random gen = new Random();
        return gen.Next(0, 2) == 1 ? true : false;        
    }

    public void reserveRoom(int roomNumber, string person_name) {        
        if (this.bookRoom(roomNumber, person_name)) {            
            this._logger.LogInfo($"Room {roomNumber} booked for {person_name}");
        } else {
            this._logger.LogWarning($"Room {roomNumber} not booked for {person_name}");
        }
    }
}