class LogToConsole : ILogger {
    public void LogError(String message) {
        Console.WriteLine("ERROR: " + message);
    }

    public void LogWarning(String message) {
        Console.WriteLine("WARNING: " + message);
    }

    public void LogInfo(String message) {
        Console.WriteLine("INFO: " + message);
    }
}