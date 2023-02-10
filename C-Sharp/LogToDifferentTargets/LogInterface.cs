interface ILogger {
    abstract public void LogError(String message);
    abstract public void LogWarning(String message);
    abstract public void LogInfo(String message);
}