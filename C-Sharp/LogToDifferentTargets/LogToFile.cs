class LogToFile : ILogger {

    private string path = @"/tmp/my_logfile.txt";

    public void LogError(String message) {
        DateTime now = DateTime.Now;

        using (StreamWriter sw = File.AppendText(this.path))
        {
            sw.WriteLine($"{now.ToString()} ERROR: " + message);
        }        
    }

    public void LogWarning(String message) {
        DateTime now = DateTime.Now;

        using (StreamWriter sw = File.AppendText(this.path))
        {
            sw.WriteLine($"{now.ToString()} WARNING: " + message);
        }        
    }

    public void LogInfo(String message) {
        DateTime now = DateTime.Now;
        
        using (StreamWriter sw = File.AppendText(this.path))
        {
            sw.WriteLine($"{now.ToString()} INFO: " + message);
        }        
    }
}