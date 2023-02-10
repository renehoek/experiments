namespace MessageConverter;
using System.Text.Json;

abstract class FileProccesorBase {
    
    protected abstract PersonData readFile(string inputFileName);
    /*This method must be implemented by concrete classes. In this abstract class only the signature is declared */
    
    public void GenerateJSONOutput(string inputFileName) {
        /*This method is the common functionality, that is: Given a input generate a JSON version of it */
        
        PersonData pd = this.readFile(inputFileName);
        /* In the line above, the 'readFile' method is called. The implementation is provided by a concrete class */

        string filecontentAsJson = JsonSerializer.Serialize(pd);

        DateTime now = DateTime.Now;
        string outputFileName = @"/tmp/message_converter_" + now.ToString("yyyy-MM-dd-HH-mm-ss-fff") + ".json";
        File.WriteAllText(outputFileName, filecontentAsJson);
        Console.WriteLine($"JSON file writen in: {outputFileName}");

    }
}