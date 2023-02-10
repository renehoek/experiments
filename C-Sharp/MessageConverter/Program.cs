namespace MessageConverter;
class Program
{
    static void Main(string[] args)    
    {
        string strPathWorkingFolder = Directory.GetCurrentDirectory();
        string path_input_folder = Path.Combine(strPathWorkingFolder, "InputFiles");
        Console.WriteLine($"Input folder: {path_input_folder}");        
        
        XMLFileProccesor xmlFileProccesor = new XMLFileProccesor();
        xmlFileProccesor.GenerateJSONOutput(Path.Combine(path_input_folder, "person.xml"));

        CSVFileProccesor csvFileProccesor = new CSVFileProccesor();
        csvFileProccesor.GenerateJSONOutput(Path.Combine(path_input_folder, "person.csv"));
                
    }
}
