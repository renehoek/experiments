namespace MessageConverter;

class CSVFileProccesor : FileProccesorBase {
    /* This is a concrete class. It derives from the abstract 'FileProcessorBase' class, so it 
    must provide an implementation for the 'readFile' method. This implementation can read a CSV file */
    protected override PersonData readFile(string inputFileName) {
        
        /*Don't focus on the CSV file reading below. What is relevant is that this class implements a specific behaviour */
        string[] lines = File.ReadAllLines(inputFileName);
        string[] messageContent = lines[0].Split(',');

        PersonData pd = new PersonData();
        pd.firstName = messageContent[0];
        pd.lastName = messageContent[1];
        pd.personNumber = messageContent[2];

        return pd;

    }
}