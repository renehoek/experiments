namespace MessageConverter;
using System.Xml;
using System.Xml.Linq;

class XMLFileProccesor : FileProccesorBase {
    /* This is a concrete class. It derives from the abstract 'FileProcessorBase' class, so it 
    must provide an implementation for the 'readFile' method. This implementation can read a XML file */
    protected override PersonData readFile(string inputFileName) {
        
        /*Don't focus on the XML file reading below. What is relevant is that this class implements a specific behaviour */
        XDocument doc = XDocument.Load(inputFileName);        
        XElement? element = doc.Element("Message");

        PersonData pd = new PersonData();
        if (element is not null) {        
                        
            XElement? element_firstname = element.Element("FirstName");            
            pd.firstName = element_firstname is not null ? element_firstname.Value : String.Empty;
                                    
            XElement? element_lastname = element.Element("LastName");
            pd.lastName = element_lastname is not null ? element_lastname.Value : String.Empty;
            
            XElement? element_personNumber = element.Element("PersonNumber");
            pd.personNumber = element_personNumber is not null ? element_personNumber.Value : String.Empty;
            
        } 
        return pd;
    }
}