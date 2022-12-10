namespace StringLibraryTest;
using UtilityLibraries;
using Microsoft.VisualStudio.TestTools.UnitTesting;

[TestClass]
public class UnitTest1
{
    
    [TestMethod]
    public void ItShouldStartWithUpper()
    {
        string[] words = {"Hello", "My", "World"};
        foreach(var word in words){
            bool result = word.StartsWithUpper();
            Assert.IsTrue(result, string.Format("Expected for '{0}' True, got: {1}", word, result));
        }
    }

    [TestMethod]
    public void ItShouldStartWithLower()
    {
        string[] words = {"hello", "my", "world", "0", ","};
        foreach(var word in words){
            bool result = word.StartsWithUpper();
            Assert.IsFalse(result, string.Format("Expected for '{0}' False, got: {1}", word, result));
        }
    }
    
    [TestMethod]
    public void ItShouldHandleNoneAndEmpty()
    {
        string?[] words = {"", null, string.Empty};
        foreach(var word in words){
            bool result = StringLibrary.StartsWithUpper(word);
            Assert.IsFalse(result, string.Format("Expeted for '{0}' False, got: {1}", word, result));
        }
    }
}