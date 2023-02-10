# This is a sample Python script.

# Press ⇧F10 to execute it or replace it with your code.
# Press Double ⇧ to search everywhere for classes, files, tool windows, actions, and settings.

from redirect.interfaces import ResourceConfig
from redirect.reader import TextFileReader, JsonFileReader, FixtureJsonReader
from redirect.redirectlookup import WebRedirectLookup
from redirect.writer import SortedTextWriter

def main():
    # rl = WebRedirectLookup(FixtureJsonReader, ReaderConfig("redirects_adad.json",
    #                                                        {'file_hndl': open("./data/redirects_adad.json")}))

    rl = WebRedirectLookup(TextFileReader, ResourceConfig("./data/redirects_adad.txt",
                                                          {'file_hndl': open("./data/redirects_adad.txt")}))

    rl.dump(SortedTextWriter, ResourceConfig('not_used', {'file_hndl': open("./data/redirects_adad.txt", "w")}))

    # rl = WebRedirectLookup(TextFileReader, ReaderConfig("./data/redirects_adad.txt",
    #                                                      {'file_hndl': open("./data/redirects_adad.txt")}))
    # target = rl.getRedirect("masterofthering.co.uk")
    # print(target)
    #
    # rl = WebRedirectLookup(JsonFileReader, ReaderConfig("./data/redirects.json",
    #                                                     {'file_hndl': open("./data/redirects.json")}))
    # target = rl.getRedirect("a8.nl")
    # print(target)







# Press the green button in the gutter to run the script.
if __name__ == '__main__':
    main()

# See PyCharm help at https://www.jetbrains.com/help/pycharm/
