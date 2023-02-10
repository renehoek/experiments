import io
from typing import Tuple, TextIO

from redirect.interfaces import Reader
import re
import json


class LineNotValidError(RuntimeError):
    pass


class TextFileReader(Reader):
    """
    Loads the lookup-table from a file
    """

    def __init__(self):
        super().__init__()
        self.__redirects = {}

    @staticmethod
    def _line_to_key_value(line: str) -> Tuple[str, str]:
        """
        Split a line in to two values. One is the key, the other the value
        :param line: The line
        :return: Key, value
        :raise LineNotValidError: If the line could not be parsed.
        """
        sentences = re.split(' +|\t', line)
        sentences = [sentence.strip() for sentence in sentences]
        sentences = [x for x in sentences if len(x) > 0]
        if len(sentences) >= 2:
            return sentences[0], sentences[1]

        raise LineNotValidError("Line {s} could not be parsed".format(s=line))

    def __parse_contents(self, resource: TextIO) -> dict:
        """
        Parse the contents of the file
        :param resource: The stream to parse
        :return: The lookup dictionary
        """

        resource.seek(0)
        lookup = {}
        while line := resource.readline().strip():
            if line.startswith("#"):
                continue
            try:
                k, v = self._line_to_key_value(line)
            except LineNotValidError:
                pass
            else:
                lookup[k] = v

        return lookup

    def load(self):
        """

        :return:
        """
        if 'file_hndl' in self._config:
            self.__redirects = self.__parse_contents(self._config['file_hndl'])
        elif 'file_name' in self._config:
            with open(self._config['file_name'], 'r') as fnr:
                self.__redirects = self.__parse_contents(fnr)
        else:
            raise RuntimeError("No resource is specified in the config.")


    def getRedirects(self) -> dict:
        """
        Returns the lookups
        :return:
        """
        return self.__redirects


class JsonFileReader(Reader):
    """
    Reads the redirects from a json file
    """

    def __init__(self):
        super().__init__()
        self.__redirects = {}

    def load(self):
        resource = self._config['file_hndl']
        resource.seek(0)
        self.__redirects = json.load(resource)

    def getRedirects(self) -> dict:
        """
        Returns the lookups
        :return:
        """
        return self.__redirects


class FixtureJsonReader(Reader):
    """
    Reads the django Fixture in Json format
    """

    def __init__(self):
        super().__init__()
        self.__redirects = {}

    def store_redirect(self, item: dict):
        """
        Stores a single item from the fixture.
        :param item: The item in the list of fixtures.
        :return:
        """
        host = item["fields"]["host"].strip()
        domain = item["fields"]["domain"].strip()
        target = item["fields"]["target"].strip()

        if len(host) > 0:
            self.__redirects["{h}.{d}".format(h=host, d=domain)] = target
        if len(host) == 0:
            self.__redirects["{d}".format(h=host, d=domain)] = target
        if host == "www":
            self.__redirects["{d}".format(h=host, d=domain)] = target

    def load(self):
        resource = self._config['file_hndl']
        resource.seek(0)
        v = json.load(resource)
        for item in v:
            self.store_redirect(item)

    def getRedirects(self) -> dict:
        """
        Returns the lookups
        :return:
        """
        return self.__redirects


class DictReader(Reader):
    """
    Keeps the provided dictionary as lookup-table
    """

    def load(self):
        """
        Pass, the dictionary is already loaded.
        :return:
        """
        pass

    def getRedirects(self) -> dict:
        return self._config
