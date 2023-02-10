import abc


class ResourceConfig:
    """
    Represents a config with a specific name
    """
    def __init__(self, name: str, config: dict):
        """
        :param name: The unique name to identify this config
        :param config: The config itself
        """
        self.name = name
        self.config = config


class Reader(metaclass=abc.ABCMeta):
    """
    Interface for loading a lookup-table for redirects.
    """
    __instances = {}

    def __init__(self):
        self._config = None

    @classmethod
    def create(cls, config: ResourceConfig):
        if config.name in cls.__instances:
            return cls.__instances[config.name]

        v_ = cls()
        v_._config = config.config
        v_.load()
        cls.__instances[config.name] = v_

        return v_

    @abc.abstractmethod
    def load(self):
        """
        Loads the lookup-table from the resource.
        :param resource:
        :return:
        """
        raise NotImplementedError

    @abc.abstractmethod
    def getRedirects(self) -> dict:
        """
        Returns the lookup-table
        :return:
        """
        raise NotImplementedError


class Writer(metaclass=abc.ABCMeta):

    def __init__(self):
        self._config = None

    @classmethod
    def create(cls, config: ResourceConfig):

        v_ = cls()
        v_._config = config.config
        return v_

    @abc.abstractmethod
    def write(self, redirects: dict):
        """
        Loads the lookup-table from the resource.
        :param redirects: The redirects to write.
        :return:
        """
        raise NotImplementedError


class RedirectLookup:
    """
    Service to look up a redirect
    """

    def __init__(self, reader: Reader, config: ResourceConfig):
        self.__redirects = None
        self.__config = config
        self.__reader = reader.create(config)

    def getRedirects(self) -> dict:
        if self.__redirects is None:
            self.__redirects = self.__reader.getRedirects()

        return self.__redirects

    def refresh(self):
        """
        Re-read the resource
        :return:
        """
        self.__reader.load()
        self.__redirects = self.__reader.getRedirects()

    @abc.abstractmethod
    def getRedirect(self, name: str) -> str:
        """
        Given a source, returns the target
        :param name: The source
        :return: The target
        :raise LookupError: if no target exists for the given source
        """
        raise NotImplementedError

    def dump(self, writer: type[Writer], config: ResourceConfig):
        """
        Dump the redirects
        :param writer: The writer class
        :param config: The config for the writer
        :return:
        """

        w = writer.create(config)
        w.write(self.getRedirects())
