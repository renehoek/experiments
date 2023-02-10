import abc
import os


class DirectoryConfig(abc.ABC):
    @abc.abstractmethod
    def getBaseDir(self):
        raise NotImplementedError("This method should be implemented")

class DirectorySize:

    def __get__(self, instance, owner):
        count = 0
        for path in os.scandir(instance.getBaseDir()):
            if path.is_file():
                count += 1
        return count
