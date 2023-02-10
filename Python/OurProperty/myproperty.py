class chatty_property:
    """ emulation of the property class
        for educational purposes """

    def __init__(self,
                 fget=None,
                 fset=None,
                 fdel=None,
                 doc=None):
        """Attributes of 'our_decorator'
        fget
            function to be used for getting
            an attribute value
        fset
            function to be used for setting
            an attribute value
        fdel
            function to be used for deleting
            an attribute
        doc
            the docstring
        """
        self.fget = fget
        self.fset = fset
        self.fdel = fdel
        print("\n__init__ called with:")
        print(f"fget={fget}, fset={fset}, fdel={fdel}, doc={doc}")

        if doc is None and fget is not None:
            print(f"doc set to docstring of {fget.__name__} method")
            doc = fget.__doc__
        self.__doc__ = doc

    def __get__(self, obj, objtype=None):
        if obj is None:
            return self
        if self.fget is None:
            raise AttributeError("unreadable attribute")
        return self.fget(obj)

    def __set__(self, obj, value):
        if self.fset is None:
            raise AttributeError("can't set attribute")
        self.fset(obj, value)

    def __delete__(self, obj):
        if self.fdel is None:
            raise AttributeError("can't delete attribute")
        self.fdel(obj)

    def getter(self, fget):
        return type(self)(fget, self.fset, self.fdel, self.__doc__)

    def setter(self, fset):
        return type(self)(self.fget, fset, self.fdel, self.__doc__)

    def deleter(self, fdel):
        return type(self)(self.fget, self.fset, fdel, self.__doc__)