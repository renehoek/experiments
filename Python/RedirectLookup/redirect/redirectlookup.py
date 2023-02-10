from redirect.interfaces import RedirectLookup


class WebRedirectLookup(RedirectLookup):
    """
    Redirect lookup service for the internet
    """

    def getRedirect(self, name: str) -> str:
        """
        Returns the target for the given name
        :param name: The source
        :return: The target
        :raises KeyError: If no redirect exists for the given name.
        """
        return super().getRedirects()[name]
