from collections.abc import Callable

the_loggers = []

Callback = Callable[..., None]


def register_logger(fnoutput=None) -> Callable[[Callback], [Callback]]:
    """
    :param fnoutput: The filename to use for output
    :return:
    """

    def register_logger_inner(func: Callback) -> Callback:
        """
        Register function logger in the list of loggers
        :param func:
        :return:
        """
        if fnoutput is None:
            the_loggers.append((func, None))
        else:
            the_loggers.append((func, {'fnoutput': fnoutput}))
        return func

    return register_logger_inner


@register_logger()
def log_to_console(text: str):
    """
    Log to console
    :param text: The text to log
    :return:
    """
    print(text)


@register_logger(fnoutput="./logging.txt")
def log_to_file(text: str, fnoutput: str):
    """
    Log to file
    :param text: The text to log
    :param fnoutput: The filename to write to
    return:
    """
    with open(fnoutput, "a") as o_file:
        o_file.write("{}\n".format(text))


def log_text(text: str):
    """
    Log the text
    :param text: The text
    :return:
    """
    for log_s in the_loggers:
        logger = log_s[0]
        kw = log_s[1]
        if kw is not None:
            logger(text, **kw)
        else:
            logger(text)
