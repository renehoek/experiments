from redirect.interfaces import Writer


class SortedTextWriter(Writer):
    def write(self, redirects: dict):
        """
        Write the redirects in the file.
        :param redirects:
        :return:
        """

        def sort_domain(x: tuple) -> str:
            source = x[0]
            s_ = source.split('.')
            if len(s_) > 2:
                return s_[1]
            elif len(s_) == 2:
                return s_[0]
            else:
                return source

        redirects = [(s, t) for s, t in redirects.items()]
        redirects.sort(key=sort_domain)

        wf = self._config['file_hndl']
        for s, t in redirects:
            wf.write("{s}\t{t}\n".format(s=s, t=t))
