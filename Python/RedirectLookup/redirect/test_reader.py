from unittest import TestCase
from interfaces import ResourceConfig
from reader import TextFileReader, LineNotValidError
from io import StringIO


class TestFileReader(TestCase):

    def test_should_parse_a_line(self):
        a_line = "a-8.nl  http://www.active8.nl/"
        r = TextFileReader._line_to_key_value(a_line)
        self.assertEqual(r, ("a-8.nl", "http://www.active8.nl/"))

        a_line = "a-8.nl    http://www.active8.nl/"
        r = TextFileReader._line_to_key_value(a_line)
        self.assertEqual(r, ("a-8.nl", "http://www.active8.nl/"))

        a_line = "a-8.nl    http://www.active8.nl/ bb"
        r = TextFileReader._line_to_key_value(a_line)
        self.assertEqual(r, ("a-8.nl", "http://www.active8.nl/"))

    def test_should_warn_about_invalid_line(self):
        a_line = "a-8.nl"
        with self.assertRaises(LineNotValidError):
            TextFileReader._line_to_key_value(a_line)

        a_line = ""
        with self.assertRaises(LineNotValidError):
            TextFileReader._line_to_key_value(a_line)

        a_line = "  "
        with self.assertRaises(LineNotValidError):
            TextFileReader._line_to_key_value(a_line)

    def test_should_load_lookup_file(self):

        with open("../data/redirects.txt") as fn:
            fr = TextFileReader.create(ResourceConfig("load_lookup_file", {'file_hndl': fn}))

        expected = {"a-8.nl": "http://www.active8.nl/",
                    "a8.nl": "http://www.active8.nl/",
                    "advocatentarieven.nl": "http://www.verzekeringsadvocaten.nl/"}
        rv = fr.getRedirects()
        self.assertDictEqual(rv, expected)

    def test_should_do_a_reload(self):
        fn = StringIO("""a-8.nl  http://www.active8.nl/
            a8.nl   http://www.active8.nl/
            advocatentarieven.nl    http://www.verzekeringsadvocaten.nl/
            # This is a comment            
            """)


        fr = TextFileReader.create(ResourceConfig("do_reload", {'file_hndl': fn}))

        expected = {"a-8.nl": "http://www.active8.nl/",
                    "a8.nl": "http://www.active8.nl/",
                    "advocatentarieven.nl": "http://www.verzekeringsadvocaten.nl/"}
        rv = fr.getRedirects()
        self.assertDictEqual(rv, expected)

        fn.truncate(0)
        fn.seek(0)
        fn.write("""a-8.nl  http://www.active8.nl/
    a8.nl   http://www.active8.nl/
    advocatentarieven.nl    http://www.verzekeringsadvocaten.nl/
    # This is a comment
    aktive8.nl   http://www.active8.nl/
    """)

        fr.load()
        expected = {"a-8.nl": "http://www.active8.nl/",
                    "a8.nl": "http://www.active8.nl/",
                    "advocatentarieven.nl": "http://www.verzekeringsadvocaten.nl/",
                    "aktive8.nl": "http://www.active8.nl/"}
        rv = fr.getRedirects()
        self.assertDictEqual(rv, expected)

    def test_should_return_same_lookup_table(self):
        fn = StringIO("""a-8.nl  http://www.active8.nl/
                    a8.nl   http://www.active8.nl/
                    advocatentarieven.nl    http://www.verzekeringsadvocaten.nl/
                    # This is a comment            
                    """)

        fn2 = StringIO("""some crazy text file""")

        fr1 = TextFileReader.create(ResourceConfig("return_same_lookup_table", {'file_hndl': fn}))
        fr2 = TextFileReader.create(ResourceConfig("return_same_lookup_table", {'file_hndl': fn2}))
        # TextFileReader fr2 has the same config-id. So fr2 should ignore that 'crazy text file' and use
        # the same as TextFileReader fr

        expected = {"a-8.nl": "http://www.active8.nl/",
                    "a8.nl": "http://www.active8.nl/",
                    "advocatentarieven.nl": "http://www.verzekeringsadvocaten.nl/"}
        rv1 = fr1.getRedirects()
        rv2 = fr2.getRedirects()
        self.assertDictEqual(rv1, expected)
        self.assertDictEqual(rv2, expected)


    def test_should_ignore_whitespaces_before_and_after(self):
        fn = StringIO("""a-8.nl  http://www.active8.nl/      
                a8.nl   http://www.secondentry.nl/                        
                mijndomein.nl   http://www.secondentry.nl/      
                """)

        fr = TextFileReader.create(ResourceConfig("ignore_whitespaces_before_and_after", {'file_hndl': fn}))

        expected = {"a-8.nl": "http://www.active8.nl/",
                    "a8.nl": "http://www.secondentry.nl/",
                    "mijndomein.nl": "http://www.secondentry.nl/"}
        rv = fr.getRedirects()
        self.assertDictEqual(rv, expected)

    def test_should_keep_last_seen_key(self):
        fn = StringIO("""a-8.nl  http://www.active8.nl/
        a8.nl   http://www.active8.nl/        
        # This is a comment
        a8.nl   http://www.secondentry.nl/
        """)

        fr = TextFileReader.create(ResourceConfig("keep_last_seen_key", {'file_hndl': fn}))

        expected = {"a-8.nl": "http://www.active8.nl/",
                    "a8.nl": "http://www.secondentry.nl/"}
        rv = fr.getRedirects()
        self.assertDictEqual(rv, expected)
