import unittest
import ceasercipher


class MyTestCase(unittest.TestCase):
    def test_encrypt(self):
        plain_text = 'hello world'
        encrypted_text = ceasercipher.encrypt(plain_text, 4)
        decrypted_text = ceasercipher.decrypt(encrypted_text, 4)
        self.assertEqual(plain_text, decrypted_text)



if __name__ == '__main__':
    unittest.main()
