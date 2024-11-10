def encrypt(plain_text: str, shift_amount: int) -> str:

    encrypted_text = ""
    for char in plain_text:
        ascii_val = ord(char)
        encrypted_text += chr(ascii_val + shift_amount)

    return encrypted_text

def decrypt(encrypted_text: str, shift_amount: int) -> str:
    decrypted_text = ""
    for char in encrypted_text:
        ascii_val = ord(char)
        decrypted_text += chr(ascii_val - shift_amount)
    return decrypted_text

def main():
    plain_text = "hello world"
    shift_amount = 3
    encrypted_text = encrypt(plain_text, shift_amount)
    decrypted_text = decrypt(encrypted_text, shift_amount)
    print(encrypted_text)
    print(decrypted_text)



if __name__ == "__main__":
    main()
