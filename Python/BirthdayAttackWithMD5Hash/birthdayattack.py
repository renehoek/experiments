import hashlib
import random


# Function to generate a random string of a given length
def generate_random_string(length):
    charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    return ''.join(random.choice(charset) for _ in range(length))


# Function to perform the Birthday Attack
# md5 hash has weak collision resistance --> Given two random inputs it is feasible to get the same hash-value.
#
# BirthDay paradox --> With 23 people there is 50% chance that at least two have the same birthday
# Year is 365 days, we consider birthday as any day in a year (day & month).
# See the following calculation:
#   With 23 people, there are 253 pairs (23*22) / 2 = 253
#   The chance of 2 people having different birthdays is: 1 - (1/365) => 364/365 => .997
#   The chance of 253 pairs of these 2 people having different birthdays is: (364/365) ^ 253 = .4995
#   The chance of 253 pairs of these 2 people having the same birthday is: 1 - .4995 = 0.5005
# or
#   With 13 people there is 95% chance of picking the same letter of the alphabet
#   With 13 people, there are (13*12) / 2 = 78 pairs
#   The chance that two people pick a different letter is: (25/26) => 0.961 (very high)
#   The chance that 78 pairs pick a different letter is: (25/26) * (25/26) * ... => (25/26) ^ 78 => 0.046 (very low)
#   The chance that one pair of these 78 pick the same letter is: 1 - 0.046 => 0.954 (very high)
def birthday_attack():
    hash_dict = {}
    num_attempts = 0

    while True:
        num_attempts += 1
        random_string = generate_random_string(10)
        hash_value = hashlib.md5(random_string.encode()).hexdigest()

        if hash_value in hash_dict:
            print(f"Collision found after {num_attempts} attempts!")
            print(f"Original String 1: {hash_dict[hash_value]}")
            print(f"Original String 2: {random_string}")
            break

        hash_dict[hash_value] = random_string


# Example usage
if __name__ == "__main__":
    birthday_attack()
