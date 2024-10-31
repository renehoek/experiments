# Recursive Python3 program for
# coin change problem.

# Returns the count of ways we can sum
# coins[0...n-1] coins to get sum "sum"

# https://www.geeksforgeeks.org/coin-change-dp-7/?ref=lbp 
def count(coins, n, sum):
    # print("Coins:", coins)
    # print("N:", n)
    # print("Sum:", sum)
    # print("====")

    # If sum is 0 then there is 1
    # solution (do not include any coin)
    if (sum == 0):
        return 1

    # If sum is less than 0 then no
    # solution exists
    if (sum < 0):
        return 0

    # If there are no coins and sum
    # is greater than 0, then no
    # solution exist
    if (n <= 0):
        return 0

    # count is sum of solutions (i)
    # including coins[n-1] (ii) excluding coins[n-1]

    coin_included = count(coins, n, sum - coins[n - 1])
    coin_excluded = count(coins, n - 1, sum)
    result = coin_included + coin_excluded
    print (f"Sum: {sum}, Coin included: {coin_included}, Coin excluded: {coin_excluded}, result: {result}")
    return result


def main():
    # Driver program to test above function
    coins = [1, 2, 3]
    n = len(coins)
    print(count(coins, n, 5))


if __name__ == '__main__':
    main()
# This code is contributed by Smitha Dinesh Semwal
