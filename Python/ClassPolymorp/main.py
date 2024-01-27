# This is a sample Python script.

# Press ⇧F10 to execute it or replace it with your code.
# Press Double ⇧ to search everywhere for classes, files, tool windows, actions, and settings.
import os
import random
import tempfile

from bad_solution import start_the_count as start_the_count_bad_solution
from elegant_solution import start_the_count as start_the_count_elegant_solution
from elegant_solution_mapreduce import PathInputData, WebsiteInputData, LineCounterWorker, SpaceCounterWorker


def write_test_files(datadir):
    for i in range(100):
        with open(os.path.join(datadir, str(i)), 'w') as f:

            for line in range(0, random.randint(0, 100)):
                sp = ' ' * random.randint(0, 8)
                f.write("{sp}\n".format(sp=sp))

# noinspection PyCompatibility
def main():
    with tempfile.TemporaryDirectory() as tmpdir:
        write_test_files(tmpdir)
        result = start_the_count_bad_solution(tmpdir)
        print("Number of lines: {r}".format(r=result))

        result = start_the_count_elegant_solution(LineCounterWorker, PathInputData, {'datadir': tmpdir})
        print("Number of lines: {r}".format(r=result))

        result = start_the_count_elegant_solution(SpaceCounterWorker, PathInputData, {'datadir': tmpdir})
        print("Number of spaces: {r}".format(r=result))

        urls = ["https://www.nu.nl", "https://www.ad.nl"]
        result = start_the_count_elegant_solution(LineCounterWorker, WebsiteInputData, {'urls': urls})
        print("Number of lines: {r}".format(r=result))

        result = start_the_count_elegant_solution(SpaceCounterWorker, WebsiteInputData, {'urls': urls})
        print("Number of spaces: {r}".format(r=result))


# Press the green button in the gutter to run the script.
if __name__ == '__main__':
    main()
