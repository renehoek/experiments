import os
from threading import Thread
from typing import List, Iterator

from bad_solution_mapreduce import PathInputData, LineCounterWorker, Worker


def generate_inputs(datadir) -> Iterator[PathInputData]:
    for fn in os.listdir(datadir):
        yield PathInputData(os.path.join(datadir, fn))


def create_workers(input_datas: Iterator[PathInputData]):
    workers = []
    for input_data in input_datas:
        workers.append(LineCounterWorker(input_data))

    return workers


def execute_workers(workers: List[Worker]):
    threads = [Thread(target=w.map) for w in workers]
    for thr in threads:
        thr.start()
    for thr in threads:
        thr.join()

    # noinspection PyCompatibility
    first, *rest = workers
    for worker in rest:
        first.reduce(worker)

    return first.result


def start_the_count(data_dir: str):
    inputs = generate_inputs(data_dir)
    workers = create_workers(inputs)
    return execute_workers(workers)
