from typing import Type

from elegant_solution_mapreduce import Worker, InputData


def start_the_count(worker_class: Type[Worker], inputdata_class: Type[InputData], config: {}):

    workers = worker_class.create_workers(inputdata_class, config)
    return worker_class.execute(workers)



