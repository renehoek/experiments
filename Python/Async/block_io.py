# SuperFastPython.com
# example of running a blocking function call in asyncio
import time
import asyncio
import threading

# blocking function
def blocking_task():
    # report a message
    print('task is running')
    # block
    time.sleep(4)

    # report a message
    print('task is done')
    thread = threading.current_thread()
    print(f'name={thread.name}, daemon={thread.daemon}')


# background coroutine task
async def background():
    # loop forever
    while True:
        # report a message
        print('>background task running')
        # sleep for a moment
        await asyncio.sleep(0.5)


# main coroutine
async def main():
    # run the background task
    _ = asyncio.create_task(background())
    # execute the blocking call
    coro = asyncio.to_thread(blocking_task)
    await coro

if __name__ == "__main__":
    # start the asyncio program
    asyncio.run(main())