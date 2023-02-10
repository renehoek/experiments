import asyncio
import urllib.parse
import random
import sys

the_urls = [
    "https://www.nu.nl", "https://governet.rabobank.nl", "https://www.ad.nl", "https://www.rvhhosting.nl",
    "https://www.integron.nl", "https://www.slashdot.org", "https://www.google.com"]

async def print_http_headers(url):
    url = urllib.parse.urlsplit(url)

    if url.scheme == 'https':
        reader, writer = await asyncio.open_connection(url.hostname, 443, ssl=True)
    else:
        reader, writer = await asyncio.open_connection(url.hostname, 80)

    query = (
        f"HEAD {url.path or '/'} HTTP/1.0\r\n"
        f"Host: {url.hostname}\r\n"
        f"\r\n"
    )

    writer.write(query.encode('latin-1'))
    lines = []
    while True:
        line = await reader.readline()
        if not line:
            break

        line = line.decode('latin1').rstrip()
        if line is not None:
            lines.append(line)
        #   print(f'Host: {url.hostname} HTTP header> {line}')

    # Ignore the body, close the socket
    writer.close()

    return lines


async def main():
    background_tasks = set()

    for url in the_urls:
        task = asyncio.create_task(print_http_headers(url))
        background_tasks.add(task)
        task.add_done_callback(background_tasks.discard)

    rvalue = await asyncio.gather(*background_tasks)
    for r in rvalue:
        print(r)


if __name__ == "__main__":
    asyncio.run(main())