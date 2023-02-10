import asyncio
import aiohttp

class WebScraper(object):
    def __init__(self, urls):
        self.urls = urls
        # Global Place To Store The Data:
        self.all_data  = []
        self.master_dict = {}

    async def fetch(self, session, url) -> tuple:
        try:
            async with session.get(url) as response:
                status = response.status
                text = await response.text()
                return text, url, status
        except Exception as e:
            print(str(e))

    async def start(self):
        tasks = []
        headers = {
            "user-agent": "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)"}
        async with aiohttp.ClientSession(headers=headers) as session:
            # for url in self.urls:
            #     tasks.append(self.fetch(session, url))
            #
            # results = await asyncio.gather(*tasks)

            for url in self.urls:
                task = asyncio.create_task(self.fetch(session, url))
                tasks.append(task)

            done, pending = await asyncio.wait(tasks, return_when=asyncio.ALL_COMPLETED)
            results = [x.result() for x in done]

            self.all_data.extend(results)

            # Storing the raw HTML data.
            for result in results:
                if result is not None:
                    url = result[1]
                    self.master_dict[url] = {'body': result[0], 'status': result[2]}
                else:
                    continue

def main():
    the_urls = [
        "https://www.nu.nl", "https://governet.rabobank.nl", "https://www.ad.nl", "https://www.rvhhosting.nl",
        "https://www.integron.nl", "https://www.slashdot.org", "https://www.google.com",
        "https://www.rvhhosting.nl/support.html", "https://governet.faceworks.nl"]

    ws = WebScraper(the_urls)
    asyncio.run(ws.start())

    for url in ws.master_dict:
        status = ws.master_dict[url]['status']
        print(f"URL: {url}  Status: {status}")



if __name__ == "__main__":
   main()