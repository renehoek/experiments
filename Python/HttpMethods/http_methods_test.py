import requests

def test_http_methods():
    url = "https://httpbin.org"
    methods = {
        "GET": f"{url}/get",
        "POST": f"{url}/post",
        "PUT": f"{url}/put",
        "DELETE": f"{url}/delete",
        "HEAD": f"{url}/get",  # HEAD werkt als GET maar zonder body
        "OPTIONS": f"{url}/get",  # OPTIONS werkt op elke URL
        "PATCH": f"{url}/patch",
        "TRACE": f"{url}/trace",  # Niet altijd toegestaan om veiligheidsredenen
    }
    
    for method, endpoint in methods.items():
        try:
            response = requests.request(method, endpoint)
            print(f"{method} {endpoint} - Status: {response.status_code}")
            
            if method != "HEAD":  # HEAD heeft geen body
                print("Response:", response.text[:200], "...")  # Toon eerste 200 tekens
        except requests.RequestException as e:
            print(f"Error bij {method} verzoek: {e}")
    
    # CONNECT kan niet direct via requests worden getest, want dit wordt op proxy-niveau afgehandeld.
    print("CONNECT kan niet direct worden getest met requests, dit gebeurt op proxy-niveau.")

if __name__ == "__main__":
    test_http_methods()
