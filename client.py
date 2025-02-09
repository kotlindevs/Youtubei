import requests

STATUS_OK=200

client = requests.get(
    url = "https://httpbin.org/basic-auth/user/pass",auth=("user","pass")
)
if client.status_code == STATUS_OK:
    print("Response 200!")

headers = client.headers["content-type"]
jsonValue = client.text

print("Result :",jsonValue)