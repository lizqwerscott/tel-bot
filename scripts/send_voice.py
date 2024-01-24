import requests
import sys
import json

with open(sys.argv[3], "rb") as f:
    payload = {
        "chat_id": sys.argv[2],
    }

    files = {
        'voice': f.read()
    }
    resp = requests.post("https://api.telegram.org/bot{token}/sendVoice".format(token=sys.argv[1]), data=payload, files=files).json()
    print(json.dumps({ "message_id": resp["result"]["message_id"] }, ensure_ascii=False))
