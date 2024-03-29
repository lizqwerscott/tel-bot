import requests
import sys
import json

with open(sys.argv[3], "rb") as f:
    payload = {
        "chat_id": sys.argv[2],
        "title": sys.argv[4],
        "performer": sys.argv[5]
    }

    files = {
        'audio': f.read()
    }
    resp = requests.post("https://api.telegram.org/bot{token}/sendAudio".format(token=sys.argv[1]), data=payload, files=files).json()
    print(json.dumps({ "message_id": resp["result"]["message_id"] }, ensure_ascii=False))
