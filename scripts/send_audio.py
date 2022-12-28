import requests
import sys

with open(sys.argv[3], "rb") as f:
    payload = {
        "chat_id": sys.argv[2],
        "title": sys.argv[4],
        "performer": sys.argv[5]
    }

    files = {
        'audio': f.read()
    }
    resp = requests.post("https://api.telegram.org/bot{token}/sendAudio".format(token=sys.argv[1]), data=payload, files=files)
