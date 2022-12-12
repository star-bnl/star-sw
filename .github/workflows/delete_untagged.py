#!/usr/bin/env python3

import argparse
import json
import requests
import types

parser = argparse.ArgumentParser()
parser.add_argument("token", help="GitHub token")
args = parser.parse_args()

domain = "api.github.com"
org = "star-bnl"
package_type = "container"
package_name = "star-sw"

api_url = f"https://{domain}/orgs/{org}/packages/{package_type}/{package_name}/versions"

respjson = requests.get(api_url, auth=("token", args.token))
entries  = json.loads(respjson.text, object_hook=lambda d: types.SimpleNamespace(**d))

for e in entries:
    if not e.metadata.container.tags:
        response = requests.delete(api_url + f"/{e.id}", auth=("token", args.token))
        print("delete", e.id, e.html_url, e.name, response.url, response.status_code)
