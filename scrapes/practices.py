"""
Pull all available practice email addresses
"""
import urlparse

import ffs
from lxml import html
import requests

base = "http://www.nhs.uk/ServiceDirectories/Pages/PrimaryCareTrustListing.aspx"
gp_base = "http://www.nhs.uk/Services/Trusts/GPs/DefaultView.aspx?id="

data = ffs.Path(__file__).parent / '../data'

surgeriez = {}

page = html.fromstring(requests.get(base).content)
page.make_links_absolute("http://www.nhs.uk")

linkz = page.cssselect('.trust-list li a')
linkz = [l for l in linkz if l.attrib.get('class',  None) !=  'back-to-top']
linkz = [(l.text, l.attrib['href']) for l in linkz]

for pct in linkz:

    print pct[0]
    pctid = pct[1].split('=')[1]
    pctpage = html.fromstring(requests.get(gp_base + pctid).content)
    for surgery in pctpage.cssselect('div.child-org-item'):
        name = surgery.cssselect('h3 a')[0].text
        addr = surgery.cssselect('dd.addrss')[0].text
        email = None
        if surgery.cssselect('dd.email a'):
            email = surgery.cssselect('dd.email a')[0].text

        surgeriez[name] = dict(name=name, email=email, addr=addr, pct=pct[0])


withemail = [s for s in surgeriez.values() if s['email'] is not None]

withfile = data / 'practice_emails.csv'

with withfile.csv() as csv:
    csv.writerow(['pct', 'name', 'addr', 'postcode', 'email'])
    for practice in withemail:
        csv.writerow([practice['pct'],
                      practice['name'],
                      practice['addr'],
                      " ".join(practice['addr'].split()[-2:]),
                      practice['email'] or ""
                      ])


import ipdb
ipdb.set_trace()
