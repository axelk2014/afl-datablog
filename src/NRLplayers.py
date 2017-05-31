#!/usr/bin/python

import os,re,time
import urllib.request
from bs4 import BeautifulSoup


soup = BeautifulSoup(open("NRLPlayers.htm"),"lxml")

playerlinks = soup.find_all("a")
mymatch = re.compile('[/]rugby-league[/]players[/][a-z]*-[a-z]*[/]')
players = mymatch.findall(str(playerlinks))
f = open('output.txt','w')
for p in players:
	f.write(p+"\n")



# https://www.zerotackle.com/rugby-league/players/ashley-taylor/'
# https://www.zerotackle.com/rugby-league/players/leeson-ah-mau/