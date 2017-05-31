#!/usr/bin/python

import os,re,time
import urllib.request
from bs4 import BeautifulSoup


path = '/Users/alex/Documents/datasets/afl/data/raw/afltables.com/'


soup = BeautifulSoup(open(path+'playersA.html'),"lxml")

f = open(path+'exceptionlist.txt', 'w')
lstplayersPages = [y['href'] for y in soup.find_all('a', href = re.compile(r'players[/]'))]
for player in lstplayersPages:
	#response = urllib.request.urlopen('http://afltables.com/afl/stats/'+player)
	#data = response.read()
	s = open(path+player[10:])
	soup2 = BeautifulSoup(s, 'lxml')
	table = soup2.find('table')
	#print(player)
	# first year
	first_year = table.find_all('tr')[1].find_all('td')[0].text
	# last year
	last_year = table.find_all('tr')[-3].find_all('td')[0].text
	if (int(first_year)+6 < 2009) or (int(last_year)-7 > 2009):
		print(player)
		f.write(player[10:]+'\n')
	else:
		print('skipping - '+player)
		continue

