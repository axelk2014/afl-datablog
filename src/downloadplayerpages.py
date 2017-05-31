#!/usr/bin/python

import os,re,time
import urllib.request
from bs4 import BeautifulSoup

# set variables
time_start = time.strftime("%X")
path = '/Users/alex/Documents/datasets/afl/data/raw/afltables.com/'

# download first file
urllib.request.urlretrieve('http://afltables.com/afl/stats/playersA_idx.html', path+'playersA.html')

soup = BeautifulSoup(open(path+'playersA.html'),"lxml")

# loop for letter pages
lstplayerLetterPages = [x['href'] for x in soup.find_all('a', href = re.compile(r'players[A-Z]_*'))]

# iterate through list and feed into loop below
for p in lstplayerLetterPages[:
	response = urllib.request.urlopen('http://afltables.com/afl/stats/'+p)
	#response = urllib.request.urlopen('http://afltables.com/afl/stats/'+link['href'])
	data = response.read()
	soup2 = BeautifulSoup(data, 'lxml')
	# loop for player pages
	lstplayersPages = [y['href'] for y in soup2.find_all('a', href = re.compile(r'players[/]'))]
	for player in lstplayersPages:
		response = urllib.request.urlopen('http://afltables.com/afl/stats/'+player)
		data = response.read()
		soup3 = BeautifulSoup(data, 'lxml')
		table = soup3.find('table')
		print(player)
		# first year
		first_year = table.find_all('tr')[1].find_all('td')[0].text
		# last year
		last_year = table.find_all('tr')[-3].find_all('td')[0].text
		if (int(first_year)+6 >= 2009) or (int(last_year)-6 >= 2009):
			print('downloading '+player)
			urllib.request.urlretrieve('http://afltables.com/afl/stats/'+player, path+player[10:])
			time.sleep(4)
		else:
			print('skipping - '+player)
			continue


time_end = time.strftime("%X")
print('\n\ndowloading player pages finished\nstarted:'+time_start+'\ncompleted:'+time_end)


