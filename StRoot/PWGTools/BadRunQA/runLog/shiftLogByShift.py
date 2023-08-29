import warnings
from selenium import webdriver
import time
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.wait import WebDriverWait
from selenium.common.exceptions import TimeoutException
from bs4 import BeautifulSoup
import re
from dateutil import parser
import traceback
import pytz
from datetime import datetime, timedelta
from prettytable import PrettyTable, ALL
import os

import sentiment as sen
import UI
import browser
from pageCache import PageCache
#from shiftLog import autoLogin

def findRunTime(runID, driver, timeout, pc):
    year = int(runID[:2]) - 1
    url = 'https://online.star.bnl.gov/RunLogRun%d/index.php?r=%s' % (year, runID)
    soup = BeautifulSoup(pc.getUrl(url, driver, timeout, 'STAR RunLog Browser'), "html.parser")
   
    # see if it's mark as junk by shift Leader
    # should NOT have been produced in the first place
    # but it does happen
    spans = soup.find_all('span', 'cr')
    if len(spans) > 0 and 'Marked as Junk' in spans[0].text:
        raise ValueError('Shift leader marked the run %s as junk' % runID)
    spans = soup.find_all('span', 'cg')
    startTime = spans[0].text # first span should be RTS Start Time
    startDateTime = parser.parse(startTime.lstrip('[').rstrip(']'))
    endTime = spans[1].text # Second span should be RTS Stop Time
    endDateTime = parser.parse(endTime.lstrip('[').rstrip(']'))

    # convert GMT to Eastern Time (daylight saving IS considered)
    eastern = pytz.timezone('US/Eastern')
    return startDateTime.astimezone(eastern), endDateTime.astimezone(eastern)

def parseContent(cell):
    # check if there are multiple versions
    ver = cell.find_all('span')
    aonclick = cell.find_all('a', onclick=True)
    if len(ver) > 0 and len(aonclick) > 0:
        headerList = []
        for ele in aonclick[0].previous_siblings:
            headerList.append(ele.text.strip())
        for pele in aonclick[0].parents:
            if pele.name == 'td':
                break # we moved into previous column. Not what we needed
            for ele in pele.previous_siblings:
                headerList.append(ele.text.strip())

        # get the latest version
        word = (' '.join(reversed(headerList))).lstrip() + '\n'
        word = word + '*' * 20 + aonclick[-1].get_text(strip=True, separator='\n').replace('\t', ' ') + '*' * 20 + '\n'
        lastVer = ver[-1]
        # remove all <del> element
        for s in lastVer.select('del'):
            s.extract()
        # remove line break due to <strong> by replacing all <bn/> as line break and don't use line break as tag separator
        lastVer = BeautifulSoup(str(lastVer).replace('<br/>', '\n'), 'html.parser')
        word = word + lastVer.get_text(separator=' ').replace('\t', ' ')
    else:
        word = cell.get_text(strip=True, separator='\n').replace('\t', ' ')
    # remove empty line, remove consecutive line breaks, remove consecutive spaces
    return re.sub('\n\s*\n', '\n', re.sub('\n+', '\n', re.sub(r" +", " ", word)))  



def getAllEntriesOnDate(driver, date, timeout, pc):
    url = "https://online.star.bnl.gov/apps/shiftLog%d/logForPeriod.jsp?startDate=%d/%d/%d&endDate=%d/%d/%d&B1=Submit" % (date.year, date.month, date.day, date.year, date.month, date.day, date.year)
    entries = {}
    
    soup = BeautifulSoup(pc.getUrl(url, driver, timeout, 'ShiftLog'), "html.parser")
    tables = soup.findAll('table')
    for table  in tables:
        timestamp = date
        rows = table.find_all('tr')
        prevstamp = None
        oneSec = timedelta(seconds=1)
        for row in rows:
            # Find all cells in the row
            cells = row.find_all("td")#[-1]

            # there should be a time and content column
            if len(cells) != 2:
                continue

            # get time
            word = cells[0].get_text(strip=True)
            timeString = word[:5]
            minHr = datetime.strptime(timeString, '%H:%M')
            timestamp = timestamp.replace(hour=minHr.hour, minute=minHr.minute, second=0)

            # there could be multiple messages in a minutes
            if prevstamp == timestamp:
                timestamp = prevstamp + oneSec
                if prevstamp.minute != timestamp.minute:
                    warnings.warn('What is this? There are more than 60 entries at time %s. Who wrote this? I am ignoring 61th and beyon entries created in this minute.' % prevstamp.strftime('%B %d, %Y %H:%M'))
                    continue
            prevstamp = timestamp

            # save content
            entries[timestamp] = parseContent(cells[1])

    return entries
 
def getEntriesInRange(driver, start, end, timeout, dp, pc):
    beginTime = start
    currTime = beginTime.replace(hour=0, minute=0, second=0)
    oneDay = timedelta(days=1)
    results = {}
    while currTime <= end:
        # reduce wepage loading by dynamic programing
        if currTime in dp:
            res = dp[currTime]
        else:
            res = getAllEntriesOnDate(driver, currTime, timeout, pc)
            dp[currTime] = res
        for dt, content in res.items():
            if start <= dt and dt <= end:
                results[dt] = content
        currTime += oneDay
    return results

def getEntriesAndSummary(driver, start, end, searchWindows, 
                         deltaBefore, deltaAfter, timeout, dp, pc):
    summaryResult = None
    results = getEntriesInRange(driver, start - deltaBefore,  
                                max(end + deltaAfter, start + searchWindows), timeout, dp, pc)
    finalResults = {}
    # search for summary before current datetime
    for currTime, content in results.items():
        if start - deltaBefore <= currTime and currTime <= end + deltaAfter:
            finalResults[currTime] = content
        if summaryResult is None and content.startswith('Summary Report'):
            if currTime > end:
                summaryResult = (currTime, content)
    return finalResults, summaryResult

def printDict(runID, result):
    runStart = result.runStart
    runEnd = result.runEnd
    x = PrettyTable()
    x.hrules=ALL
    x.field_names = ['Time', 'Content']
    x._max_width = {'Time' : 10, 'Content' : 80}
    # reverse chronological order
    insertedStart = False
    insertedEnd = False
    for time, content in result.history.items():
        if runStart is not None and not insertedStart and time > runStart:
            insertedStart = True
            x.add_row([runStart.strftime('%H:%M'), 'RUN' + runID + 'START' + '8'*(70 - 8 - len(runID) if 70 - 8 - len(runID) > 0 else 1)])
        if runEnd is not None and not insertedEnd and time >= runEnd:
            insertedEnd = True
            x.add_row([runEnd.strftime('%H:%M'), 'RUN' + runID + 'END' + '8'*(70 - 6 - len(runID) if 70 - 6 - len(runID) > 0 else 1)])
        if runID in content:
            content = content.replace(runID, '>'*10 + runID + '<'*10)
        x.add_row([time.strftime('%B %d, %Y\n%H:%M'), content])
    # guarantee to have RUN<runID>START and END line if runStart and runEnd is not None
    if runStart is not None and not insertedStart:
        x.add_row([runStart.strftime('%H:%M'), 'RUN' + runID + 'START' + '8'*(70 - 8 - len(runID) if 70 - 8 - len(runID) > 0 else 1)])
    if runEnd is not None and not insertedEnd:
        x.add_row([runEnd.strftime('%H:%M'), 'RUN' + runID + 'END' + '8'*(70 - 6 - len(runID) if 70 - 6 - len(runID) > 0 else 1)])

    x.align['Content'] = 'l'
    return x.get_string()
