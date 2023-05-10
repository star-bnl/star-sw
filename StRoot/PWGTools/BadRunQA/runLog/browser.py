from selenium import webdriver
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.wait import WebDriverWait
import time
from pynput.keyboard import Key, Controller

def autoLogin(driver, username, password, timeout):
    """
    Using pynput to login is an unsafe and error-prone way to login
    but I can't get authenticated with the other ways I've tried
    Will change when alternatives are found
    """
    print('*' * 100)
    print('Using pre-entered credentials')
    print('IMPORTANT: KEEP THE BROWSER ON TOP DURING THE LOGIN PROCESS.')
    print('You can focus on other windows AFTER a successful login')
    keyboard = Controller()
    # login with shiftLog2019 home page
    # once you have the login session, you are all set
    # if this url fails, replace with any other shift log page
    #url = 'https://online.star.bnl.gov/apps/shiftLog2021/logForFullTextSearch.jsp?text=22031042'
    url = 'https://online.star.bnl.gov/RunLogRun21/index.php?r=22031042'

    driver.get(url)
    keyboard.type(username)
    keyboard.press(Key.tab)
    keyboard.release(Key.tab)
    keyboard.type(password)
    keyboard.press(Key.enter)
    keyboard.release(Key.enter)
    print('*' * 100)
    WebDriverWait(driver, timeout).until(EC.any_of(EC.title_is('STAR RunLog Browser'), EC.title_contains('Error'), EC.title_contains('error'), EC.title_contains('Unauthorize')))
    if 'unauthorize' in driver.title.lower():
        raise RuntimeError('Incorrect password or username')

def getDriver(firefox, timeout, username=None, password=None):
    if firefox:
        driver = webdriver.Firefox()
    else:
        driver = webdriver.Chrome()
    driver.set_page_load_timeout(timeout)
    if username is not None and password is not None: 
        # supposedly you only need to enter credientials once at the beginning
        autoLogin(driver, username, password, timeout)
    return driver

