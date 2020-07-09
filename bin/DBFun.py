import MySQLdb


def db_connect(dbname):
    try:
        db = MySQLdb.connect(host="duvall.star.bnl.gov",port=3306,user="starreco",passwd="",db=dbname)
    except:
        print("Can't connect to database")
        return 0
    #print("Connected to database")
    try:
        cur = db.cursor()
    except AttributeError:
        print("Can't have cursor")
        return 0
    return_dictionary = {}
    return_dictionary["db"] = db
    return_dictionary["cursor"] = cur
    return return_dictionary

def db_disconnet(db):
    db.close()

def execute_query(cur, query):
    try:
        cur.execute(query)
    except:
        print("Can't execute the query!!!")
        return 0
    return cur

def execute(dbname, query):
    ret = db_connect(dbname)
    if ret!=0:
       db = ret.get("db")
       cur = ret.get("cursor")
       cur = execute_query(cur, query)
       result = cur
       db_disconnet(db)
       if cur!=0: 
          result = cur
       else:
          return 0
    else: 
       return 0
    return result

def describetable(dbname, table):
    ret = db_connect(dbname)
    if ret!=0:
       db = ret.get("db")
       cur = ret.get("cursor")
       query = "SELECT * FROM "+table+" LIMIT 0"
       cur = execute_query(cur, query)
       if cur!=0: 
          result = cur.description
       db_disconnet(db)
    else:
       return 0
    return result

def CheckForRow(dataset,value,db,cur):
    try:
       query = "select * from ProdOptions where " \
               +"prodSeries="+value.get("prodSeries")+" and " \
               +"eventType="+value.get("eventType")+" and " \
  	       +"libVersion="+value.get("libVersion")+" and " \
  	       +"geometry="+value.get("geometry")+" and " \
  	       +"chainOpt="+value.get("chainOpt")+" and " \
  	       +"chainName="+value.get("chainName")+" and " \
  	       +"calibTag="+value.get("calibTag")+" and " \
  	       +"comment="+value.get("comment")+""
       print(query)
       #cur.execute(query)
       
    except:
       print("Unable to search row in ProdOperation table!!!") 

def AddRowinTable(dataset,value):
    #incomplete
    dirt = db_connect()
    if dirt==0:
       return 0
    db = dirt.get("db")
    cur = dirt.get("cursor")
    #check for existing row
    exist = CheckForRow(dataset,value,db,cur)
    try:
        query = "INSERT INTO ProdOptions (prodSeries,eventType,libVersion,geometry,chainOpt,chainName,calibTag,comment) VALUES ("+value.get("prodSeries")+"," \
  +value.get("eventType")+"," \
  +value.get("libVersion")+"," \
  +value.get("geometry")+"," \
  +value.get("chainOpt")+"," \
  +value.get("chainName")+"," \
  +value.get("calibTag")+"," \
  +value.get("comment")+")"
        #cur.execute(query)
        print(query)
    except:
        print("Unable to add row in ProdOperation table!!!")
    db_disconnet(db) 
    return 1
