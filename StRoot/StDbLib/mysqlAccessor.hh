/***************************************************************************
 *
 * $Id: mysqlAccessor.hh,v 1.5 1999/12/03 22:24:01 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: Storage specific SQL queries to database
 *
 ***************************************************************************
 *
 * $Log: mysqlAccessor.hh,v $
 * Revision 1.5  1999/12/03 22:24:01  porter
 * expanded functionality used by online, fixed bug in
 * mysqlAccessor::getElementID(char*), & update StDbDataSet to
 * conform to changes in Xml reader & writer
 *
 * Revision 1.4  1999/10/19 14:30:41  porter
 * modifications relevant to use with StDbBroker and future merging with
 * "params" database structure + some docs + suppressing diagnostics messages
 *
 * Revision 1.3  1999/09/30 02:06:14  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef mysqlAccessor_HH
#define mysqlAccessor_HH

#include "tableQuery.hh" // Interface for StarDb Queries

#include "StDbTable.h"
#include "StDbConfigNode.hh"
#include "MysqlDb.h"
#include "StDbBuffer.h"


class mysqlAccessor : public tableQuery {

  // MySQL Specific functions 

MysqlDb Db;
StDbBuffer buff;
unsigned int theEndTime;

public:

  mysqlAccessor():theEndTime(2145934799){ };
   ~mysqlAccessor(){};
    
  virtual void initDbQuery(const char* dbname, const char* serverName, const char* host, const int portNumber){ Db.Connect(host,"","",dbname,portNumber);};

  // tableQuery Interface;

  virtual int QueryDb(StDbTable* table, unsigned int reqTime);
  virtual int QueryDb(StDbTable* table, const char* reqTime);
  virtual int WriteDb(StDbTable* table, unsigned int storeTime);
  virtual int WriteDb(StDbTable* table, const char* storeTime);
  virtual int QueryDb(StDbConfigNode* node);
  virtual int QueryDescriptor(StDbTable* table);

  virtual StDbBuffer* getBuffer(){return (StDbBuffer*) &buff;}; 

  virtual void freeQuery() { }; // nothing yet ... 
  virtual bool execute(const char* name);
  virtual unsigned int getUnixTime(const char* time);
  virtual char* getDateTime(unsigned int time);


protected:

  virtual bool isConfig(const char* name);  // has "Keys"
  virtual char* getKeyName(const char* nodeName); // add "Keys"
  virtual char* getNodeName(const char* keyName); // strip "Keys"
  virtual int*  getElementID(char* nodeName, int& numRows);
  virtual bool  isBaseLine(const char* baseline);
  virtual char* getBoolString(bool baseline);
  virtual char* getNextID(char*& currentElement);
  virtual char* getEndDateTime() {return getDateTime(theEndTime);};
  virtual unsigned int getEndTime(){return theEndTime; };

};


#endif









