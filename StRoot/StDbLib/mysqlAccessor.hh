/***************************************************************************
 *
 * $Id: mysqlAccessor.hh,v 1.6 2000/01/10 20:37:55 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: Storage specific SQL queries to database
 *
 ***************************************************************************
 *
 * $Log: mysqlAccessor.hh,v $
 * Revision 1.6  2000/01/10 20:37:55  porter
 * expanded functionality based on planned additions or feedback from Online work.
 * update includes:
 * 	1. basis for real transaction model with roll-back
 * 	2. limited SQL access via the manager for run-log & tagDb
 * 	3. balance obtained between enumerated & string access to databases
 * 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
 * 	5. restructured Node model for better XML support
 *
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

unsigned int theEndTime;

char* mdbName;
StDbType mdbType;
StDbDomain mdbDomain;

public:

MysqlDb Db;
StDbBuffer buff;

  mysqlAccessor(StDbType type, StDbDomain domain):theEndTime(2145934799), mdbName(0), mdbType(type), mdbDomain(domain) { };
   ~mysqlAccessor();
    
  virtual int initDbQuery(const char* dbname, 
                          const char* serverName, 
                          const char* host, 
                          const int portNumber);

  // tableQuery Interface;

  virtual int QueryDb(StDbTable* table, unsigned int reqTime);
  virtual int QueryDb(StDbTable* table, const char* whereClause);
  virtual int WriteDb(StDbTable* table, unsigned int storeTime);
  virtual int WriteDb(StDbConfigNode* node, int currentID);
  virtual int QueryDb(StDbConfigNode* node);
  virtual int QueryDescriptor(StDbTable* table);

  virtual StDbBuffer* getBuffer(){return (StDbBuffer*) &buff;}; 

  // DB & timestamp helper methods

  virtual unsigned int getUnixTime(const char* time);
  virtual char* getDateTime(unsigned int time);
  virtual char* getDbName() const;


protected:

  virtual void  deleteRows(const char* tableName, int* rowID, int nrows);
  virtual bool  queryNodeInfo(StDbNodeInfo* node);
  virtual bool  readNodeInfo(StDbNodeInfo* node);
  virtual bool  storeNodeInfo(StDbNodeInfo* node);
  virtual bool  prepareNode(StDbNode* dbNode, StDbNodeInfo* node);

  virtual char* getEndDateTime() {return getDateTime(theEndTime);};
  virtual unsigned int getEndTime(){return theEndTime; };

};


#endif









