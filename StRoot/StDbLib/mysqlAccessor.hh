#ifndef mysqlAccessor_HH
#define mysqlAccessor_HH

#include "tableQuery.hh" // Interface for StarDb Queries

#include "StDbTableComponent.h"
#include "StDbConfigNode.hh"
#include "mysqlTypeR.hh"
#include "mysqlTypeW.hh"
#include "mysql_utils.hh"
#include "mysqlQuery.hh"


class mysqlAccessor : public tableQuery {

  // MySQL Specific functions 

mysqlQuery  mquery;
MYSQL_RES* mresult;
MYSQL_FIELD* mfields;
MYSQL_ROW mrow;
  int mnum_fields; // for writer

  //  mysql helper

mysqlTypeR* mReader;
mysqlTypeW* mWriter;

public:

  mysqlAccessor() {mReader = new mysqlTypeR();
                   mWriter = new mysqlTypeW();};
  mysqlAccessor(mysqlQuery& query) { mReader = new mysqlTypeR();
                                     mWriter = new mysqlTypeW();
                                     mquery = query;};
  ~mysqlAccessor(){delete mReader; delete mWriter;};

    
  // MySQL Specific Functions


  virtual void initDbQuery(const char* dbname, const char* serverName, const char* host, const int portNumber){mquery.init(dbname,"dummy",host,0);};

  virtual int QueryDb(StDbTableComponent* table);
  virtual int WriteDb(StDbTableComponent* table);
  virtual int QueryDb(StDbConfigNode* node);
  typeAcceptor* getReader() { return (typeAcceptor*)mReader; };
  typeAcceptor* getWriter() { return (typeAcceptor*)mWriter; };
  virtual void freeQuery() { // nothing yet ... 
                           };
 
  virtual bool execute(const char* name);

  // These should probably be protected 

  virtual char* prepareConfigQuery(const char* configName);
  virtual char* prepareDataQuery(int timeStamp, int version);
  virtual char* prepareInitialQuery(int icount, int version);

  virtual bool isConfig(const char* name);  // has "Keys"
  virtual char* getKeyName(const char* nodeName); // add "Keys"
  virtual char* getNodeName(const char* keyName); // strip "Keys"
  virtual int   getVersionIndex(const char* val) { return atoi(val); };
  virtual int   getElementID(const char* nodeName);

  virtual int   fillMetaData(MYSQL_ROW row);

};


#endif









