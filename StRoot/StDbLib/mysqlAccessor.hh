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

public:

  mysqlAccessor() {};
   ~mysqlAccessor(){};
    
  virtual void initDbQuery(const char* dbname, const char* serverName, const char* host, const int portNumber){Db.Connect(host,"","",dbname,0);};

  // tableQuery Interface;

  virtual int QueryDb(StDbTable* table);
  virtual int WriteDb(StDbTable* table);
  virtual int QueryDb(StDbConfigNode* node);
  virtual int QueryDescriptor(StDbTable* table);
  virtual StDbBuffer* getBuffer(){return (StDbBuffer*) &buff;}; 

  virtual void freeQuery() { }; // nothing yet ...              
                           
  virtual bool execute(const char* name);

  // These should probably be protected

protected:

  virtual bool isConfig(const char* name);  // has "Keys"
  virtual char* getKeyName(const char* nodeName); // add "Keys"
  virtual char* getNodeName(const char* keyName); // strip "Keys"
  virtual int   getElementID(const char* nodeName);


};


#endif









