#ifndef STDBSERVER_HH
#define STDBSERVER_HH

#include "StDbDefs.hh"    // enumeration of type & domain

class StDbTableComponent;
class StDbConfigNode;

#include "tableQuery.hh" // includes the query

class StDbServer {

private:
  
  char* mserverName;
  char* mhostName;
  char* munixSocket;
  int   mportNumber;
  char* mdbName;

  StDbType mdbType;
  StDbDomain mdbDomain;
  char* mdomainName;
  char* mtypeName;
  bool  mconnectState;

  // will become dbQuery & tableReader not "mysqlQuery" & "mysqlReader. 

  //  mysqlQuery mquery;
  tableQuery* mdatabase; 

protected:

  void setUnixSocket(); //unixSocket is derived from name,host, & port 
  virtual void initServer();

public:

  StDbServer(StDbType type, StDbDomain domain);
  StDbServer(StDbType type, StDbDomain domain, const char* typeName, const char* domainName);
  StDbServer(const char* filename) : mserverName(0), mhostName(0), munixSocket(0), mdbName(0), mconnectState(false), mdatabase(0){}; // XML catalog file not yet implemented
  StDbServer(const char* server, const char* hostname, int port);
  virtual ~StDbServer();


  // Interface 

  virtual StDbType getDbType() { return mdbType; };
  virtual StDbDomain getDbDomain() { return mdbDomain; };
 
  virtual char* getServerName() const ;
  virtual char* getHostName() const ;
  virtual int   getPortNumber() const ;
  virtual void init() { initServer(); };
  virtual bool isconnected() { return mconnectState;};
  
  virtual void QueryDb(StDbTableComponent* table);
  virtual void QueryDb(StDbConfigNode* node);

};


#endif











