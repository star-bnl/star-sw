#ifndef TABLEQUERY_HH
#define TABLEQUERY_HH


class StDbTableComponent;
class StDbConfigNode;
class typeAcceptor;


class tableQuery {

public:

  virtual ~tableQuery() {};

  virtual void initDbQuery(const char* dbname, const char* serverName, const char* hostName, const int portNumber) = 0;
   virtual int QueryDb(StDbTableComponent* table) = 0;
  virtual int QueryDb(StDbConfigNode* node) = 0;
 
  virtual typeAcceptor* getReader() = 0;
  virtual typeAcceptor* getWriter() = 0;
  virtual void freeQuery() = 0;

};

#endif





