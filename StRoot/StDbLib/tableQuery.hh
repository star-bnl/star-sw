#ifndef TABLEQUERY_HH
#define TABLEQUERY_HH


class StDbTable;
class StDbConfigNode;
class StDbBuffer;

class tableQuery {

public:

  virtual ~tableQuery() {};

  virtual void initDbQuery(const char* dbname, const char* serverName, const char* hostName, const int portNumber) = 0;

  virtual int QueryDb(StDbTable* table) = 0;
  virtual int WriteDb(StDbTable* table) = 0;
  virtual int QueryDb(StDbConfigNode* node) = 0;
  virtual int QueryDescriptor(StDbTable* table) = 0;

  virtual StDbBuffer* getBuffer() = 0; 

  virtual void freeQuery() = 0;

};

#endif





