/***************************************************************************
 *
 * $Id: tableQuery.hh,v 1.4 1999/11/19 21:58:07 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: Abstract class for Storage specific SQL queries
 *
 ***************************************************************************
 *
 * $Log: tableQuery.hh,v $
 * Revision 1.4  1999/11/19 21:58:07  porter
 * added method to return "malloc'd" version of table instead of new
 * so that delete of St_Table class i done correctly
 *
 * Revision 1.3  1999/09/30 02:06:15  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef TABLEQUERY_HH
#define TABLEQUERY_HH


class StDbTable;
class StDbConfigNode;
class StDbBuffer;

class tableQuery {

public:

  tableQuery() {};
  virtual ~tableQuery() {};

  virtual void initDbQuery(const char* dbname, const char* serverName, const char* hostName, const int portNumber) = 0;

  virtual int QueryDb(StDbTable* table, unsigned int reqTime) = 0;
  virtual int WriteDb(StDbTable* table, unsigned int storeTime) = 0;
  virtual int QueryDb(StDbTable* table, const char* reqTime) = 0;
  virtual int WriteDb(StDbTable* table, const char* storeTime) = 0;
  virtual int QueryDb(StDbConfigNode* node) = 0;
  virtual int QueryDescriptor(StDbTable* table) = 0;

  virtual unsigned int getUnixTime(const char* time) = 0;
  virtual char* getDateTime(unsigned int time) = 0;

  virtual StDbBuffer* getBuffer() = 0; 

  virtual void freeQuery() = 0;

};

#endif





