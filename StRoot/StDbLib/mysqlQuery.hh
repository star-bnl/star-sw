#ifndef MYSQLQUERY_HH
#define MYSQLQUERY_HH

#include "mysql.h"
#include "mysql_com.h"
#include <iostream.h>
#include <strstream.h>
#include "enumType.hh"
#include <string.h>
//typedef int bool;
//#define true 1
//#define false 0


class mysqlQuery {

private:

MYSQL mdata;
char* mdbname; // shouldn't need this!
char* mselect;
char* mfrom;
char* mwhere;
char* minsert;
char* mset;

// note:: Create DataBase & Table is done with perl script via header file
       
char* mquery;

public:

  mysqlQuery();
  virtual ~mysqlQuery(){};
  
  virtual bool init(const char* dbname);
  virtual bool init(const char* dbname, const char* serverName, const char *hostName, int portNumber);

  virtual void setPredicateClause(const char* select, const char* from, const char* where);
  virtual MYSQL_RES* selectFromWhere(const char* select, const char* from, const char* where);
  virtual MYSQL_RES* insertSET(const char * tableName, const char* dataString);
  virtual MYSQL_RES* execute();

};


#endif




