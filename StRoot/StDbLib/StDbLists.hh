/***************************************************************************
 *
 * $Id: StDbLists.hh,v 1.3 1999/09/30 02:06:07 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  mapping of Db-names & Db-enumerations
 *
 ***************************************************************************
 *
 * $Log: StDbLists.hh,v $
 * Revision 1.3  1999/09/30 02:06:07  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef STDBLISTS_HH
#define STDBLISTS_HH

#include "StDbDefs.hh"

#include <string.h>

class dbType{

public:

  StDbType type;
  char* name;
  dbType(StDbType atype, const char* aname){ type = atype;
                                             name = new char[strlen(aname)+1];
                                             strcpy(name,aname);}
  dbType(dbType&d){type=d.type;
                   name = new char[strlen(d.name)+1];
                   strcpy(name,d.name);}
  ~dbType(){ if(name)delete [] name; };
};

class dbDomain{

public:

  StDbDomain domain;
  char* name;
  dbDomain(StDbDomain adomain, const char* aname){ domain = adomain;
                                             name = new char[strlen(aname)+1];
                                             strcpy(name,aname);}
  dbDomain(dbDomain&d){domain=d.domain;
                   name = new char[strlen(d.name)+1];
                   strcpy(name,d.name);}
  ~dbDomain(){ if(name)delete [] name; };
};

#endif


