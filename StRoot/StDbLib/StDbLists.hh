#ifndef STDBLISTS_HH
#define STDBLISTS_HH

#include "StDbDefs.hh"

#include <string.h>

// Shortcut mapping of Enumerations to names in database
// These are classes but everything is public
// ..... will put into database eventually

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


