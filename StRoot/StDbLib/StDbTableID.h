#ifndef STDBTABLEID_HH
#define STDBTABLEID_HH

#include <string.h>

class StDbTableID {

protected:

 int mtableID;
 char* mname;

public:

  StDbTableID(const char* name, int tableID): mname(0) {  setName(name);
                                                          mtableID = tableID;}
  StDbTableID(StDbTableID& t): mname(0) { setName(t.getName());
                                mtableID = t.getID();}
  ~StDbTableID() { if(mname) delete [] mname; }

  void setName(const char* name);
  char* getName() const { return strdup(mname); } 
  bool checkName(const char* name); 
  void setID(int tableID) {mtableID=tableID;}
  int getID() const { return mtableID; }

};


inline
void StDbTableID::setName(const char* name){
  if(mname) delete [] mname;
  mname = new char[strlen(name)+1];
  strcpy(mname,name);
}


inline
bool StDbTableID::checkName(const char* name){ if(strcmp(name,mname)==0)return true;
return false;
}

#endif

