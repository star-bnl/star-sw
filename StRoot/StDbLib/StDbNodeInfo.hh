/***************************************************************************
 *
 * $Id: StDbNodeInfo.hh,v 1.1 2000/01/10 20:37:54 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: C++ c-struct to hold Basic database information
 *
 ***************************************************************************
 *
 * $Log: StDbNodeInfo.hh,v $
 * Revision 1.1  2000/01/10 20:37:54  porter
 * expanded functionality based on planned additions or feedback from Online work.
 * update includes:
 * 	1. basis for real transaction model with roll-back
 * 	2. limited SQL access via the manager for run-log & tagDb
 * 	3. balance obtained between enumerated & string access to databases
 * 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
 * 	5. restructured Node model for better XML support
 *
 *
 ***************************************************************************/


#ifndef STDBNODEINFO_HH
#define STDBNODEINFO_HH

#include "StDbDefs.hh"

class StDbNodeInfo {

protected:

  void deletePointers();
  char* getNextID(char*& currentElement) const;

public:
  StDbNodeInfo();
  ~StDbNodeInfo();
  StDbNodeInfo(StDbNodeInfo& node);

  void  setNodeInfo(StDbNodeInfo* node); // quick way to reset all values.
  void  deleteInfoPointers(); // quick delete of db-information
  void  copyInfo(StDbNodeInfo* node);

  char* mstrDup(const char* s2) ;  // strdup isn't ANSI
  void  mstrCpy(char*& s1, const char* s2) ;
  int*  getElementID(const char* elementID, int& numRows) ;


char*      name;
char*      versionKey;
char*      nodeType;
char*      structName;
char*      elementID;
char*      dbName;
bool       IsBaseLine;
bool       IsBinary;
bool       IsIndexed;
int        nodeID;
StDbType   dbType;
StDbDomain dbDomain;

};

#endif












