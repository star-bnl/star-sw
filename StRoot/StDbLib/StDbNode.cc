/***************************************************************************
 *
 * $Id: StDbNode.cc,v 1.11 2016/05/25 20:40:01 dmitry Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: Base-class database entities
 *
 ***************************************************************************
 *
 * $Log: StDbNode.cc,v $
 * Revision 1.11  2016/05/25 20:40:01  dmitry
 * coverity - reverse_inull
 *
 * Revision 1.10  2004/01/15 00:02:25  fisyak
 * Replace ostringstream => StString, add option for alpha
 *
 * Revision 1.9  2003/12/16 01:30:32  porter
 * additional fixes for change from ostrstream to StString that were not exposed until
 * running in online
 *
 * Revision 1.8  2003/09/16 22:44:17  porter
 * got rid of all ostrstream objects; replaced with StString+string.
 * modified rules.make and added file stdb_streams.h for standalone compilation
 *
 * Revision 1.7  2003/09/02 17:57:49  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.6  2001/02/09 23:06:25  porter
 * replaced ostrstream into a buffer with ostrstream creating the
 * buffer. The former somehow clashed on Solaris with CC5 iostream (current .dev)
 *
 * Revision 1.5  2001/01/22 18:37:57  porter
 * Update of code needed in next year running. This update has little
 * effect on the interface (only 1 method has been changed in the interface).
 * Code also preserves backwards compatibility so that old versions of
 * StDbLib can read new table structures.
 *  -Important features:
 *    a. more efficient low-level table structure (see StDbSql.cc)
 *    b. more flexible indexing for new systems (see StDbElememtIndex.cc)
 *    c. environment variable override KEYS for each database
 *    d. StMessage support & clock-time logging diagnostics
 *  -Cosmetic features
 *    e. hid stl behind interfaces (see new *Impl.* files) to again allow rootcint access
 *    f. removed codes that have been obsolete for awhile (e.g. db factories)
 *       & renamed some classes for clarity (e.g. tableQuery became StDataBaseI
 *       and mysqlAccessor became StDbSql)
 *
 * Revision 1.4  2000/04/25 18:26:03  porter
 * added flavor & production time as settable query fields in
 * table &/or node. Associated SQL updated in mysqlAccessor.
 * Flavor key supports "+" as an OR symbol.
 *
 * Revision 1.3  2000/01/19 20:20:06  porter
 * - finished transaction model needed by online
 * - fixed CC5 compile problem in StDbNodeInfo.cc
 * - replace TableIter class by StDbTableIter to prevent name problems
 *
 * Revision 1.2  2000/01/14 14:50:52  porter
 * expanded use of verbose mode & fixed inconsistency in
 * StDbNodeInfo::getElementID
 *
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

#include "StDbNode.hh"
#include "StDbDefaults.hh"
#include "stdb_streams.h"
#include <stdlib.h>

#ifdef __ROOT__
ClassImp(StDbNode)
#endif

////////////////////////////////////////////////////////////////////

StDbNode::StDbNode(const char* name, const char* versionKey):  mname(0), mversion(0), mdbName(0), mnodeID(0),mnodeType(0){

  setName(name);
  setVersion(versionKey);
  misConfigured = false;
  mcanRollBack  = false;
}

////////////////////////////////////////////////////////////////////
StDbNode::StDbNode(const char* name ): mname(0), mversion(0), mdbName(0), mnodeID(0) ,mnodeType(0){
  setName(name);
  setVersion(StDbDefaults::Instance()->printVersion());
  misConfigured = false;
  mcanRollBack  = false;
}

///////////////////////////////////////////////////////
StDbNode::StDbNode(StDbNode& node){

  mname         = node.getName();
  mversion      = node.getVersion();
  mdbName       = node.getDbName();
  mdbType       = node.getDbType();
  mdbDomain     = node.getDbDomain();
  mnodeID       = node.getNodeID();
  mnodeType     = node.getNodeType();
  misConfigured = node.IsConfigured();
  mcanRollBack  = node.canRollBack();
}

///////////////////////////////////////////////////////
StDbNode::~StDbNode(){
  if(mname)     delete [] mname;
  if(mversion)  delete [] mversion;
  if(mdbName)   delete [] mdbName;
  if(mnodeType) delete [] mnodeType;
}

/////////////////////////////////////////////////////////////

char* StDbNode::getName()     { return mstrDup((const char*)mname); };
char* StDbNode::getVersion()  {  return mstrDup((const char*)mversion); }
char* StDbNode::getDbName()   { return mstrDup((const char*)mdbName); }
char* StDbNode::getNodeType()  { return mstrDup(mnodeType); }

void  StDbNode::setName(const char* nodeName)  { 
  if(mname) delete [] mname;
   mname=mstrDup(nodeName);
}
void  StDbNode::setVersion(const char* version){ 
  if(mversion) delete [] mversion;
  mversion=mstrDup(version); 
}
void  StDbNode::setDbName(const char* dbName)  { 
  if(mdbName)delete [] mdbName;
  mdbName=mstrDup(dbName); 
}
void  StDbNode::setNodeType(const char* type) { 
  if(mnodeType) delete [] mnodeType;
  mnodeType=mstrDup(type); 
}

////////////////////////////////////////////////////////////////
int* StDbNode::decodeElementID(const char* elemID, int& numRows) {

numRows=1;
int * retVal = 0;
char* id=strstr((char*)elemID,"None");

if(id){
  numRows=1;
  int* e = new int[1]; *e=0;
  return e;
}

char* tmpName = new char[strlen(elemID)+1];
strcpy(tmpName,elemID);

int numElements = 1;
id = strstr(tmpName,",");
char* id1;
char* id2;

id2 = strstr(tmpName,"-");
StString sl;

if(id2 && ( (id && id2<id) || !id)){
  id=id2;
  id[0]=',';
  sl<<"r";
} else {
  sl<<"l";
}

int numEntries = 1;
if(id)id++;
while(id){
  //  cout << "id = " << id << endl;
   numEntries++;
   id1=strstr(id,",");
   id2=strstr(id,"-");
   id = id1;
   if(id && id2 && id2<id){
       id=id2;
       id[0]=',';
       sl<<"r";
   } else {
       sl<<"l";
   }
   if(id)id++;
}
 char* islist=new char[strlen((sl.str()).c_str())+1];
 strcpy(islist,(sl.str()).c_str());

 // cout << "My string list = " << islist << endl;

 int* tmpElements = new int[100000];
 char* p1=&tmpName[0];
 char* anID=0;
 anID = getNextID(p1);
 if ( anID ) {
   tmpElements[0] = atoi(anID);
   delete [] anID;
 }
 numElements = 1;
 int iEnd, iStart, k;
 for(int ient=1;ient<numEntries;ient++){
   anID = getNextID(p1);
   if ( anID ) {
   	if(islist[ient-1]=='r'){
     iEnd = atoi(anID);
     iStart = tmpElements[numElements-1];
     int irange=iEnd-iStart;
     for(int ir=1;ir<=irange;ir++){
       numElements++;
       tmpElements[numElements-1]=iStart+ir;
     }
   	} else {
     numElements++;
     tmpElements[numElements-1]=atoi(anID);
   	}
   	delete [] anID;
   }
 }

 retVal = new int[numElements];
 for(k=0;k<numElements;k++)retVal[k]=tmpElements[k];
 numRows = numElements;
 
 delete [] islist;
 delete [] tmpElements;
 delete [] tmpName; 

return retVal;
}

////////////////////////////////////////////////////////////////
char*
StDbNode::getNextID(char*& currentElement) const {

char* nextID = 0;
if(!currentElement)return nextID;

char* id = strstr(currentElement,",");

if(!id) {
  nextID = new char[strlen(currentElement)+1];
  strcpy(nextID,currentElement);
  currentElement = 0;
} else {
  int iloc = id-currentElement;
  nextID = new char[iloc+1];
  strncpy(nextID,currentElement,iloc);
  nextID[iloc]='\0';
  currentElement = id; currentElement++;
}

return nextID;
}

////////////////////////////////////////////////////////////////
char* StDbNode::mstrDup(const char* s2) {

  char* s1=0;
  if(!s2) return s1;
  s1 = new char[strlen(s2)+1];
  strcpy(s1,s2);
return s1;
}













