/***************************************************************************
 *
 * $Id: StDbNodeInfo.cc,v 1.2 2000/01/14 14:50:52 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: C++ c-struct to hold Basic database information
 *
 ***************************************************************************
 *
 * $Log: StDbNodeInfo.cc,v $
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
#include "StDbNodeInfo.hh"
#include <strstream.h>
#include <iostream.h>
#include <stdlib.h>
#include <string.h>


StDbNodeInfo::StDbNodeInfo(): name(0), versionKey(0), nodeType(0), structName(0), elementID(0), dbName(0), IsBaseLine(false), IsBinary(false), IsIndexed(true), nodeID(0), dbType(dbStDb), dbDomain(dbDomainUnknown) {};


StDbNodeInfo::StDbNodeInfo(StDbNodeInfo& node){
name=mstrDup(node.name);
versionKey=mstrDup(node.versionKey);
copyInfo(&node);
}

void
StDbNodeInfo::copyInfo(StDbNodeInfo* node){

 nodeType=mstrDup(node->nodeType);
 structName=mstrDup(node->structName);
 elementID=mstrDup(node->elementID);
 dbName=mstrDup(node->dbName);
 IsBaseLine=node->IsBaseLine;
 IsBinary=node->IsBinary;
 IsIndexed=node->IsIndexed;
 nodeID = node->nodeID;
 dbType = node->dbType;
 dbDomain = node->dbDomain;

 /*
 cout << "******* In copyInfo **********" << endl;
 cout << name << " " << versionKey << " "<< nodeType <<endl;
 cout << structName<< " " <<elementID <<" " <<dbName<< " "<<endl;
 cout << dbType<< " "<< dbDomain << endl;
 cout << "******* ****END**** **********" << endl;
 */


}

StDbNodeInfo::~StDbNodeInfo(){ deletePointers();}

void
StDbNodeInfo::deletePointers(){

 deleteInfoPointers();
 if(dbName)     {delete [] dbName; dbName=0;}
 if(name)       {delete [] name; name=0;}
 if(versionKey) {delete [] versionKey; versionKey = 0;}

}

void
StDbNodeInfo::setNodeInfo(StDbNodeInfo* node){
 deleteInfoPointers();
 copyInfo(node);
}

char*
StDbNodeInfo::mstrDup(const char* s2) {

char* s1=0;

if(!s2){
  cerr << "Attempting to copy null pointer " << endl;  
  return s1;
}
 if(!strstr(s2,"\0")) cout << "Dup ERROR ************" <<endl;
s1 = new char[strlen(s2)+1];
strcpy(s1,s2);
// cout << "duplicated "<<s2 << " into " << s1 << endl;
return s1;

}

void
StDbNodeInfo::mstrCpy(char*& s1, const char* s2) {

  if(s2){ 
    if(s1) delete [] s1;
    if(!strstr(s2,"\0")) cout << "COPY ERROR ************" <<endl;

    s1 = new char[strlen(s2)+1];
    strcpy(s1,s2);
    //  cout << " copied over " << s1 << " from " << s2 << endl;
  } else {
    cout << " Input a null pointer " << endl;
  }

}

void
StDbNodeInfo::deleteInfoPointers() {

 if(nodeType)  { delete [] nodeType; nodeType = 0;}
 if(structName){ delete [] structName; structName = 0;}
 if(elementID) { delete [] elementID;  elementID = 0;}

}


////////////////////////////////////////////////////////////////

int*
StDbNodeInfo::getElementID(const char* elemID, int& numRows) {

numRows=1;
int * retVal = 0;
char* id=strstr(elemID,"None");

if(id){
  numRows=1;
  int* e = new int[1]; *e=0;
  return e;
}

// On the to-do list:
//   now we expect list in the form 1,2,3,4, ... 
//   but we may want to also allow 1-800 and/or 1,2,6-12,15,19
//   so I should remake string so that 1-800 = 1,2,3,4,...,799,800
//
// cout << "My elementID = " << nodeName << endl;

char* tmpName = new char[strlen(elemID)+1];
strcpy(tmpName,elemID);

int numElements = 1;
id = strstr(tmpName,",");
char* id1;
char* id2;

id2 = strstr(tmpName,"-");
char islist[2048];
ostrstream sl(islist,2048);

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
 sl << ends;

 // cout << "My string list = " << islist << endl;

 int* tmpElements = new int[100000];
 char* p1=&tmpName[0];
 char* anID;
 anID = getNextID(p1);
 tmpElements[0] = atoi(anID);
 numElements = 1;
 int iEnd, iStart, k;
 for(int ient=1;ient<numEntries;ient++){
   anID = getNextID(p1);
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
   if(anID) delete [] anID;
 }

 retVal = new int[numElements];
 for(k=0;k<numElements;k++)retVal[k]=tmpElements[k];
 numRows = numElements;
 delete [] tmpElements;
 delete [] tmpName; 


return retVal;
}


////////////////////////////////////////////////////////////////

char*
StDbNodeInfo::getNextID(char*& currentElement) const {

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













