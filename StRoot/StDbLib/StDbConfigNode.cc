/***************************************************************************
 *
 * $Id: StDbConfigNode.cc,v 1.14 2000/01/27 05:54:33 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Node (directory) to hold list of dbtables
 *
 ***************************************************************************
 *
 * $Log: StDbConfigNode.cc,v $
 * Revision 1.14  2000/01/27 05:54:33  porter
 * Updated for compiling on CC5 + HPUX-aCC + KCC (when flags are reset)
 * Fixed reConnect()+transaction model mismatch
 * added some in-code comments
 *
 * Revision 1.13  2000/01/19 20:20:04  porter
 * - finished transaction model needed by online
 * - fixed CC5 compile problem in StDbNodeInfo.cc
 * - replace TableIter class by StDbTableIter to prevent name problems
 *
 * Revision 1.12  2000/01/14 14:50:52  porter
 * expanded use of verbose mode & fixed inconsistency in
 * StDbNodeInfo::getElementID
 *
 * Revision 1.11  2000/01/10 20:37:53  porter
 * expanded functionality based on planned additions or feedback from Online work.
 * update includes:
 * 	1. basis for real transaction model with roll-back
 * 	2. limited SQL access via the manager for run-log & tagDb
 * 	3. balance obtained between enumerated & string access to databases
 * 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
 * 	5. restructured Node model for better XML support
 *
 * Revision 1.10  1999/12/28 21:31:41  porter
 * added 'using std::vector' and 'using std::list' for Solaris CC5 compilation.
 * Also fixed some warnings arising from the CC5 compiles
 *
 * Revision 1.9  1999/12/03 22:24:00  porter
 * expanded functionality used by online, fixed bug in
 * mysqlAccessor::getElementID(char*), & update StDbDataSet to
 * conform to changes in Xml reader & writer
 *
 * Revision 1.8  1999/09/30 02:06:03  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#include <iostream.h>
#include <strstream.h>
#include <strings.h>

#include "StDbConfigNode.hh"
#include "StDbManager.hh"
#include "StDbFactories.hh"
#include "StDbTableIter.hh"
#include "StDbTable.h"
#include "StDbServer.hh"

////////////////////////////////////////////////////////////////

StDbConfigNode::StDbConfigNode(StDbConfigNode* parent, const char* nodeName, const char* configName): StDbNode(*(StDbNode*)parent) {

  zeroNodes();
  //  mnode.dbType = parent->getDbType();
  //  mnode.dbDomain = parent->getDbDomain();
  setName(nodeName);
  setVersion(configName);
  setParentNode(parent);

  // If StarDb Type, then name holds map to real Db type
  // while the domain may still be StarDb
  // Else if StarDb domain, then name holds map to real domain type
  // Else both type & domain are that of parent

  if(mnode.dbType==dbStDb){
    mnode.dbType=StDbManager::Instance()->getDbType(mnode.name);
    misConfigured = false; 
  } else if(mnode.dbDomain==dbStar){
    mnode.dbDomain=StDbManager::Instance()->getDbDomain(mnode.name);
    misConfigured = false;
  }

}

////////////////////////////////////////////////////////////////

StDbConfigNode::StDbConfigNode(StDbConfigNode* parent, StDbNodeInfo* node): StDbNode(node) {

  zeroNodes();
  setParentNode(parent);

  // If StarDb Type, then name holds map to real Db type
  // while the domain may still be StarDb
  // Else if StarDb domain, then name holds map to real domain type
  // Else both type & domain are that of parent (which maybe User defined)

  if(strcmp(mnode.nodeType,"DB")==0)mnode.mstrCpy(mnode.nodeType,"Config");

  if(mnode.dbType==dbStDb){
    mnode.dbType=StDbManager::Instance()->getDbType(mnode.name);
    misConfigured = false;
  } else if(mnode.dbDomain==dbStar) {
    mnode.dbDomain=StDbManager::Instance()->getDbDomain(mnode.name);
    misConfigured = false;
  }

}

////////////////////////////////////////////////////////////////

StDbConfigNode::StDbConfigNode(StDbType type, StDbDomain domain, const char* nodeName, const char* configName): StDbNode(nodeName,configName) {

  zeroNodes();
  mnode.dbType = type;
  mnode.dbDomain = domain;
  //  setName(nodeName);
  //  setVersion(configName);

}

////////////////////////////////////////////////////////////////

StDbConfigNode::~StDbConfigNode(){

deleteTables();
deleteChildren();

};


////////////////////////////////////////////////////////////////

void
StDbConfigNode::buildTree(){

 StDbServer* server=0;
 StDbTableIter* itr=0;
 StDbTable* table=0;

 server = StDbManager::Instance()->findServer(mnode.dbType, mnode.dbDomain);
 if(server){
   char* dbName = server->getDbName();
   setDbName(dbName); delete [] dbName;
   if(!server->QueryDb(this)){
     if(StDbManager::Instance()->IsVerbose())
       cout<<"Node "<<mnode.name<<"::"<<mnode.versionKey<<" has no children or tables "<<endl;
   }
 }

 if(mhasData){
  itr = getStDbTableIter();
  while(!itr->done()){
    table = itr->next();
    server->QueryDescriptor(table);
  }
 if(itr)delete itr;
 }

 if(mfirstChildNode)mfirstChildNode->buildTree();
 if(mnextNode)mnextNode->buildTree();

}

/////////////////////////////////////////////////////////////////

void
StDbConfigNode::printTree(int depth){

  if(StDbManager::Instance()->IsVerbose()){
    for(int k=0;k<depth;k++)cout<<" ";
    cout<<"Node=" << mnode.name <<" VersionKey = " << mnode.versionKey <<endl;
   if(mhasData) {
     int depth2 = depth+4;
     printTables(depth2);
   }
  }
 int depth3=depth+4;
 if(mfirstChildNode)mfirstChildNode->printTree(depth3);
 if(mnextNode)mnextNode->printTree(depth);

}

/////////////////////////////////////////////////////////////////

void
StDbConfigNode::printTables(int depth){

  if(StDbManager::Instance()->IsVerbose()){

    char* pdepth = new char[depth+1];
    ostrstream os(pdepth,depth+1);
    for(int k=0;k<depth;k++)os<<" ";
    os<<ends;
    int* elementID;
    int nrows;

    TableList::iterator itr;
    for(itr = mTables.begin(); itr!=mTables.end(); ++itr){
      cout <<pdepth<<"Table="<<(*itr)->getMyName();
      cout <<" VersionKey = "<<(*itr)->getVersion()<<endl;
      elementID = (*itr)->getElementID(nrows);
      cout <<pdepth<<"ElementIDs = ";
      if(elementID){
        for(int j=0;j<nrows;j++)cout<<" "<<elementID[j]<<" ";
      } else {
        cout<< " None ";
      }
      cout << endl;
    }
 }

}


////////////////////////////////////////////////////////////////////////

StDbTable*
StDbConfigNode::addDbTable(const char* tableName, const char* version){
  // just like addTable but also loads the descriptor from the database

  StDbTable* table = addTable(tableName,version);
  StDbServer* server = StDbManager::Instance()->findServer(mnode.dbType, mnode.dbDomain);
  if(server)server->QueryDescriptor(table);

  return table;
}

////////////////////////////////////////////////////////////////////////

StDbTable*
StDbConfigNode::addTable(const char* tableName, const char* version){

  if(!mfactory)mfactory = StDbFactories::Instance()->getFactory(mnode.dbType);

  if(!mfactory) cout << " No Factory " << endl;
  StDbTable* table = 0;
  table = mfactory->getDbTable(tableName,0);

  if(table){
    table->setVersion((char*)version);
    table->setDbType(mnode.dbType);
    table->setDbDomain(mnode.dbDomain);
    table->setDbName(mnode.dbName);
    mTables.push_back(table);
    if(!mhasData)mhasData=true;
  } else {
    cout << " Could not Find table " << tableName << endl;
  }


return table;
}

////////////////////////////////////////////////////////////////
StDbTable*
StDbConfigNode::addTable(StDbNodeInfo* node){

  if(!mfactory)mfactory = StDbFactories::Instance()->getFactory(mnode.dbType);

  if(!mfactory) cout << " No Factory " << endl;
  StDbTable* table = 0;
  table = mfactory->getDbTable(node->name,0);

  resolveNodeInfo(node);

  if(table){
     if(node->versionKey) table->setVersion(node->versionKey);
     table->setNodeInfo(node);
     //table->setDbName(mnode.dbName);
     mTables.push_back(table);
     if(!mhasData) mhasData = true;
  } else {
    cout << " Could not Find table " << mnode.name << endl;
  }

return table;
}

 
////////////////////////////////////////////////////////////////

StDbTable*
StDbConfigNode::findTable(const char* name, const char* subPath){

if(subPath && (strcmp(subPath,"/")==0))return findLocalTable(name);

StDbConfigNode* node = findConfigNode(subPath);
if(node)return node->findLocalTable(name);

// if we got here then we can't find it
StDbTable* table=0;
return table;
}

////////////////////////////////////////////////////////////////

StDbTable*
StDbConfigNode::findLocalTable(const char* name){

  TableList::iterator itr;
  StDbTable* table=0;
    for(itr = mTables.begin(); itr!=mTables.end(); ++itr){
      if((*itr)->checkName(name)){
	table=*itr;
        break;
      }
    }

return table;
}


////////////////////////////////////////////////////////////////

void
StDbConfigNode::removeTable(StDbTable* table){
 
  if(!table)return;


  TableList::iterator itr;
  StDbTable* myTable=0;

  do {
    for(itr = mTables.begin(); itr!=mTables.end(); ++itr){
        myTable=*itr;
        if(myTable && compareTables(myTable,table)){
                 mTables.erase(itr);
                 break;
        }
        myTable=0;
    }
  } while (mTables.begin() != mTables.end());


}


////////////////////////////////////////////////////////////////

bool
StDbConfigNode::compareTables(StDbTable* tab1, StDbTable* tab2){

bool retVal=false;
// compare name & version
    char* name1=tab1->getName();
    char* version1 = tab1->getVersion();
    char* name2=tab2->getName();
    char* version2 = tab2->getVersion();

    if(!(strcmp(name1,name2)==0) || !(strcmp(version1,version2)==0)){
      delete [] name1; delete [] name2; delete [] version1; delete [] version2;
      return retVal;
    }

    // compare each row identifier
    int nRows1; int nRows2;
    int* elements1 = tab1->getElementID(nRows1);
    int* elements2 = tab2->getElementID(nRows2);
    if(nRows1 != nRows2) return retVal;
    bool check=true;
    for(int i=0;i<nRows1;i++){
      if(elements1[i] != elements2[i]){
        check = false;
        break;
      }
    }
    if(check)retVal=true;

return retVal;
}


////////////////////////////////////////////////////////////////

void
StDbConfigNode::resolveNodeInfo(StDbNodeInfo*& node){
  //
  // used to restrict a some of a table's node Information 
  // to that of the parent node's... 
  // currently baseline & elementID attributes
  // 

  node->IsBaseLine=mnode.IsBaseLine;
  // if elementID="None" then Table-Node's elementID is used
  // else it is from this parent Object.
  if(!strstr(mnode.elementID,"None"))mnode.mstrCpy(node->elementID,mnode.elementID);

}


////////////////////////////////////////////////////////////////

StDbTableIter*
StDbConfigNode::getStDbTableIter(){

  StDbTableIter* itr = new StDbTableIter();
  itr->init(this);

return itr;
}

////////////////////////////////////////////////////////////////

void
StDbConfigNode::deleteTree(){

if(hasChildren())getFirstChildNode()->deleteTree();
if(mnextNode)mnextNode->deleteTree();
if(mnextNode)delete mnextNode;
if(mfirstChildNode)delete mfirstChildNode;

}


////////////////////////////////////////////////////////////////

void
StDbConfigNode::deleteChildren(){

  if(mfirstChildNode){
    StDbConfigNode* nextChild = mfirstChildNode->getNextNode();
    delete mfirstChildNode; mfirstChildNode=0;
    StDbConfigNode* node = 0;
    while(nextChild){
        node=nextChild->getNextNode();
        delete nextChild;
        nextChild = node;
    }          

  }        

}

////////////////////////////////////////////////////////////////

StDbConfigNode*
StDbConfigNode::findConfigNode(StDbType type, StDbDomain domain, const char* subPath){
StDbConfigNode* node=findConfigNode(type,domain);
return node->findConfigNode(subPath);
}

////////////////////////////////////////////////////////////////

StDbConfigNode*
StDbConfigNode::findConfigNode(StDbType type, StDbDomain domain){

  //
  // Searches for the "highest" node of type & domain
  // e.g. if request Calibrations , tpc & node is of that
  // type & domain it checks parent 1st and if it is also
  // that type & domain returns this call to the parent node
  // .. else continue down the tree ...

StDbConfigNode* node = 0;

  if(type == mnode.dbType && domain == mnode.dbDomain){
    if(mparentNode && mparentNode->isNode(type,domain))return mparentNode->findConfigNode(type,domain);
    return this;
    }
   if(hasChildren())node=mfirstChildNode->findConfigNode(type,domain);
   if(node)return node;
   if(mnextNode)node = mnextNode->findConfigNode(type,domain);

return node;
}

////////////////////////////////////////////////////////////////

StDbConfigNode*
StDbConfigNode::findConfigNode(const char* subPath){

  // tries to find a node below (in the child sense)
  // where nodeName/nodeName/nodeName/... = subPath
  // 
  // returns null pointer if not found

  char* path = new char[strlen(subPath)+1];
  strcpy(path,subPath);

if(path[0]=='/')path++;
char* id=strstr(path,"/");
if(id){
  *id='\0';
   id++;
}

char* nextNodeName=new char[strlen(path)+1];
strcpy(nextNodeName,path);

StDbConfigNode* node=getFirstChildNode();
bool found = false;

 if(!node) found = true;
 while(!found){
   if(!node->checkName(nextNodeName)){
       node=node->getNextNode();
       if(!node) found=true;
   } else {
       found=true;
   }
 }
    

if(node && id) node=findConfigNode(id);

delete [] path;
delete [] nextNodeName;

return node;
}

////////////////////////////////////////////////////////////////

void
StDbConfigNode::resetConfig(const char* configName){

if(mfirstChildNode)mfirstChildNode->deleteTree();
if(mhasData)deleteTables();
setVersion(configName);
buildTree();

}

////////////////////////////////////////////////////////////////

void StDbConfigNode::setParentNode(StDbConfigNode* parent){

mparentNode = parent;
 if(parent->hasChildren()){
   StDbConfigNode* sib = parent->getFirstChildNode();
   sib->appendNode(this);
  } else {
  parent->setFirstChildNode(this);
  }

}

////////////////////////////////////////////////////////////////

void StDbConfigNode::appendNode(StDbConfigNode* node){

  if(mnextNode){
     mnextNode->appendNode(node);
  } else {
     mnextNode = node;
  }

}
  
////////////////////////////////////////////////////////////////

void 
StDbConfigNode::setFirstChildNode(StDbConfigNode* node){
  mfirstChildNode = node;
  
} 


////////////////////////////////////////////////////////////////

void
StDbConfigNode::deleteTables(){

  TableList::iterator itr;
  StDbTable* table;

  do {
    for(itr = mTables.begin(); itr!=mTables.end(); ++itr){
        table=*itr;
        mTables.erase(itr);
        delete table;
        break;
    }
  } while (mTables.begin() != mTables.end());

}


////////////////////////////////////////////////////////////////














