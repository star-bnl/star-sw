#include <iostream.h>
#include <strings.h>

#include "StDbConfigNode.hh"
#include "StDbManager.hh"
#include "StDbFactories.hh"
#include "TableIter.hh"
#include "StDbTable.h"
#include "StDbServer.hh"

////////////////////////////////////////////////////////////////

StDbConfigNode::StDbConfigNode(StDbConfigNode* parent, const char* nodeName, const char* configName){

  zeroNodes();
  mdbType = parent->getDbType();
  mdbDomain = parent->getDbDomain();
  setName(nodeName);
  setConfigName(configName);
  setParentNode(parent);

  // If StarDb Type, then name holds map to real Db type
  // while the domain may still be StarDb
  // Else if StarDb domain, then name holds map to real domain type
  // Else both type & domain are that of parent

  if(mdbType==StarDb){
    mdbType=StDbManager::Instance()->getDbType(mnodeName);
  } else if(mdbDomain==Star){
    mdbDomain=StDbManager::Instance()->getDbDomain(mnodeName);
  }

}

////////////////////////////////////////////////////////////////

StDbConfigNode::StDbConfigNode(StDbType type, StDbDomain domain, const char* nodeName, const char* configName){

  zeroNodes();
  mdbType = type;
  mdbDomain = domain;
  setName(nodeName);
  setConfigName(configName);

  // If StarDb Type, then name holds map to real Db type
  // while the domain may still be StarDb
  // Else if StarDb domain, then name holds map to real domain type
  // Else both type & domain are that of parent
  /*
  if(mdbType==StarDb){
    mdbType=StDbManager::Instance()->getDbType(mnodeName);
  } else if(mdbDomain==Star){
    mdbDomain=StDbManager::Instance()->getDbDomain(mnodeName);
  }
  */
}
////////////////////////////////////////////////////////////////

StDbConfigNode::~StDbConfigNode(){

deleteTables();
deleteChildren();
if(mconfigName) delete [] mconfigName;
if(mnodeName) delete [] mnodeName;

};



////////////////////////////////////////////////////////////////

void 
StDbConfigNode::setName(const char* name){ 

if(mnodeName) delete [] mnodeName;
mnodeName = new char[strlen(name)+1];
strcpy(mnodeName,name);

}

////////////////////////////////////////////////////////////////

void 
StDbConfigNode::setConfigName(const char* name){ 

if(mconfigName) delete [] mconfigName;
mconfigName = new char[strlen(name)+1];
strcpy(mconfigName,name);

}

////////////////////////////////////////////////////////////////

void
StDbConfigNode::buildTree(){

 StDbServer* server=0;
 TableIter* itr=0;
 StDbTableI* table=0;

 server = StDbManager::Instance()->findServer(mdbType, mdbDomain);
 if(server)server->QueryDb(this);
 itr = getTableIter();
 while(!itr->done()){
   table = itr->next();
   server->QueryDescriptor((StDbTable*)table);
 }
 if(itr)delete itr;
 if(mfirstChildNode)mfirstChildNode->buildTree();
 if(mnextNode)mnextNode->buildTree();

}

/////////////////////////////////////////////////////////////////

void
StDbConfigNode::printTree(){

 if(mfirstChildNode){
   //   cout << mnodeName << " Sends to Child" << mfirstChildNode->getName() << endl;
   mfirstChildNode->printTree();
 }
 if(mnextNode){
   //   cout << mnodeName << " Sends To Sib" << mnextNode->getName() << endl;
   mnextNode->printTree();
 }

}

////////////////////////////////////////////////////////////////////////

StDbTable*
StDbConfigNode::addTable(const char* tableName, char* version, int elementID){

  if(!mfactory)mfactory = StDbFactories::Instance()->getFactory(mdbType);

  if(!mfactory) cout << " No Factory " << endl;
  StDbTable* table = 0;
  table = mfactory->getDbTable(tableName,0);

  if(table){
    table->setVersion(version);
    table->setElementID(elementID);
    table->setDbType(mdbType);
    table->setDbDomain(mdbDomain);
    mTables.push_back(table);
    if(!mhasData)mhasData=true;
  } else {
    cout << " Could not Find table " << tableName << endl;
  }

return table;
}

////////////////////////////////////////////////////////////////

TableIter*
StDbConfigNode::getTableIter(){

  TableIter* itr = new TableIter();
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
if(hasChildren())getFirstChildNode()->deleteTree();
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

  if(type == mdbType && domain == mdbDomain){
    if(mparentNode && mparentNode->isNode(type,domain))return mparentNode->findConfigNode(type,domain);
    return this;
    }
   if(hasChildren())node=mfirstChildNode->findConfigNode(type,domain);
   if(node)return node;
   if(mnextNode)node = mnextNode->findConfigNode(type,domain);

return node;
}

////////////////////////////////////////////////////////////////

void
StDbConfigNode::resetConfig(const char* configName){

if(mfirstChildNode)mfirstChildNode->deleteTree();
if(mhasData)deleteTables();
setConfigName(configName);
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

char * 
StDbConfigNode::getName() const { return strdup(mnodeName); } 

////////////////////////////////////////////////////////////////

char * 
StDbConfigNode::getConfigName() const { return strdup(mconfigName); } 

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
////////////////////////////////////////////////////////////////











