#include <iostream.h>
#include <strings.h>

#include "StDbConfigNode.hh"
#include "StDbManager.hh"
#include "StDbFactories.hh"
#include "TableIter.hh"
#include "StDbTableComponent.h"
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

  if(mdbType==StarDb){
    mdbType=StDbManager::Instance()->getDbType(mnodeName);
  } else if(mdbDomain==Star){
    mdbDomain=StDbManager::Instance()->getDbDomain(mnodeName);
  }

}

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

 StDbServer* server = StDbManager::Instance()->findServer(mdbType, mdbDomain);
 server->QueryDb(this);
 if(mfirstChildNode)mfirstChildNode->buildTree();
 if(mnextNode)mnextNode->buildTree();

}

/////////////////////////////////////////////////////////////////

void
StDbConfigNode::printTree(){

  /*  cout << "My Name = " << mnodeName<< " my config= "<< mconfigName << endl;
  if(mhasData){
    cout << mnodeName << " have data" << endl;
  } else {
    cout << mnodeName << " has no data" << endl;
  }
  */
 if(mfirstChildNode){
   //   cout << mnodeName << " Sends to Child" << mfirstChildNode->getName() << endl;
   mfirstChildNode->printTree();
 }
 if(mnextNode){
   //   cout << mnodeName << " Sends To Sib" << mnextNode->getName() << endl;
   mnextNode->printTree();
 }

}

///////////////////////////////////////////////////////////////

void
StDbConfigNode::addTable(const char* tableName, int version, int elementID){

  if(!mfactory){
    mfactory = StDbFactories::Instance()->getFactory(mdbType, mdbDomain);
  } else {
    cout << " already have factory " << endl;
  }

  StDbTableComponent* table = 0;
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

StDbConfigNode*
StDbConfigNode::findConfigNode(StDbType type, StDbDomain domain){

StDbConfigNode* node = 0;

  if(type == mdbType && domain == mdbDomain){
    if(mparentNode->isNode(type,domain))return mparentNode->findConfigNode(type,domain);
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
  StDbTableComponent* table;

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











