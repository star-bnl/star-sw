/***************************************************************************
 *
 * $Id: StDbConfigNode.cc,v 1.29 2012/03/16 19:36:18 dmitry Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Node (directory) to hold list of dbtables
 *
 ***************************************************************************
 *
 * $Log: StDbConfigNode.cc,v $
 * Revision 1.29  2012/03/16 19:36:18  dmitry
 * converted dangled char pointers to std::string objects + fixed typo
 *
 * Revision 1.28  2012/03/16 17:36:46  dmitry
 * added * symbol to global override indicators
 *
 * Revision 1.27  2012/03/14 18:12:45  dmitry
 * Added protection against zero-sized strings and whitespaces sent as dbType. Null pointer was expected to indicate omitted dbType, but now StDbLib code receives \0 string instead (from upstream, checked St_db_Maker, StDbBroker, StDbConfigNode).
 *
 * Revision 1.26  2011/11/28 17:03:08  dmitry
 * dbv override support in StDbLib,StDbBroker,St_db_Maker
 *
 * Revision 1.25  2004/01/15 00:02:25  fisyak
 * Replace ostringstream => StString, add option for alpha
 *
 * Revision 1.24  2003/09/16 22:44:17  porter
 * got rid of all ostrstream objects; replaced with StString+string.
 * modified rules.make and added file stdb_streams.h for standalone compilation
 *
 * Revision 1.23  2003/09/02 17:57:49  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.22  2003/01/10 04:19:20  porter
 * added feature of getting timestamp list (but no data) for a table.
 * fixed 2 features sometimes used in online in query-by-whereclause.
 * removed a stray 'cout' in a routine that is rarely accessed
 *
 * Revision 1.21  2001/10/26 16:35:28  porter
 * improved directory search
 *
 * Revision 1.20  2001/01/22 18:37:52  porter
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
 * Revision 1.19  2000/06/30 01:57:01  porter
 * fixed a delete bug & small memory leak found by Akio via Insure++ ,
 * updated SetTable() method for containing idList, corrected enumeration
 * map to rhic domain for Conditions_rhic database
 *
 * Revision 1.18  2000/04/25 18:26:02  porter
 * added flavor & production time as settable query fields in
 * table &/or node. Associated SQL updated in mysqlAccessor.
 * Flavor key supports "+" as an OR symbol.
 *
 * Revision 1.17  2000/03/28 17:03:18  porter
 * Several upgrades:
 * 1. configuration by timestamp for Conditions
 * 2. query by whereClause made more systematic
 * 3. conflict between db-stored comments & number lists resolved
 * 4. ensure endtime is correct for certain query falures
 * 5. dbstl.h->handles ObjectSpace & RogueWave difference (Online vs Offline)
 *
 * Revision 1.16  2000/03/01 20:56:15  porter
 * 3 items:
 *    1. activated reConnect for server timeouts
 *    2. activated connection sharing; better resource utilization but poorer
 *       logging
 *    3. made rollback method in mysqlAccessor more robust (affects writes only)
 *
 * Revision 1.15  2000/02/15 20:27:44  porter
 * Some updates to writing to the database(s) via an ensemble (should
 * not affect read methods & haven't in my tests.
 *  - closeAllConnections(node) & closeConnection(table) method to mgr.
 *  - 'NullEntry' version to write, with setStoreMode in table;
 *  -  updated both StDbTable's & StDbTableDescriptor's copy-constructor
 *
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

#include "stdb_streams.h"
#include <string.h>

#include "StDbConfigNode.hh"
#include "StDbManager.hh"
#include "StDbTable.h"

#ifdef __ROOT__
ClassImp(StDbConfigNode)
#endif

#define __CLASS__ "StDbConfigNode"

////////////////////////////////////////////////////////////////

StDbConfigNode::StDbConfigNode(StDbConfigNode* parent, const char* nodeName, const char* configName): StDbNode(nodeName,configName) {

  zeroNodes();
  setParentNode(parent);
  setDbName(parent->printDbName());
  setDbType(parent->getDbType());
  setDbDomain(parent->getDbDomain());
}

////////////////////////////////////////////////////////////////
StDbConfigNode::StDbConfigNode(StDbConfigNode* parent, StDbNode& node): StDbNode(node) {
  zeroNodes();
  setParentNode(parent);
}

////////////////////////////////////////////////////////////////
StDbConfigNode::StDbConfigNode(StDbType type, StDbDomain domain, const char* nodeName, const char* configName): StDbNode(nodeName,configName) {

  zeroNodes();
  mdbType = type;
  mdbDomain = domain;
}

////////////////////////////////////////////////////////////////
StDbConfigNode::~StDbConfigNode(){ deleteChildren();};

///////////////////////////////////////////////////////////////
void StDbConfigNode::zeroNodes() {
  mfirstChildNode=0;
  mnextNode=0;
  mparentNode=0;
  mhasData=false;
  mbranchID=0;
  misDbNode=false;
};


////////////////////////////////////////////////////////////////
void
StDbConfigNode::setFlavor(const char* flavor){

  setTablesFlavor(flavor);
  if(mfirstChildNode)mfirstChildNode->setFlavor(flavor);
  if(mnextNode)mnextNode->setFlavor(flavor);

}

////////////////////////////////////////////////////////////////
void
StDbConfigNode::setProdTime(unsigned int ptime){
  setTablesProdTime(ptime);
  if(mfirstChildNode)mfirstChildNode->setProdTime(ptime);
  if(mnextNode)mnextNode->setProdTime(ptime);
}

////////////////////////////////////////////////////////////////                                                                                                        
void                                                                                                                                                                    
StDbConfigNode::setProdTimeOverride(unsigned int ptime, char* dbType, char* dbDomain) {
    bool isAllTypesAccepted = false;

    // ***** get complete name for user-selected override *****
    std::string completeName;
    if (dbType && dbType[0] != '\0' && dbType[0] != ' ' && dbType[0] != '*') {
        completeName.append(dbType);
    } else {
        isAllTypesAccepted = true;
    }
    completeName.append("_");
    if (dbDomain && dbDomain[0] != '\0' && dbDomain[0] != ' ' && dbDomain[0] != '*') {
        completeName.append(dbDomain);
    }

    // ***** get current name for a node ******
    std::string currentName;
    if (!isAllTypesAccepted) {
        // strict check, dbType is provided
        currentName  = StDbManager::Instance()->printDbName( this->getDbType(), this->getDbDomain() ) ;
    } else {
        // dbType is not provided, check domainName only
        currentName.append("_");
        currentName.append( StDbManager::Instance()->getDbDomainName( this->getDbDomain() ) ) ;
    }

    // ***** compare names, override if needed *****
    if (completeName == currentName) {
        //std::cout << "complete = current = " << completeName <<", overriding\n";
        setTablesProdTimeOverride(ptime, dbType, dbDomain);
    }

    if (mfirstChildNode) {
        //std::cout << "first child node: " << StDbManager::Instance()->printDbName( mfirstChildNode->getDbType(), mfirstChildNode->getDbDomain() ) << "\n";
        mfirstChildNode->setProdTimeOverride(ptime, dbType, dbDomain);
    }

    if (mnextNode) {
        //std::cout << "next node: " << StDbManager::Instance()->printDbName( mnextNode->getDbType(), mnextNode->getDbDomain() ) << "\n";
        mnextNode->setProdTimeOverride(ptime, dbType, dbDomain);
    }
}

////////////////////////////////////////////////////////////////

void
StDbConfigNode::deleteTree(){

 if(mfirstChildNode)delete mfirstChildNode;
 if(mnextNode){
   mnextNode->deleteTree();
   delete mnextNode;
 }
}

////////////////////////////////////////////////////////////////
void
StDbConfigNode::deleteChildren(){

  StDbConfigNode* nextChild = mfirstChildNode;
  while(nextChild){
    StDbConfigNode* node=nextChild->getNextNode();
    delete nextChild;
    nextChild=node;
  }
  mfirstChildNode=0; 
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

  // Searches for the "highest" node of type & domain
  // e.g. if request Calibrations , tpc & node is of that
  // type & domain it checks parent 1st and if it is also
  // that type & domain returns this call to the parent node
  // .. else continue down the tree ...

StDbConfigNode* node = 0;

// search parent & this 
 if(isNode(type,domain)){
    if(mparentNode && mparentNode->isNode(type,domain))
      return mparentNode->findConfigNode(type,domain);
    return this;
 }

// search children & sibs
   if(hasChildren())node=mfirstChildNode->findConfigNode(type,domain);
   if(!node && mnextNode)node = mnextNode->findConfigNode(type,domain);

return node;
}

////////////////////////////////////////////////////////////////

StDbConfigNode*
StDbConfigNode::findConfigNode(const char* subPath){

  // tries to find a node below (in the child sense)
  // where nodeName/nodeName/nodeName/... = subPath
  // 
  // returns null pointer if not found

  StDbConfigNode* node=0;
  if(!subPath) return node;
  char* path = mstrDup(subPath); 

if(path[0]=='/')path++;
char* id=strstr(path,"/");
if(id){  *id='\0';   id++; }
char* nextNodeName=mstrDup(path);   
node=getFirstChildNode();

if(node){
 bool found = false;
 while(!found){
   if(!node->checkName(nextNodeName)){
       node=node->getNextNode();
       if(!node) found=true;
   } else {
       found=true;
   }
 }

}

if(node && id) { 
  node=node->findConfigNode(id);
  *id='/'; // make path whole again
}

delete [] path;
delete [] nextNodeName;

return node;
}

////////////////////////////////////////////////////////////////
StDbConfigNode*
StDbConfigNode::findChildConfigNode(const char* name){

  StDbConfigNode* retVal=0;
  if(!name) return retVal;
  retVal=getFirstChildNode();
  while(retVal){
    if(retVal->checkName(name)) break;
    retVal=retVal->getNextNode();
  }
  return retVal;
}

////////////////////////////////////////////////////////////////
void StDbConfigNode::setParentNode(StDbConfigNode* parent){
mparentNode = parent;
parent->setChildNode(this);
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
StDbConfigNode::setChildNode(StDbConfigNode* node){
  if(mfirstChildNode) {
    mfirstChildNode->appendNode(node);
  } else {
    mfirstChildNode = node;
  }
} 

////////////////////////////////////////////////////////////////
void 
StDbConfigNode::setFirstChildNode(StDbConfigNode* node){
  if(mfirstChildNode)node->appendNode(mfirstChildNode);
  mfirstChildNode = 0;
  setChildNode(node);
} 


/////////////////////////////////////////////////////////////////

void
StDbConfigNode::printTree(int depth){

if(!depth)cout<<endl<<"************* Node Structure ****************" <<endl<<endl;
 
  if(StDbManager::Instance()->IsVerbose()){
    for(int k=0;k<depth;k++)cout<<" ";
    cout<<"Node=" << mname <<" VersionKey = " << mversion <<endl;
   if(mhasData) {
     int depth2 = depth+4;
     printTables(depth2);
   }
  }
 int depth3=depth+4;
 if(mfirstChildNode)mfirstChildNode->printTree(depth3);
 if(mnextNode)mnextNode->printTree(depth);

 if(!depth)cout<<endl<<"*********************************************" << endl;

}
#undef __CLASS__
