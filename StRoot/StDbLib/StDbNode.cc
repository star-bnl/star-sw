/***************************************************************************
 *
 * $Id: StDbNode.cc,v 1.4 2000/04/25 18:26:03 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: Base-class database entities
 *
 ***************************************************************************
 *
 * $Log: StDbNode.cc,v $
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
#include <iostream.h>
   
StDbNode::StDbNode(StDbNodeInfo* node) {

mnode.mstrCpy(mnode.name,node->name);
mnode.mstrCpy(mnode.versionKey,node->versionKey);
mdefaultVersion = StDbDefaults::Instance()->IsDefaultVersion(mnode.versionKey);

mnode.setNodeInfo(node);

misConfigured = true;
misNode = true;
mcanRollBack = false;

}

StDbNode::StDbNode(const char* name ){

char* version=StDbDefaults::Instance()->getVersion();
mnode.mstrCpy(mnode.name,name);
mnode.mstrCpy(mnode.versionKey,version); delete [] version;
mdefaultVersion = true;

misConfigured = false;
misNode = false;
mcanRollBack = false;

}

////////////////////////////////////////////////////////////////////


StDbNode::StDbNode(const char* name, const char* versionKey){

mnode.mstrCpy(mnode.name,name);
mnode.mstrCpy(mnode.versionKey,versionKey);
mdefaultVersion = StDbDefaults::Instance()->IsDefaultVersion(mnode.versionKey);

misConfigured = false;
misNode = false;
mcanRollBack = false;

}

////////////////////////////////////////////////////////////////////

StDbNode::StDbNode(StDbNode& node){

mnode.name = node.getName();
mnode.versionKey = node.getVersion();
mdefaultVersion = node.defaultVersion();
node.getNodeInfo(&mnode);

misConfigured = node.IsConfigured();
misNode = node.IsNode();
mcanRollBack = false;

}

/////////////////////////////////////////////////////////////

void
StDbNode::setNodeInfo(StDbNodeInfo* node) {
mnode.setNodeInfo(node);
}

/////////////////////////////////////////////////////////////

void
StDbNode::getNodeInfo(StDbNodeInfo* node){
node->deleteInfoPointers();
node->copyInfo(&mnode);
}

/////////////////////////////////////////////////////////////

char*
StDbNode::getName() { return mnode.mstrDup((const char*)mnode.name); };

char*
StDbNode::getVersion()  {  return mnode.mstrDup((const char*)mnode.versionKey); }

char*
StDbNode::getDbName()  {return mnode.mstrDup((const char*)mnode.dbName); }

char*
StDbNode::getElementID()  { return mnode.mstrDup((const char*)mnode.elementID); }

int*
StDbNode::getElementID(int& nrows) { return mnode.getElementID((const char*)mnode.elementID, nrows); }

void
StDbNode::setName(const char* nodeName){ mnode.mstrCpy(mnode.name,nodeName);}

void
StDbNode::setVersion(const char* version){ 
  mnode.mstrCpy(mnode.versionKey,version);
  mdefaultVersion=StDbDefaults::Instance()->IsDefaultVersion(mnode.versionKey);
}

void
StDbNode::setDbName(const char* nodeDbName){ mnode.mstrCpy(mnode.dbName,nodeDbName);}

void
StDbNode::setElementID(const char* elementID) { mnode.mstrCpy(mnode.elementID,elementID); }














