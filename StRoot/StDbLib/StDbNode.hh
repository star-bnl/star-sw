/***************************************************************************
 *
 * $Id: StDbNode.hh,v 1.2 2000/01/19 20:20:06 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: Base-class database entities
 *
 ***************************************************************************
 *
 * $Log: StDbNode.hh,v $
 * Revision 1.2  2000/01/19 20:20:06  porter
 * - finished transaction model needed by online
 * - fixed CC5 compile problem in StDbNodeInfo.cc
 * - replace TableIter class by StDbTableIter to prevent name problems
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
#ifndef STDBNODE_HH
#define STDBNODE_HH

#include "StDbDefs.hh"
#include "StDbNodeInfo.hh"
#include <string.h>

class StDbNode {

protected:

StDbNodeInfo mnode;
bool misConfigured;
bool misNode;
bool mcanRollBack;



public:

   StDbNode(StDbNodeInfo* node);
   StDbNode(const char* name, const char* versionKey);
   StDbNode(StDbNode& node);

   virtual ~StDbNode() {};

   virtual void  setNodeInfo(StDbNodeInfo* node);
   virtual void  getNodeInfo(StDbNodeInfo* node);
   
   virtual char* getName() ;
   virtual char* getMyName() ;
   virtual char* getVersion() ;
   virtual char* getDbName() ;
   virtual char* getCstrName();
   virtual StDbType getDbType() const;
   virtual StDbDomain getDbDomain() const;
   virtual int getNodeID() const;
   
   virtual void  setName(const char* nodeName);
   virtual void  setVersion(const char* nodeVersion);
   virtual void  setDbName(const char* nodeDbName);
   virtual void  setDbType(StDbType type);
   virtual void  setDbDomain(StDbDomain domain);

   virtual void  setElementID(const char* elementID);
   virtual char* getElementID() ;
   virtual int*  getElementID(int& nrows) ;

   virtual void  setConfigured(bool isConfigured);
   virtual bool  IsConfigured() const ;
   virtual void  setAsNode(bool isNode);
   virtual bool  IsNode() const;
   virtual bool  canRollBack() const;
   virtual void  addWrittenNode(int dataID);
   virtual void  commit();

   virtual bool  checkName(const char* nodeName) const;
   virtual bool  checkVersion(const char* nodeVersion) const;
   virtual bool  checkNode(const char* nodeName, const char* nodeVersion) const;

   virtual bool IsBaseLine() const;
   virtual bool IsIndexed() const;
   virtual bool IsBinary() const;
   
};

inline
char* StDbNode::getMyName() { return mnode.name; }

inline 
char* StDbNode::getCstrName() { return mnode.structName; }

inline
StDbType StDbNode::getDbType() const { return mnode.dbType;}

inline
StDbDomain StDbNode::getDbDomain() const { return mnode.dbDomain; }

inline
void StDbNode::setDbType(StDbType type) { mnode.dbType=type;}

inline
void StDbNode::setDbDomain(StDbDomain domain) { mnode.dbDomain=domain; }

inline
int StDbNode::getNodeID() const { return mnode.nodeID; }

inline
void StDbNode::setConfigured(bool isConfigured){ misConfigured=isConfigured; }

inline
bool StDbNode::IsConfigured() const { return misConfigured; }

inline
void StDbNode::setAsNode(bool isNode){ misNode=isNode; }

inline
bool StDbNode::IsNode() const { return misNode; }

inline
bool StDbNode::canRollBack() const { return mcanRollBack; }

inline
void StDbNode::addWrittenNode(int dataID){
 mcanRollBack=true;
 mnode.nodeID=dataID;
}

inline
void StDbNode::commit() { mcanRollBack = false; }

inline
bool StDbNode::checkName(const char* nodeName) const {
if(mnode.name && strcmp(mnode.name,nodeName)==0)return true;
return false;
}

inline
bool StDbNode::checkVersion(const char* nodeVersion) const {
if(mnode.versionKey && strcmp(mnode.versionKey,nodeVersion)==0)return true;
return false;
}

inline
bool StDbNode::checkNode(const char* nodeName, const char* nodeVersion) const {
return (checkName(nodeName) && checkVersion(nodeVersion));
}

inline
bool StDbNode::IsBinary() const { return mnode.IsBinary; }

inline
bool StDbNode::IsBaseLine() const { return mnode.IsBaseLine; }

inline
bool StDbNode::IsIndexed() const { return mnode.IsIndexed; }

#endif








