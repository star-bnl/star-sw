/***************************************************************************
 *
 * $Id: dbNodes.h,v 1.1 2000/01/10 20:31:17 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: simple list of nodes (db objects)
 *
 ***************************************************************************
 *
 * $Log: dbNodes.h,v $
 * Revision 1.1  2000/01/10 20:31:17  porter
 * modified StDbBroker to be an interface to the DB-interface, StDbLib.
 *  - old functionality is retained for the short-term & modifications
 *    are extensions
 *
 *
 **************************************************************************/
#ifndef DBNODES_HH
#define DBNODES_HH

#include "dbNodeArray.h"
#include "StDbLib/StDbNode.hh"

#include <vector>
#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StDbNode*, allocator<StDbNode*> > nodeVec;
#else
using std::vector;
typedef vector<StDbNode*> nodeVec;
#endif


class dbNodes : public dbNodeArray {

protected:

nodeVec mnodes;
int numNodes;
int curNode;

int*   mpids;
int maxList;
int curPids;

public:

  dbNodes();
  ~dbNodes() { };

  virtual int       addNode(StDbNode* node); 
  virtual StDbNode* getNode(int index)     ;

  virtual void      setParentID(int index) ;
  virtual int       getParentID(int index) ;
  virtual StDbNode* getParent(int index)   ;

  virtual int       getNumNodes()          ;
  virtual void      reset()                ;   
  virtual StDbNode* next()                 ;       

};

inline
int dbNodes::getNumNodes(){ return numNodes; }

inline
void dbNodes::reset(){ curNode=0; }

#endif







