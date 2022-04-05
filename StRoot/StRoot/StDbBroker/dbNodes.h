/***************************************************************************
 *
 * $Id: dbNodes.h,v 1.5 2001/01/22 18:40:25 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: simple list of nodes (db objects)
 *
 ***************************************************************************
 *
 * $Log: dbNodes.h,v $
 * Revision 1.5  2001/01/22 18:40:25  porter
 * Added a wrapper for StMessage so one can use it in StDbLib
 *
 * Revision 1.4  2000/01/27 20:30:40  porter
 * cleaned up dtor & error logic
 *
 * Revision 1.3  2000/01/27 05:56:03  porter
 * update for compiling on CC5+HPUX-aCC+KCC
 *
 * Revision 1.2  2000/01/14 14:49:10  porter
 * set verbose level for checking, added $Id & $Logs, & made node container
 * more robust for interactions with StDbLib
 *
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
#if !defined(ST_NO_NAMESPACES)
using std::vector;
#endif

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StDbNode*, allocator<StDbNode*> > nodeVec;
#else
typedef vector<StDbNode*> nodeVec;
#endif


class dbNodes : public dbNodeArray {

protected:

nodeVec mnodes;
int numNodes;
int curNode;

int*   mpids;
int maxList;

  void extendParentList();

public:

  dbNodes();
  ~dbNodes() { deleteLists(); }

  virtual void deleteLists();

  virtual int       addNode(StDbNode* node, int parentID); 
  virtual StDbNode* getNode(int index)     ;

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

inline
void dbNodes::extendParentList() {
    int newMax = 2*maxList;
    int* tmpList = new int[newMax];
    memcpy(tmpList,mpids,maxList*sizeof(int));
    delete [] mpids;
    mpids = tmpList;
    maxList = newMax;
}
 

#endif







