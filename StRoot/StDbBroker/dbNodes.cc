/***************************************************************************
 *
 * $Id: dbNodes.cc,v 1.1 2000/01/10 20:31:17 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: simple list of nodes (db objects)
 *
 ***************************************************************************
 *
 * $Log: dbNodes.cc,v $
 * Revision 1.1  2000/01/10 20:31:17  porter
 * modified StDbBroker to be an interface to the DB-interface, StDbLib.
 *  - old functionality is retained for the short-term & modifications
 *    are extensions
 *
 *
 **************************************************************************/

#include "dbNodes.h"


dbNodes::dbNodes(): numNodes(0), curNode(0), maxList(500), curPids(0) {

  mpids = new int[maxList];

}

////////////////////////////////////////////////////////

int
dbNodes::addNode(StDbNode* node){

 mnodes.push_back(node);
 numNodes++;

return numNodes;
}

////////////////////////////////////////////////////////

StDbNode*
dbNodes::getNode(int index){

 StDbNode* node=0;
 if(index>-1 && index<numNodes)return mnodes[index];
 return node;

}

////////////////////////////////////////////////////////

void
dbNodes::setParentID(int index){

  if(curPids==maxList){
    int newMax = 2*maxList;
    int* tmpList = new int[newMax];
    memcpy(tmpList,mpids,maxList*sizeof(int));
    delete [] mpids;
    mpids = tmpList;
    maxList = newMax;
  }
    
  mpids[curPids]=index;
  curPids++;

}

////////////////////////////////////////////////////////

int
dbNodes::getParentID(int index){

if(index>-1 && index<numNodes)return mpids[index];
return 0;
}

////////////////////////////////////////////////////////

StDbNode*
dbNodes::getParent(int index){ 

 StDbNode* node=0;
 if(index>-1 && index<numNodes)return getNode(mpids[index]);
 return node;

}

////////////////////////////////////////////////////////

StDbNode*
dbNodes::next(){

 StDbNode* node=getNode(curNode);
 curNode++;
 return node;

}











