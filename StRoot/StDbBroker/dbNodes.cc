/***************************************************************************
 *
 * $Id: dbNodes.cc,v 1.4 2016/05/24 17:44:16 dmitry Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description: simple list of nodes (db objects)
 *
 ***************************************************************************
 *
 * $Log: dbNodes.cc,v $
 * Revision 1.4  2016/05/24 17:44:16  dmitry
 * first batch of fixes for Coverity findings
 *
 * Revision 1.3  2000/01/27 20:30:40  porter
 * cleaned up dtor & error logic
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

#include "dbNodes.h"


dbNodes::dbNodes(): numNodes(0), curNode(0), maxList(500) {

  mpids = new int[maxList];

}

////////////////////////////////////////////////////////
void
dbNodes::deleteLists() {

  if(mpids){
    delete [] mpids;
    mpids = 0;
  }
  
  mnodes.clear();

}
////////////////////////////////////////////////////////

int
dbNodes::addNode(StDbNode* node, int parentID){

 mnodes.push_back(node);
 if(numNodes==maxList) extendParentList();
 mpids[numNodes]=parentID;
 numNodes++;

return numNodes-1;
}

////////////////////////////////////////////////////////

StDbNode*
dbNodes::getNode(int index){

 StDbNode* node=0;
 if(index>-1 && index<numNodes)return mnodes[index];
 return node;

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















