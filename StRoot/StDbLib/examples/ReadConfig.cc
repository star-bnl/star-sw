
#include "StDbManager.hh"
#include "StDbConfigNode.hh"
#include "StDbTable.h"
#include "StDbXmlReader.h"
#include "StDbXmlWriter.h"
#include <time.h>

int main(){


  StDbManager* mgr = StDbManager::Instance();
  StDbConfigNode* node = mgr->initConfig(dbStDb,dbStar,"reconV0");


  // set timestamp
  unsigned int rTime = (unsigned int)time(NULL);
  mgr->setRequestTime(rTime);

  // request tables
  mgr->fetchAllTables(node); 

  // print out the list of tables
  mgr->setVerbose(true);
  node->printTree(0);

  delete node;
  delete mgr;

} 




