#include "StDbManager.hh"
#include "StDbConfigNode.hh"
#include "StDbTable.h"
#include "StDbXmlReader.h"
#include "StDbXmlWriter.h"
#include "StDbElementIndex.hh"
#include <time.h>

int main(){
 
  StDbManager* mgr = StDbManager::Instance();
  //--> mgr->setVerbose(true); //option for printing all SQL statements 

  //--> get structure
  StDbConfigNode* node = mgr->initConfig(dbStDb,dbStar,"reconV0");

  //--> select via timestamp (here it is simply 'now')
  mgr->setRequestTime((unsigned int)time(NULL));

  //--> get all data for this timestamp
  mgr->fetchAllTables(node);

  //--> print the structure (not the data)
  mgr->setVerbose(true);
  node->printTree(0);
  mgr->setVerbose(false);

  //--> print out the data-volume & # of tables
  node->printNumberStats();

  //--> print the clock time for all queries
  mgr->printTimeStats();

  //--> clean up the memory
  delete node;
  delete mgr;

} 




