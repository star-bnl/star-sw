
#include "StDbManager.hh"
#include "StDbConfigNode.hh"

int main(){


  StDbManager* mgr = StDbManager::Instance();
  //  mgr->setVerbose(true);
  StDbConfigNode* node = mgr->initConfig(dbStDb,dbStar,"reconV0");
  mgr->setRequestTime("19991210010101");
  mgr->fetchAllTables(node); 

  delete node;
  delete mgr;

} 



