
#include "StDbManager.hh"
#include "StDbConfigNode.hh"
#include "StDbTable.h"
#include "StDbXmlReader.h"
#include "StDbXmlWriter.h"
#include <time.h>

int main(){


  StDbManager* mgr = StDbManager::Instance();
  mgr->setVerbose(true);
  StDbConfigNode* node = mgr->initConfig(dbCalibrations,dbTpc,"reconV0");

  node->setFlavor("onl");
  unsigned int rTime = (unsigned int)time(NULL);
  node->setProdTime(rTime);
  mgr->setRequestTime(rTime);
  mgr->fetchAllTables(node); 
  StDbTable* table = node->findTable("tpcElectronics");

  ofstream of("tpcElectronics.Now.xml");
  StDbXmlWriter writer(of);  
  writer.ioTable(table);

  //  mgr->setStoreTime(rTime);
  //  mgr->storeAllTables(node);

  delete node;
  delete mgr;

} 




