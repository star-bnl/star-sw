
#include "StDbManager.hh"
#include "StDbConfigNode.hh"
#include "StDbTable.h"
#include "StDbXmlReader.h"
#include "StDbXmlWriter.h"
#include <time.h>

int main(){


  StDbManager* mgr = StDbManager::Instance();
  StDbConfigNode* node = mgr->initConfig(dbCalibrations,dbTpc,"reconV0");

  unsigned int rTime = (unsigned int)time(NULL);
  mgr->setRequestTime(rTime);
  mgr->fetchAllTables(node); 
  StDbTable* table = node->findTable("tpcElectronics");


  if(table){
    ofstream of("tpcElectronics.Now.xml");
    StDbXmlWriter writer(of);  
    writer.ioTable(table);
  }

  delete node;
  delete mgr;

} 




