
#include "StDbManager.hh"
#include "StDbConfigNode.hh"
#include "StDbTable.h"
#include "StDbXmlReader.h"
#include "StDbXmlWriter.h"

int main(){


  StDbManager* mgr = StDbManager::Instance();
  StDbConfigNode* node = mgr->initConfig(dbGeometry,dbTpc,"reconV0");
  mgr->setRequestTime("19991210010101");
  mgr->fetchAllTables(node); 
  StDbTable* table = node->findTable("tpcDimensions");

  ofstream of("tpcDimensions.xml");
  StDbXmlWriter writer(of);  
  writer.ioTable(table);

  delete node;
  delete mgr;

} 



