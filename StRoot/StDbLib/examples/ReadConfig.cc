
#include "StDbManager.hh"
#include "StDbConfigNode.hh"
#include "StDbTable.h"
#include "StDbXmlReader.h"
#include "StDbXmlWriter.h"

int main(){


  StDbManager* mgr = StDbManager::Instance();
  //  mgr->setVerbose(true);
  //  StDbConfigNode* node = mgr->initConfig(dbStDb,dbStar,"reconV0");
  StDbConfigNode* node = mgr->initConfig(dbGeometry,dbTpc,"reconV0");
  mgr->setRequestTime("19991210010101");
  mgr->fetchAllTables(node); 
  StDbTable* table = node->findTable("tpcDimensions");
  if(table){
    cout << table->getName() << endl;
    int* eid;
    int nrows;
    eid=table->getElementID(nrows);
    if(eid){
      cout << " ElementIDs = ";
      for(int i=0;i<nrows;i++)cout <<" "<<eid[i];
      cout<<endl;
    } else {
      cout << " No ElementIDs " << endl;
    }

  } else {
    cout << " no table found in node " << endl;
  }

  ifstream ifs("MyTpcDimensions.xml");
  StDbXmlReader reader;
  reader.readTable(ifs);
  table->StreamAccessor(&reader,true);
  int numRows = table->GetNRows();
  table->setRowNumber(0); 

  for(int k=0;k<numRows;k++)table->dbStreamer(&reader,true);


  ofstream of("My2TpcDimensions.xml");
  StDbXmlWriter writer(of);  
  writer.ioTable(table);

  delete node;
  delete mgr;

} 



