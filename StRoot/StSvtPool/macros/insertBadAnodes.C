//
// macro to transfer bad anodes data from ASCII file to data base
//

#define NUMBER_OF_HYBRIDS 432
#include <fstream.h>
#include "/afs/rhic/star/packages/DEV/StRoot/StDbLib/StDbDefs.hh"

void insertBadAnodes(char* unixTime = 0, Bool_t write = kFALSE)
{
  Bool_t debug = kTRUE;

  // DB-specific libs
  gSystem->Load("libStDb_Tables.so");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker"); 
  gSystem->Load("St_base"); 
  gSystem->Load("StUtilities"); 
  gSystem->Load("StSvtClassLibrary"); 

  // input file      
  ifstream fileBad("badAnodes_01_21_04.txt");

  // out test file
  if (debug)  
    ofstream fileout("outBadAnodesTest.txt");
  
  int barrel, ladder, wafer, hybrid;
  int index, anode;

  // define IDs
  int* rowIDs=new int[NUMBER_OF_HYBRIDS];

  StSvtConfig* config = new StSvtConfig();
  config->setConfiguration("FULL");

  // define new table to be stored
  svtBadAnodes_st *badAnodes = new svtBadAnodes_st[NUMBER_OF_HYBRIDS];    

  for (int i=0;i<432;i++) {
    rowIDs[i]=i;
    for (int j=0;j<240;j++)
      badAnodes[i].isBadAnode[j] = 0;
  }

  while (kTRUE) {

    // read bad anodes file
    fileBad >> barrel >> ladder >> wafer >> hybrid >> anode;

    index = config->getHybridIndex(barrel, ladder, wafer, hybrid);

    if (fileBad.eof()) break;

    badAnodes[index].isBadAnode[anode-1] = 1;
  }

  if (debug)  
    for (int i=0;i<432;i++)
      for (int j=0;j<240;j++) {
	if (badAnodes[i].isBadAnode[j])
	  fileout << i << " " << j+1 << endl;
      }

  if (write) {
    StDbManager* mgr=StDbManager::Instance();
    StDbConfigNode* svtCalibNode = mgr->initConfig(dbCalibrations,dbSvt);           StDbTable* svtCalibTable = svtCalibNode->addDbTable("svtBadAnodes");    
    svtCalibTable->SetTable((char*)badAnodes,NUMBER_OF_HYBRIDS,rowIDs);
    mgr->setStoreTime(unixTime);
    cout<<" Will attempt store with timestamp="<<mgr->getDateStoreTime()<<endl;    mgr->storeDbTable(svtCalibTable);
    delete [] badAnodes;
  }

}
