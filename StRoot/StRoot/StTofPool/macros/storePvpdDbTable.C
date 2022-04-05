// $Id: storePvpdDbTable.C,v 1.1 2003/12/16 15:30:26 geurts Exp $
// macro fills the pVPD Config and StrobeDef dbase tables
//  - pvpdConfig is a 1-row table and 4 distinct time periods
//  - pvpdStrobeDef has 6 rows and 2 time periods.
//
// based on
//  http://www.star.bnl.gov/STAR/comp/db/StoreDbTable.cc.html
//
// fg. Aug 2003 
//
// $Log: storePvpdDbTable.C,v $
// Revision 1.1  2003/12/16 15:30:26  geurts
// *** empty log message ***
//
//
#include <iostream.h>

void storePvpdDbTable()
{
  //-- load dBase and Table definition libraries
  gSystem->Load("StDbLib.so");
  gSystem->Load("libStDb_Tables.so");

  //-- get the singleton manager
  StDbManager* dbManager = StDbManager::Instance();

  //-- connect to the db & get an empty container
  StDbConfigNode* configNode = dbManager->initConfig("Calibrations_tof");


  //----------------------------------------
  //-- fill the pVPD config structures (single row)
  const Int_t nCfgs = 4;
  TString cfgStoreTime[nCfgs];

  //-- add table to the container with descriptor given by Database
  StDbTable* configTable = configNode->addDbTable("pvpdConfig");

  //-- fill structures & store times
  pvpdConfig_st b[nCfgs];

  //- y2 AuAu
  b[0].nEastTubes=3;
  b[0].nWestTubes=3;
  b[0].eastwestCoincidence=1;
  b[0].minEastTubes=2;
  b[0].minWestTubes=2;
  cfgStoreTime[0] = "2001-07-01 00:00:00";
  //- y2 pp
  b[1].nEastTubes=3;
  b[1].nWestTubes=3;
  b[1].eastwestCoincidence=0;
  b[1].minEastTubes=1;
  b[1].minWestTubes=1;
  cfgStoreTime[1] = "2001-11-27 00:00:00";
  //- y3 dAu
  b[2].nEastTubes=3;
  b[2].nWestTubes=3;
  b[2].eastwestCoincidence=1;
  b[2].minEastTubes=1;
  b[2].minWestTubes=1;
  cfgStoreTime[2] = "2003-01-06 00:00:00";
  //- y3 pp
  b[3].nEastTubes=3;
  b[3].nWestTubes=3;
  b[3].eastwestCoincidence=1;
  b[3].minEastTubes=1;
  b[3].minWestTubes=1;
  cfgStoreTime[3] = "2003-03-24 12:00:00";

  //-- run over all configuration entries
  for (int i=0;i<nCfgs;i++){
    //- store data in the table
    configTable->SetTable((char*)&b[i],1);
    //- set store time
    dbManager->setStoreTime(cfgStoreTime[i].Data());
    //- store table in dBase
    dbManager->storeDbTable(configTable);
  }


  //----------------------------------------
  //-- fill the strobe definition structures (multiple rows)
  const Int_t nStrobeDefs = 2;
  const Int_t nTubes = 6;
  TString strobeStoreTime[nStrobeDefs];

  //-- add table to the container with descriptor given by Database
  StDbTable* strobeDefTable = configNode->addDbTable("pvpdStrobeDef");


  //-- all tubespVPD TDCs have the strobe peak at about the same place
  //   therefore use similar (wider) range.
  //                         -y2-   -y3-
  int tdcMin[nStrobeDefs] = {1600,  980};
  int tdcMax[nStrobeDefs] = {1650, 1020};

  strobeStoreTime[0] = "2001-07-01 00:00:00"; strobeStoreTime[1] = "2003-01-06 00:00:00";
 

  //-- run over all definition entries, fill structures and store tables
  pvpdStrobeDef_st *d = new pvpdStrobeDef_st[nTubes];
  for (int i=0;i<nStrobeDefs;i++){
    //- prepare and fill all table rows (6,nTubes)
    for (int t=0;t<nTubes;t++){
      d[t].id=t+1;
      d[t].strobeTdcMin=tdcMin[i];
      d[t].strobeTdcMax=tdcMax[i];
    }
    //- store data in table
    strobeDefTable->SetTable((char*)d,nTubes);
    //  set store time
    dbManager->setStoreTime(strobeStoreTime[i].Data());
    //-  store table in dBase
    dbManager->storeDbTable(strobeDefTable);
  }
}

