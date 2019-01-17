// macro to read tof Geom Align data
//


// #include "StDbLib/StDbManager.hh"
// #include "StDbLib/StDbConfigNode.hh"
// #include "StDbLib/StDbTable.h"
// #include "StDbLib/StDbDefs.hh"

#include <iostream>
#include <fstream>
#include <string>
#include <iomanip>
using namespace std;

void readtofGeomAlign(string ZReadTime = "2029-12-31 23:59:59")
{
  //-- load dBase and Table definition libraries
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
  gSystem->Load("St_Tables.so");

  gSystem->Load("StDbLib.so");
  gSystem->Load("libStDb_Tables.so");

  const Int_t NTRAY = 94;
  const Int_t NVPDTRAY = 2;
  const Int_t NMAX = 120;

  //-- get the singleton manager
  StDbManager* dbManager = StDbManager::Instance();

  //-- connect to the db & get an empty container
  StDbConfigNode* configNode = dbManager->initConfig("Calibrations_tof");
  //string ZReadTime = time;
  dbManager->setRequestTime(ZReadTime.c_str());

  StDbTable* tofGeomAlign = configNode->addDbTable("tofGeomAlign");
  dbManager->fetchDbTable(tofGeomAlign);

  cout<< "--- Table properties: ---" << endl;
  cout<<tofGeomAlign->getVersion()<<endl;
  cout<<tofGeomAlign->getBeginDateTime()<<endl;
  cout<<tofGeomAlign->getEndDateTime()<<endl;
  cout<< "------------------------" << endl;

  tofGeomAlign_st *tofAlign = static_cast<tofGeomAlign_st*>(tofGeomAlign->GetTable());
  if (!tofAlign){
    cout << "ERROR: no such table" << endl;
    return;
  }

  cout << "Reading out from database ..." << endl;
  Int_t nRows =  tofGeomAlign->GetNRows();
  cout << "Number of rows: " << nRows << endl;
  if (nRows != NMAX) {
    cout << " WARNING: number of rows does not match " << NMAX << endl;
  }

  ofstream outData;
  outData.open("geomAlign_readback.dat");

  for(int i=0;i<nRows;i++) {
    int tray = i+1;
    cout << " trayId=" << tray
	 << " phi0=" << tofAlign[i].phi0
	 << " z0=" << tofAlign[i].z0
	 << " x0=" << tofAlign[i].x0
	 << " a0=" << tofAlign[i].angle0 << endl;
    outData << setw(6)  << tray
	    << setw(15) << tofAlign[i].phi0
	    << setw(15) << tofAlign[i].z0
	    << setw(15) << tofAlign[i].x0
	    << setw(15) << tofAlign[i].angle0
	    << endl;
  }

  outData.close();
  cout << "done." << endl;

}
