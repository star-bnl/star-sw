// example macro to use  EEmcTTMMaker
// Author: Piotr A. Zolnierczuk
#include <TSystem.h>
#include <iostream>
//
#include <StChain.h>
//
#include <St_db_Maker/St_db_Maker.h>
#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDbReader.h>
#include <StMuDSTMaker/COMMON/StMuDebug.h>
#include <StEEmcDbMaker/StEEmcDbMaker.h>
#include <StEEmcPool/TTM/EEmcTTMMaker.h>
//

void EEmcTTMMaker::Run(
	    StChain* chain, 
	    char* inpDir    ,   // MuDST directory
	    char* inpFile   ,   // MuDST file(s)
	    char* outFile   ,   //
	    Int_t nFiles    ,   // # of MuDST file(s)
	    Int_t nEvents   ,   //
	    Int_t timeStamp )   // dbase timestamp
{ 
  if(chain==NULL) { 
    std::cerr << "null chain" << std::endl;
    return;
  }
  // now we add Makers to the chain...  some of that is black magic :) 
  // muDST main chain
  StMuDstMaker   *muDstMaker = new StMuDstMaker(0,0,inpDir,inpFile,"MuDst.root",nFiles);  
  StMuDbReader   *dbReader   = StMuDbReader::instance();               // need the STAR db 
  StEEmcDbMaker  *eemcDbMaker= new StEEmcDbMaker("eemcDb");            // need the EEMC db
  St_db_Maker    *dbMk       = new St_db_Maker("StarDb", "MySQL:StarDb");  // (???)

  // now comment in/out/change the below if you want it your way
  eemcDbMaker->setSectors(1,12);           // request EEMC DB for sectors you need
  eemcDbMaker->setTimeStampDay(timeStamp); // format: yyyymmdd
  eemcDbMaker->setPreferedFlavor("onlped","eemcPMTped"); // request alternative db flavor 

  // finally after so many lines we arrive at the good stuff
  EEmcTTMMaker *ttmmk = new  EEmcTTMMaker ("TTM",muDstMaker,eemcDbMaker);
  ttmmk->SetFileName(outFile);
  ttmmk->Summary(cerr);    // 

  StMuDebug::setLevel(0);

  chain->Init();
  chain->ls(3);

  int stat=0;
  int counter=0;
  //---------------------------------------------------
  while(nEvents<0 || counter<nEvents ) {
    
    stat = chain->Make();
    if(stat==kStEOF || stat==kStFatal) break;
    // just for fun
#if 0
    switch(counter%4) {
    case  0: cout << "\\\r"; break; 
    case  1: cout << "|\r"; break; 
    case  2: cout << "-\r"; break; 
    case  3: cout << "/\r"; break; 
    default: cout << ".\r"; break; 
    }
    cout.flush();
#endif
    if(++counter%10==0) cout << "analyzed " << counter << " events" << endl;
  }
  ttmmk->Summary(std::cerr);    // 
  ttmmk->Finish();
}
