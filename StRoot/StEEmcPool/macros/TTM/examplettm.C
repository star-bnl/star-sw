// example macro to use  EEmcTTMMaker
// Author: Piotr A. Zolnierczuk


class StChain;
StChain      *chain     =0;


void 
loadSharedLibraries()
{
  // Dynamically link needed shared libs
  gSystem->Load("libTable");
  gSystem->Load("libPhysics");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StUtilities");        // new addition 22jul99
  gSystem->Load("StTreeMaker");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StTriggerDataMaker"); // new starting from April 2003
  gSystem->Load("StBichsel");
  gSystem->Load("StEvent");
  gSystem->Load("StEventUtilities");
  gSystem->Load("StEmcUtil");
  gSystem->Load("StTofUtil");
  gSystem->Load("StPmdUtil");
  gSystem->Load("StPreEclMaker");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StMuDSTMaker");  

  // load more libraries :)
  gSystem->Load("libmysqlclient");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");

  // load even more libraries (EEMC stuff) 
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEEmcPoolTTM");

  cout << " loading of shared libraries done" << endl;

}


void runttm(
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
    if( (stat = chain->Make()) != 0 ) break;

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
    if(++counter%100==0) cout << "analyzed " << counter << " events" << endl;
  }
  ttmmk->Summary(std::cerr);    // 
  ttmmk->Finish();
}



void examplettm(
 char* inpDir    = "",            // MuDST directory
 char* inpFile   = "ttm.lis",     // MuDST file(s)
 char* outFile   = "ttm.root",
 Int_t nFiles    = 50,            // # of MuDST file(s)
 Int_t nEvents   = -1,
 Int_t timeStamp = 20040331)
{ 
  //gErrorIgnoreLevel=1999;
  std::cerr << "<xml version=\"1.0\" >" << std::endl;
  
  loadSharedLibraries();

  // create the chain    
  chain = new StChain("StChain"); 

  EEmcTTMMaker::Run(chain,inpDir,inpFile,outFile,nFiles,nEvents,timeStamp);
  
  std::cerr << "</xml>" << endl;
}
