//Now in order to read microDST use the DoMicroDst.
class StChain;
  StChain *chain=0;


int rdMuDstEEmcDemo(//char* file    ="rcf1200_2301_2000evts.MuDst.root",
	  char* file    = "mc_el-pt10_0-eta1_3-a1.MuDst.root",
	  //char* file    = "rcf1200_2377_2000evts.MuDst.root",
	  Int_t nFiles  = 1, 
	  char* inDir   = "./",
	  int nEve=1)
{
  if (gClassTable->GetID("TTable") < 0)
  gSystem->Load("libStar");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StMagF");
  gSystem->Load("StUtilities");  // new addition 22jul99
  gSystem->Load("StTreeMaker");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StEvent");
  gSystem->Load("StEventUtilities"); 
  gSystem->Load("StMcEvent"); 
  gSystem->Load("StMcEventMaker"); 
  gSystem->Load("StAssociationMaker");
  gSystem->Load("StMcAnalysisMaker");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("libgeometry_Tables");
  gSystem->Load("StDaqLib");
  gSystem->Load("StEmcUtil");
  gSystem->Load("StEmcADCtoEMaker");
  gSystem->Load("StPreEclMaker");
  gSystem->Load("StEpcMaker");

  gSystem->Load("StMuDSTMaker");

// Load my maker
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcPool1");
  gSystem->Load("StEEmcDbMaker");


// create chain    
  chain = new StChain("StChain"); 
  
// Now we add Makers to the chain...   
  maker = new StMuDstMaker(0,0,inDir,file,"MuDst.root",nFiles);
  StMuDbReader* db = StMuDbReader::instance();

  
  // instantiate your test maker here 
  StEEmcDbMaker  *myMk1=new StEEmcDbMaker("eemcDb");
  St_db_Maker *dbMk = new St_db_Maker("StarDb", "MySQL:StarDb");

  myMk1->setSectors(5,8);
  myMk1->setTimeStampDay(20030814);  // format: yyyymmdd, change order !
  // myMk1->setDBname("TestScheme/emc");
  // myMk1->setPreferedFlavor("set-b","eemcPMTcal");

  StMuEEDemoMaker *m = new StMuEEDemoMaker("jasEE","MuDst");
  
  chain->Init();
  chain->ls(3);
  


  int eventCounter=0;
  int stat=0;


  //---------------------------------------------------
  while ( stat==0 ) {// loop over events
    if(eventCounter>=nEve) break;
    chain->Clear();
    stat = chain->Make();
    printf(" event# %d done\n", eventCounter++);
    
  }
  
 
  //  chain->Finish();
  
}
