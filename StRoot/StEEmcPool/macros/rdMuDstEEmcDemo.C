//Now in order to read microDST use the DoMicroDst.
class StChain;
  StChain *chain=0;


int rdMuDstEEmcDemo(
	  char* file    = "rcf1202_2178_1000evts.MuDst.root",
	  Int_t nFiles  = 1, 
	  char* inDir   = "/star/data29/reco/pp200/pythia6_203/default/pt5/year2003/gheisha_on/trs_if/",
	  int nEve=500)
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
  assert(gSystem->Load("StEEmcUtil")==0);
  assert(gSystem->Load("StEEmcPoolmuDst")==0 );
  assert(gSystem->Load("StEEmcDbMaker")==0);


// create chain    
  chain = new StChain("StChain"); 
  
// Now we add Makers to the chain...   
  maker = new StMuDstMaker(0,0,inDir,file,"MuDst.root",nFiles);
  StMuDbReader* db = StMuDbReader::instance();

  
  // instantiate your maker here 
  //#2
  StEEmcDbMaker  *myMk=new StEEmcDbMaker("eemcDb");  
  //#1
  St_db_Maker *dbMk = new St_db_Maker("StarDb", "MySQL:StarDb");

  // request DB for sectors you need (dafault:1-12)
  myMk->setSectors(5,8);

  // overwritte the time stamp (if needed)
  // reverse order of makers: first #2, then #1
  // activate the line below
  myMk->setTimeStampDay(20030514);  // format: yyyymmdd

  // change DB-server name (if needed)
  myMk->setDBname("TestScheme/emc");
 
  // request alternative flavor of DB table (if needed)
  myMk->setPreferedFlavor("set430","eemcPMTcal");

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
