//Now in order to read microDST use the DoMicroDst.

class StChain;


StChain *chain=0;


int 
compatTTM
(
 char* inpDir  = "/star/fy2003/mudst/",
 char* inpFile = "", //"st_physics_4145010_raw_0010001.MuDst.root",
 Int_t nFiles  = 50
 )
{ 
  
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();

  StMuTimer timer;
  timer.start();
  // switch of debug messages 


  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");

  // Load my maker
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEEmcPoolTTM");

  // create chain    
  chain = new StChain("StChain"); 
  
  // Now we add Makers to the chain...   
  muDstMaker = new StMuDstMaker(0,0,inpDir,inpFile,"MuDst.root",nFiles);
  StMuDbReader* db = StMuDbReader::instance();
  
  // instantiate your maker here 
  StEEmcDbMaker  *eemcDbMaker=new StEEmcDbMaker("eemcDb");  
  St_db_Maker *dbMk = new St_db_Maker("StarDb", "MySQL:StarDb");

  
  // request DB for sectors you need (dafault:1-12)
  // myMk->setSectors(5,8);


  // to overwritte the time stamp 
  // reverse order of the above makers: first #2, then #1
  // activate the line below
  eemcDbMaker->setTimeStampDay(20030514);  // format: yyyymmdd

  // change DB-server name (if needed)
  // myMk->setDBname("TestScheme/eemc");
 
  // request alternative flavor of DB table (if needed)
  // myMk->setPreferedFlavor("set430","eemcPMTcal");

  EETowCompatMatchMaker *m = new  EETowCompatMatchMaker ("CompatTM",muDstMaker,eemcDbMaker);


  m->SetDebugLevel(kWarning);
  //cout << " track matched: " << m->GetNMatched()  ;
  StMuDebug::setLevel(0);

  chain->Init();
  chain->ls(3);

  int counter=0;
  int stat=0;
  timer.reset();
  timer.start();
  TMemStat memStat("compatTTM");
  float minSize=+1.0e38;
  float maxSize=-1.0e38;
  float curSize=0.0;
  float curUsed=0.0;
  //---------------------------------------------------
  while ( stat==0 )  {// loop over events
    //chain->Clear();
    //cerr << "file: " << chain->GetCurrentFile()->GetName() << endl;
    curUsed=memStat.Used();
    curSize=memStat.ProgSize();
    if(curSize>maxSize) maxSize=curSize;
    if(curSize<minSize) minSize=curSize;
    counter++;
    fprintf(stdout,"EVENT#%-6d   matched:%-6d  heap=%5.1f prog=%5.1f (%5.1f/%5.1f)\r",
	    counter,m->GetNMatched(),curUsed,curSize,minSize,maxSize);
    stat = chain->Make();
  }
  //chain->Finish();
  cout << endl;
  cout << "tracks matched: " << m->GetNMatched() << endl;
  if (counter) cout << "time/event " << timer.elapsedTime()/counter <<endl;
  cout << " # of events:" << counter << endl;

}
