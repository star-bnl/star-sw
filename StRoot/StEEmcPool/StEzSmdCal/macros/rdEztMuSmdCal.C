TObjArray  *HList;
//cucu200_minB=66007, cucu62_minB=76007


int rdEztMuSmdCal( int trigID=0,
		   int nEve=1000,
		   Int_t nFiles  = 20,
		   char* file="lis/R6049129.lis", //#127=EHT6, #129=minB  
		   char* inDir   = "./"
		   ){ 

  //  inDir="../0x/";
  //file="st_physics_6034014_raw_1010001.MuDst.root";

  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  cout << " loading done " << endl;

  assert( !gSystem->Load("StEEmcUtil")); 
  assert( !gSystem->Load("StEzSmdCal"));


  // libraries below are needed for DB interface
  assert( !gSystem->Load("StDbBroker")); 
  assert( !gSystem->Load("St_db_Maker")); 
  assert( !gSystem->Load("StEEmcDbMaker")); 

  // create chain    
  chain = new StChain("StChain"); 

  printf("adding muDst from '%s' ....\n",file);
  int sectID=66;

  // Now we add Makers to the chain...   
  muMk = new StMuDstMaker(0,0,inDir,file,"MuDst.root",nFiles);
  TChain* tree=muMk->chain(); assert(tree); 
  int nEntries=tree->GetEntries();
  printf("total eve in chain =%d\n",nEntries);
  printf("in=%s%s=\n",inDir,file);
  //  return;
  St_db_Maker *stDb = new St_db_Maker("StarDb", "MySQL:StarDb");
  stDb->SetFlavor("onlPed","eemcPMTped");
  stDb->SetFlavor("sim","eemcPMTcal");
  stDb->SetFlavor("sim","eemcPMTstat");
  
  myDb=new StEEmcDbMaker("eemcDb");
  //myDb->setSectors(sectID,sectID);
  // myDb->setSectors(firstSec, lastSec);
  myDb->changeMask( "/star/u/balewski/WWW-E/calibration/run5/mappingSmd/smdAllMaskDay49.dat");
  myDb->changeMask( "/star/u/balewski/WWW-E/calibration/run5/mappingTower/tileAllMaskDay49.dat");
  // myDb->changeGains("julieGains/smdG/gains05smd.dat");
  //myDb->changeGains("msarCalib/PQRgains.dat");
  //myDb->changeGains("msarCalib/towgains.dat");


  // MIP cut .........
  float thrMipSmdE=0.3/1000.;
  int emptyStripCount=6; 
  float offCenter=0.6; // fiducial area of tower =offCenter^2, was 0.8
  float twMipRelEneLow=0.5, twMipRelEneHigh=2.;  // was 0.5,1.5
  int thrMipPresAdc=12; // thres over pedestal for pre/post

  HList=new  TObjArray;
  int id;
  for(id=1;id<=12;id++) { sectID=id;
  myMk3=new MuEzSmdCalMaker("mySmdCal","MuDst");
  myMk3->SetHList(HList);
  myMk3->SetSector(sectID); 
  myMk3->setTwCuts(twMipRelEneLow, twMipRelEneHigh,offCenter);
  myMk3->setSmdCuts(thrMipSmdE,emptyStripCount);
  myMk3->setPreCuts(thrMipPresAdc);
  myMk3->SetTrigIdFilter(trigID);
  // if(id>=3) break;
  }

  //
  
  
  //myMk3->SetTrigIdFilter(66300); //zeroB
  //myMk3->SetTrigIdFilter(66007); //minB
  //   myMk3->SetTrigIdFilter(66212); //EHT19
  // myMk3->SetTrigIdFilter(66210); //EHT10

  gMessMgr->SwitchOff("D");
  gMessMgr->SwitchOn("I");
 
  muMk->SetStatus("*",0);
  muMk->SetStatus("EztAll",1);
  muMk->SetStatus("MuEvent",1);
  chain->Init();
  chain->ls(3);
  // muMk->printArrays();

  printf("All Ezt-branches set\n");
  int eventCounter=0;
  int stat=0;
  int t1=time(0);
  StMuTimer timer;
  timer.start();

  //---------------------------------------------------
  while ( 1) {// loop over events
    if(eventCounter>=nEve) break;
    eventCounter++;
    chain->Clear();
    stat = chain->Make();
    if(stat) break;
    // printf ("stat=%d\n",stat);
    if(eventCounter%200!=0)continue;

    printf("\n\n ====================%d  processing  ==============\n", eventCounter);
    
  }
   myDb->print(); //EEmcDb::exportAscii

  printf("sorting done, nEve=%d of %d\n",nEve, nEntries);
  int t2=time(0);
  if(t1==t2) t2++;
  float rate=1.*eventCounter/(t2-t1);
  float nMnts=(t2-t1)/60.;
  printf("sorting done %d of   nEve=%d, elapsed rate=%.1f Hz, tot %.1f minutes\n",eventCounter,nEntries,rate,nMnts);
  
  if (eventCounter) {
    cout << "CPU time/event= " << timer.elapsedTime()/eventCounter << " sec  "
         << " rate= " << eventCounter/timer.elapsedTime() <<  " Hz" << endl;
  }
  
  myMk3->saveHisto("smdCal-X");
  //  TString txt="soloPi0-"; txt+=trigID;  myMk3->saveHisto(txt);  


  char tt1[100];
  sprintf(tt1,"myStat%02d",sectID);
  h=(TH1F *) HList->FindObject(tt1);   assert(h);
  float *my=h->GetArray();
  printf("sec=%d nInp=%d  -->UxV multi=%.1f  one=%.1f any=%.1f tw=%.1f\n",sectID,nEve,my[2],my[3],my[4],my[5]);
  printf(" -->MIP cntr=%.1f w/tag=%.1f \n",my[6],my[7]);
  
  chain->Finish();
}


