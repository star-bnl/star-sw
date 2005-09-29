TObjArray  *HList;
//cucu200_minB=66007, cucu62_minB=76007
//pp200_minB=96011

int rdEztMuSmdCal( int run=61710371,
		   int trigID=96011,
		   int nEve=800000   ,
		   Int_t nFiles  = 1000
		   ){ 
  TString fileS="lis/R";
  fileS+=run;  fileS+=".lis";
  char* inDir   = "./";
  // inDir   ="/star/data05/scratch/eemcdb/muDst/2005/171/037b/"; fileS="st_physics_6171037_raw_2020001.MuDst.root";
  // fileS="lis/R61730x.lis";
  // inDir="/star/data04/sim/balewski/myfzdSamples/";  fileS="mc_mu-eta1_24.MuDst.root";

  TString outF="/star/data05/scratch/balewski/2005-eemcCal/day49-hist/iter2xx";
  outF="iter14-pp/";
  
  char*  file=fileS.Data();

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
  int nEntries=(int)tree->GetEntries();
  printf("total eve in chain =%d\n",nEntries);
  printf("in=%s%s=\n",inDir,file);
  //  return;
  St_db_Maker   *stDb = new St_db_Maker("StarDb", "MySQL:StarDb");
  StEEmcDbMaker *myDb = new StEEmcDbMaker("eemcDb");

  // stDb->SetFlavor("onlPed","eemcPMTped");
  //#define IS_MC  // M-C events
#ifdef IS_MC 
  //stDb->SetFlavor("sim","eemcPMTped"); // better use real if pedSmear is ON
  stDb->SetFlavor("sim","eemcPMTcal");
  stDb->SetFlavor("sim","eemcPIXcal");
  stDb->SetDateTime(20050505,0);

#if 1
  assert( !gSystem->Load("StEEmcSimulatorMaker")); 
  StEEmcSlowMaker *slowSim = new StEEmcSlowMaker("slowSim");
  slowSim->setDropBad(0);   // 0=no action, 1=drop chn marked bad in db
  slowSim->setAddPed(1);    // 0=no action, 1=ped offset from db
  slowSim->setSmearPed(1);  // 0=no action, 1=gaussian ped, width from db
  slowSim->setOverwrite(1); // 0=no action, 1=overwrite muDst values
#endif
#endif 

  myDb->changeMask( "/star/u/balewski/WWW-E/calibration/run5/absMipCal/iter5-out/auxilMask.dat");
  myDb->changeMask( "./maskCrash.dat");
  //myDb->setSectors(5,5);

  // MIP cut .........
  float thrMipSmdE=0.4/1000.; // was 0.5 MeV for data
  int emptyStripCount=7; 
  float offCenter=0.7; // fiducial area of tower =offCenter^2, was 0.8,CuCuwas0.7
  float twMipRelEneLow=0.5, twMipRelEneHigh=2.;  // was 0.5,1.5
  int thrMipPresAdc=12; // thres over pedestal for pre/post (was 12 if no energy cut)

  HList=new  TObjArray;
  int id;
  for(id=1;id<=12;id++) { 
    sectID=id;
    myMk3=new MuEzSmdCalMaker("mySmdCal","MuDst");
    // myMk3->setEZtree(false); // switch input: EzTree or regular muDst collection
    myMk3->setHList(HList);
    myMk3->setSector(sectID); 
    myMk3->setTwCuts(twMipRelEneLow, twMipRelEneHigh,offCenter);
    myMk3->setSmdCuts(thrMipSmdE,emptyStripCount);
    myMk3->setPreCuts(thrMipPresAdc);
    myMk3->setTrigIdFilter(trigID);
    // if(id>=3)
    //break;
  }

  gMessMgr->SwitchOn("I");
  gMessMgr->SwitchOff("W");

#ifndef IS_MC  
  muMk->SetStatus("*",0);
  muMk->SetStatus("EztAll",1);
  muMk->SetStatus("MuEvent",1);
#endif

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

    printf("====================%5d  processing  ==============\n", eventCounter);
    
  }
  myDb->print(); //EEmcDb::exportAscii

  printf("sorting done, nEve=%d of %d\n",eventCounter, nEntries);
  int t2=time(0);
  if(t1==t2) t2++;
  float rate=1.*eventCounter/(t2-t1);
  float nMnts=(t2-t1)/60.;
  printf("sorting done %d of   nEve=%d, elapsed rate=%.1f Hz, tot %.1f minutes\n",eventCounter,nEntries,rate,nMnts);
  
  if (eventCounter) {
    cout << "CPU time/event= " << timer.elapsedTime()/eventCounter << " sec  "
         << " rate= " << eventCounter/timer.elapsedTime() <<  " Hz" << endl;
  }
  
  outF+="/R";  outF+=run;

  // myMk3->saveHisto("smdCal-X");
  myMk3->saveHisto(outF.Data());

  char tt1[100];
  sprintf(tt1,"my%02dStat",sectID);
  h=(TH1F *) HList->FindObject(tt1);   assert(h);
  float *my=h->GetArray();
  printf("sec=%d nInp=%d  -->UxV multi=%.1f  one=%.1f, MIP any=%.1f tw=%.1f\n",sectID,eventCounter,my[2],my[3],my[4],my[5]);
  printf(" -->MIP cntr=%.1f w/post=%.1f \n",my[6],my[7]);
  
  chain->Finish();
}


#if 0
AUXILIARY code
  // stDb->SetFlavor("sim","eemcPMTcal");
  //stDb->SetFlavor("sim","eemcPMTstat");


  myDb->changeMask( "/star/u/balewski/WWW-E/calibration/run5/absMipCal/iter1-inp/smdAllMaskDay49v1.dat");
  myDb->changeMask( "/star/u/balewski/WWW-E/calibration/run5/absMipCal/iter1-inp/tileAllMaskDay49v1.dat");

  //....... pre/post
  //  myDb->changeGains("/star/u/balewski/WWW-E/calibration/run5/absMipCal/iter1-inp/smdAllSecG-slopes.dat");
  myDb->changeGains("/star/u/balewski/WWW-E/calibration/run5/absMipCal/iter2-inp/smdAllSec-absGain.dat");
  myDb->changeGains("/star/u/balewski/WWW-E/calibration/run5/absMipCal/iter3-inp/gainsP-allSect.dat");
  myDb->changeGains("/star/u/balewski/WWW-E/calibration/run5/absMipCal/iter3-inp/gainsQ-allSect.dat");
  myDb->changeGains("/star/u/balewski/WWW-E/calibration/run5/absMipCal/iter3-inp/gainsR-allSect.dat");

  // ... towers
  //  myDb->changeGains("/star/u/balewski/WWW-E/calibration/run5/absMipCal/iter3-inp/gainsT-allSect.dat");


for pp
  myDb->changeGains("/star/u/balewski/WWW-E/calibration/run5/absMipCal/iter5-out/auxilGain.dat");

#endif

