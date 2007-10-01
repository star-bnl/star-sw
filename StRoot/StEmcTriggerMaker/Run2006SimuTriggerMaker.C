class StMuDstMaker;
class  StChain *chain;
int total=0;
#include <string>
#include <map>

void Run2006SimuTriggerMaker(const char *dir ="",
			     const char* file="/star/data32/reco/pp200/pythia6_205/above_35gev/cdf_a/y2004y/gheisha_on/p05ih/rcf1230_10_4000evts.MuDst.root",
			     const char *fname="/star/data32/reco/pp200/pythia6_205/above_35gev/cdf_a/y2004y/gheisha_on/p05ih/rcf1230_10_4000evts.event.root",
			     const char *filter = "")
{
  int nevents = 10;
  
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  gSystem->Load("StTpcDb");
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StMcEvent");
  gSystem->Load("StMcEventMaker");
  gSystem->Load("StDaqLib");
  gSystem->Load("StEmcRawMaker");
  gSystem->Load("StEmcADCtoEMaker");
  gSystem->Load("StPreEclMaker");
  gSystem->Load("StEpcMaker");
  gSystem->Load("StEmcSimulatorMaker");
  gSystem->Load("StEmcUtil");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEmcTriggerMaker");

  double pi = atan(1.0)*4.0;
  cout << " loading done " << endl;
  
  chain= new StChain("StChain"); 
  chain->SetDebug(1);
  gMessMgr->SwitchOff("D");
  gMessMgr->SwitchOff("I");
  
  StIOMaker* ioMaker = new StIOMaker();
  ioMaker->SetFile(fname);
  ioMaker->SetIOMode("r");
  ioMaker->SetBranch("*",0,"0");             //deactivate all branches
  ioMaker->SetBranch("geantBranch",0,"r");   //activate geant Branch

  class StMcEventMaker *mcEventMaker = new StMcEventMaker();
  mcEventMaker->doPrintEventInfo = false;
  mcEventMaker->doPrintMemoryInfo = false;
  
  StMuDebug::setLevel(1); 
  StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,dir,file,filter,1e6,"MuDst");
 
  //Database -- get a real calibration from the database
  St_db_Maker* dbMk = new St_db_Maker("StarDb","MySQL:StarDb","MySQL:StarDb","$STAR/StarDb");
  dbMk->SetDateTime(20060506,214129 ); // for simulation
 
  //to use ideal ideal gains/status/calib uncommment correct line below
  //dbMk->SetFlavor("sim","bemcPed");
  //dbMk->SetFlavor("sim","bemcStatus");
  //dbMk->SetFlavor("sim","bemcCalib");
  //dbMk->SetFlavor("sim","bemcGain");
  //dbMk->SetFlavor("sim","eemcPMTcal");
  //dbMk->SetFlavor("sim","eemcPIXcal");

  chain->ls(3);
 //Database interface
  StDetectorDbMaker* detDbMk = new StDetectorDbMaker();

 //Endcap DB
  StEEmcDbMaker* eemcb = new StEEmcDbMaker("eemcDb");
  
  //StEmcADCtoEMaker *adc = new StEmcADCtoEMaker(); // this will just convert what's in MuDst to ADC, use for data only!
  StEmcSimulatorMaker* emcSim = new StEmcSimulatorMaker(); //use this instead to "redo" converstion from geant->adc
  emcSim->setCalibSpread(kBarrelEmcTowerId, 0.15);
  StPreEclMaker* preEcl = new StPreEclMaker(); //need this to fill new StEvent information
  
  //StEmcTrigger
  StEmcTriggerMaker *emcTrig = new StEmcTriggerMaker("bemctrigger");

  chain->Init();
  chain->PrintInfo();
  
  chain->ls(3);
  TChain* fileChain = muDstMaker->chain(); 


  int BL1_ADC[6];
  int hold=-1;
  for (Int_t iev=0;iev<nevents; iev++) {
    cout << "****************************************** " << endl;
    cout << "Working on eventNumber:\t" << iev <<"\tof:\t"<<nevents<<endl;
    cout << "*************************1***************** " << endl;
    chain->Clear();
    int iret = chain->Make(iev);
    total++;
    if (iret) {
      cout << "Bad return code!" << endl;
      break;
    }

    cout<<"           "<<endl;
    map<int,int>::iterator iter;
    map<int,int> tower;
    map<int,int> tpatch;
    map<int,int> jpatch;

    cout<<endl;
    cout<<endl;
    cout<<" 1101 ht1 = "<<emcTrig->isTrigger(1101)<<" = "<<emcTrig->is2003HT1()<<endl;    
    tower=emcTrig->barrelTowersAboveThreshold(1101);
    cout<<"Total #'s of towers="<<tower.size()<<endl;
    for ( iter=tower.begin();iter !=tower.end(); iter++){
      cout<<"tower id="<<iter->first<<"  adc="<<iter->second<<endl;
      cout<<"old tower id="<<emcTrig->get2003HT1_ID()<<" adc="<<emcTrig->get2003HT1_ADC()<<endl;
    }

    cout<<endl;
    cout<<endl;
    cout<<" 2101 ht1 = "<<emcTrig->isTrigger(2201)<<" = "<<emcTrig->is2003HT1()<<endl;   
    tower=emcTrig->barrelTowersAboveThreshold(2201);
    cout<<"Total #'s of towers="<<tower.size()<<endl;
    for ( iter=tower.begin();iter !=tower.end(); iter++){
      cout<<"tower id="<<iter->first<<"  adc="<<iter->second<<endl;
      cout<<"old tower id="<<emcTrig->get2003HT1_ID()<<" adc="<<emcTrig->get2003HT1_ADC()<<endl;
    }

    cout<<endl;
    cout<<endl;
    cout<<" 2202 ht2 = "<<emcTrig->isTrigger(2202)<<" = "<<emcTrig->is2003HT2()<<endl;   
    tower=emcTrig->barrelTowersAboveThreshold(2202);
    cout<<"Total #'s of towers="<<tower.size()<<endl;
    for ( iter=tower.begin();iter !=tower.end(); iter++){
      cout<<"tower id="<<iter->first<<"  adc="<<iter->second<<endl;
      cout<<"old tower id="<<emcTrig->get2003HT2_ID()<<" adc="<<emcTrig->get2003HT2_ADC()<<endl;
    }

    cout<<endl;
    cout<<endl;
    cout<<" 45201 ht1 = "<<emcTrig->isTrigger(45201)<<" =  "<<emcTrig->is2004HT1()<<endl;   
    tower=emcTrig->barrelTowersAboveThreshold(45201);
    cout<<"Total #'s of towers="<<tower.size()<<endl;
    for ( iter=tower.begin();iter !=tower.end(); iter++){
      cout<<"tower id="<<iter->first<<"  adc="<<iter->second<<endl;
      cout<<"old tower id="<<emcTrig->get2004HT1_ID()<<" adc="<<emcTrig->get2004HT1_ADC()<<endl;
    }

    cout<<endl;
    cout<<endl;
    cout<<" 45202 ht1 = "<<emcTrig->isTrigger(45202)<<" =  "<<emcTrig->is2004HT2()<<endl;   
    tower=emcTrig->barrelTowersAboveThreshold(45202);
    cout<<"Total #'s of towers="<<tower.size()<<endl;
    for ( iter=tower.begin();iter !=tower.end(); iter++){
      cout<<"tower id="<<iter->first<<"  adc="<<iter->second<<endl;
      cout<<"old tower id="<<emcTrig->get2004HT2_ID()<<" adc="<<emcTrig->get2004HT2_ADC()<<endl;
    }

    cout<<endl;
    cout<<endl;
    cout<<" 45206 jp1 = "<<emcTrig->isTrigger(45206)<<" =  "<<emcTrig->is2004JP1()<<endl;   
    jpatch=emcTrig->barrelJetPatchesAboveThreshold(45206);
    cout<<"Total #'s of jpatches="<<jpatch.size()<<endl;
    for ( iter=jpatch.begin();iter !=jpatch.end(); iter++){
      cout<<"jpatch id="<<iter->first<<"  adc="<<iter->second<<endl;
      cout<<"old patch id="<<emcTrig->get2004JP1_ID()<<" adc="<<emcTrig->get2004JP1_ADC()<<endl;
    }

    cout<<endl;
    cout<<endl;
    cout<<" 45207 jp2 = "<<emcTrig->isTrigger(45207)<<"  =  "<<emcTrig->is2004JP2()<<endl;   
    jpatch=emcTrig->barrelJetPatchesAboveThreshold(45207);
    cout<<"Total #'s of jpatches="<<jpatch.size()<<endl;
    for ( iter=jpatch.begin();iter !=jpatch.end(); iter++){
      cout<<"jpatch id="<<iter->first<<"  adc="<<iter->second<<endl;
      cout<<"old patch id="<<emcTrig->get2004JP2_ID()<<" adc="<<emcTrig->get2004JP2_ADC()<<endl;
    }

    cout<<endl;
    cout<<endl;
    cout<<" 127212 ht2 (matrix0)="<<emcTrig->isTrigger(127212)<<endl;
    tower=emcTrig->barrelTowersAboveThreshold(127212);
    cout<<"Total #'s of towers="<<tower.size()<<endl;
    for ( iter=tower.begin();iter !=tower.end(); iter++){
      cout<<"tower id="<<iter->first<<"  adc="<<iter->second<<endl;
    }

    cout<<endl;
    cout<<endl;
    cout<<" 127213 ht2 (matrix1)="<<emcTrig->isTrigger(127213)<<endl;
    tower=emcTrig->barrelTowersAboveThreshold(127213);
    cout<<"Total #'s of towers="<<tower.size()<<endl;
    for ( iter=tower.begin();iter !=tower.end(); iter++){
      cout<<"tower id="<<iter->first<<"  adc="<<iter->second<<endl;
    }

    cout<<endl;
    cout<<endl;
    cout<<" 137213 ht2 (matrix3) ="<<emcTrig->isTrigger(127213)<<endl;
    tower=emcTrig->barrelTowersAboveThreshold(127213);
    cout<<"Total #'s of towers="<<tower.size()<<endl;
    for ( iter=tower.begin();iter !=tower.end(); iter++){
      cout<<"tower id="<<iter->first<<"  adc="<<iter->second<<endl;
    }

    cout<<endl;
    cout<<endl;
    cout<<" 127501 jp0 (matrix1)="<<emcTrig->isTrigger(127501)<<endl;
    jpatch=emcTrig->barrelJetPatchesAboveThreshold(127501);
    cout<<"Total #'s of jpatches="<<jpatch.size()<<endl;
    for ( iter=jpatch.begin();iter !=jpatch.end(); iter++){
      cout<<"jpatch id="<<iter->first<<"  adc="<<iter->second<<endl;
    }

    cout<<endl;
    cout<<endl;
    cout<<" 137501 jp0 (matrix3) ="<<emcTrig->isTrigger(137501)<<endl;
    jpatch=emcTrig->barrelJetPatchesAboveThreshold(137501);
    cout<<"Total #'s of jpatches="<<jpatch.size()<<endl;
    for ( iter=jpatch.begin();iter !=jpatch.end(); iter++){
      cout<<"jpatch id="<<iter->first<<"  adc="<<iter->second<<endl;
    }

    cout<<endl;
    cout<<endl;
    cout<<" 127221 jp1 (matrix1)="<<emcTrig->isTrigger(127221)<<endl;
    jpatch=emcTrig->barrelJetPatchesAboveThreshold(127221);
    cout<<"Total #'s of jpatches="<<jpatch.size()<<endl;
    for ( iter=jpatch.begin();iter !=jpatch.end(); iter++){
      cout<<"jpatch id="<<iter->first<<"  adc="<<iter->second<<endl;
    }

    cout<<endl;
    cout<<endl;
    cout<<" 137221 jp1 (matrix2)="<<emcTrig->isTrigger(137221)<<endl;
    jpatch=emcTrig->barrelJetPatchesAboveThreshold(137221);
    cout<<"Total #'s of jpatches="<<jpatch.size()<<endl;
    for ( iter=jpatch.begin();iter !=jpatch.end(); iter++){
      cout<<"jpatch id="<<iter->first<<"  adc="<<iter->second<<endl;
    }

    cout<<endl;
    cout<<endl;
    cout<<" 137222 jp1 (matrix3)="<<emcTrig->isTrigger(137222)<<endl;
    jpatch=emcTrig->barrelJetPatchesAboveThreshold(127501);
    cout<<"Total #'s of jpatches="<<jpatch.size()<<endl;
    for ( iter=jpatch.begin();iter !=jpatch.end(); iter++){
      cout<<"jpatch id="<<iter->first<<"  adc="<<iter->second<<endl;
    }

    cout<<endl;
    cout<<endl;
    cout<<" 127821 http (matrix1)="<<emcTrig->isTrigger(127821)<<endl;
    tpatch=emcTrig->barrelTriggerPatchesAboveThreshold(127821);
    cout<<"Total #'s of tpatches="<<tpatch.size()<<endl;
    for ( iter=tpatch.begin();iter !=tpatch.end(); iter++){
      cout<<"tpatch id="<<iter->first<<"  adc="<<iter->second<<endl;
    }
    tower=emcTrig->barrelTowersAboveThreshold(127821);
    cout<<"Total #'s of towers="<<tower.size()<<endl;
    for ( iter=tower.begin();iter !=tower.end(); iter++){
        int triggerPatch = emcTrig->barrelTriggerPatchForTower(iter->first);
        cout<<"tower id="<<iter->first<<"  adc="<<iter->second<<"  tp (id,adc)=("<< triggerPatch << "," << tpatch[triggerPatch] << ")" <<endl;
    }

    cout<<endl;
    cout<<endl;
    cout<<" 137821 http (matrix3) ="<<emcTrig->isTrigger(137821)<<endl;
    tpatch=emcTrig->barrelTriggerPatchesAboveThreshold(137821);
    cout<<"Total #'s of tpatches="<<tpatch.size()<<endl;
    for ( iter=tpatch.begin();iter !=tpatch.end(); iter++){
      cout<<"tpatch id="<<iter->first<<"  adc="<<iter->second<<endl;
    }
    tower=emcTrig->barrelTowersAboveThreshold(137821);
    cout<<"Total #'s of towers="<<tower.size()<<endl;
    for ( iter=tower.begin();iter !=tower.end(); iter++){
        int triggerPatch = emcTrig->barrelTriggerPatchForTower(iter->first);
        cout<<"tower id="<<iter->first<<"  adc="<<iter->second<<"  tp (id,adc)=("<< triggerPatch << "," << tpatch[triggerPatch] << ")" <<endl;
    }

    cout<<endl;
    cout<<endl;
    cout<<" 137822 http (matrix4)="<<emcTrig->isTrigger(137822)<<endl;
    tpatch=emcTrig->barrelTriggerPatchesAboveThreshold(137822);
    cout<<"Total #'s of tpatches="<<tpatch.size()<<endl;
    for ( iter=tpatch.begin();iter !=tpatch.end(); iter++){
      cout<<"tpatch id="<<iter->first<<"  adc="<<iter->second<<endl;
    }  
    tower=emcTrig->barrelTowersAboveThreshold(137822);
    cout<<"Total #'s of towers="<<tower.size()<<endl;
    for ( iter=tower.begin();iter !=tower.end(); iter++){
        int triggerPatch = emcTrig->barrelTriggerPatchForTower(iter->first);
        cout<<"tower id="<<iter->first<<"  adc="<<iter->second<<"  tp (id,adc)=("<< triggerPatch << "," << tpatch[triggerPatch] << ")" <<endl;
    }

    cout<<endl;
    cout<<endl;
    cout<<" 117705 jpsi (matrix1)="<<emcTrig->isTrigger(117705)<<endl;
    tower=emcTrig->barrelTowersAboveThreshold(117705);
    cout<<"Total #'s of towers="<<tower.size()<<endl;
    for ( iter=tower.begin();iter !=tower.end(); iter++){
      cout<<"tower id="<<iter->first<<"  adc="<<iter->second<<endl;
    }

    cout<<endl;
    cout<<endl;
    cout<<" 117602 upsilon (matrix1)="<<emcTrig->isTrigger(117602)<<endl;
    tower=emcTrig->barrelTowersAboveThreshold(117602);
    cout<<"Total #'s of towers="<<tower.size()<<endl;
    for ( iter=tower.begin();iter !=tower.end(); iter++){
      cout<<"tower id="<<iter->first<<"  adc="<<iter->second<<endl;
    }

    cout<<endl;
    cout<<endl;
    cout<<" 137602 upsilon (matrix3)="<<emcTrig->isTrigger(137602)<<endl;
    tower=emcTrig->barrelTowersAboveThreshold(137602);
    cout<<"Total #'s of towers="<<tower.size()<<endl;
    for ( iter=tower.begin();iter !=tower.end(); iter++){
      cout<<"tower id="<<iter->first<<"  adc="<<iter->second<<endl;
    }


    cout<<endl;
    cout<<endl;
    cout<<" 127611 http-L2="<<emcTrig->isTrigger(127611)<<endl;
    
    cout<<endl;
    cout<<endl;
    cout<<" 127622 jp0-etot-L2="<<emcTrig->isTrigger(127622)<<endl;
 
    cout<<endl;
    cout<<endl;
    cout<<" 137622  jp0-etot-L2="<<emcTrig->isTrigger(137622)<<endl;

    for (int j=0;j<6;j++){
      BL1_ADC[j]=0.0;
      emcTrig->get2006BL1_ADC(j,&hold);
      BL1_ADC[j]=hold;
      cout<<" 2x1 Patch "<<j<<" = "<<BL1_ADC[j]<<endl;
    }
 

  }

  chain->Finish();
  cout << "****************************************** " << endl;
  cout << "total number of events  " << total << endl;
  cout << "****************************************** " << endl;
 
}
