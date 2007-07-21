class StMuDstMaker;
class  StChain *chain;
int total=0;
#include <string>
#include <map>  

void rdMu2TrigSimu(const char *dirIn ="runList/",
		   const char *file="R7101015.lis",
		   int flagMC=0
			     )
{
  int nevents = 5;
  int nfiles = 3; // make this big if you want to read all events from a run
 
  TString outDir="./out2/"; 
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();


  assert( !gSystem->Load("StDbBroker"));
  assert( !gSystem->Load("St_db_Maker"));
  assert( !gSystem->Load("StEEmcDbMaker"));
  assert( !gSystem->Load("StEEmcUtil")); // needed by eemcDb
  assert( !gSystem->Load("StDaqLib")); // needed by bemcDb
  assert( !gSystem->Load("StEmcRawMaker"));
  assert( !gSystem->Load("StEmcADCtoEMaker"));
  assert( !gSystem->Load("StTriggerUtilities"));
  gROOT->Macro("LoadLogger.C");


  cout << " loading done " << endl;
  
  chain= new StChain("StChain"); 
  TObjArray* HList=new TObjArray; // to collect all output histograms for Jan

  const char *filter = "";
  StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,dirIn,file,filter,nfiles,"MuDst");
  TChain* tree=muDstMaker->chain(); assert(tree);
  int nEntries=(int) tree->GetEntries();
  printf("total eve in muDst chain =%d\n",nEntries);  // return ;
  if(nEntries<=0) return; // do not process if no data 
 
  //Database -- get a real calibration from the database
  St_db_Maker* dbMk = new St_db_Maker("StarDb","MySQL:StarDb","MySQL:StarDb","$STAR/StarDb");
  
  //Endcap DB
  StEEmcDbMaker* eemcb = new StEEmcDbMaker("eemcDb");
  
  //BEMC adc->Et  
  StEmcADCtoEMaker *adc = new StEmcADCtoEMaker(); // this will just convert what's in MuDst to ADC, use for data only!
  
  StTriggerSimuMaker *simuTrig = new StTriggerSimuMaker("StarTrigSimu");
  simuTrig->setDbMaker(dbMk);
  simuTrig->setHList(HList);
  simuTrig->useEemc();
  

  chain->ls(3);
  chain->Init();
  chain->PrintInfo();  

  const int mxTr=13;
  int myTrgList[mxTr]    ={127580,     127551,        127271,           127571,      127821,            127831, 127575, 127622,127221,127611, 117705, 127501, 127652	};
  // prescale as for run 7101015
  char* myTrgListN[mxTr]={"eemc-http*116","eemc-jp0-mb*221", "eemc-jp1-mb*1", "bemc-jp1*433", "bemc-http-mb-fast*1", "eemc-http-mb-fast*1", "bemc-jp0-etot*979" , "bemc-jp0-etot-mb-L2jet*2", "bemc-jp1-mb*1", "bemc-http-mb-l2gamma*1", "jpsi-mb*20", "bemc-jp0-mb*909","eemc-jp0-etot-mb-L2jet*2"};
  int nR[mxTr],nS[mxTr],nRS[mxTr];
  memset(nR,0, sizeof(nR));
  memset(nS,0, sizeof(nR));
  memset(nRS,0, sizeof(nR));

  int BL1_ADC[6];
  int hold=-1;
  int t1=time(0);

  for (Int_t iev=0;iev<nevents; iev++) {
    cout << "****************************************** " << endl;
    cout << "\nWorking on eventNumber:\t" << iev <<"\tof:\t"<<nevents<<endl;
    cout << "*************************1***************** " << endl;
    chain->Clear();
    int iret = chain->Make(iev);
    total++;
    if (iret) {
      cout << "Bad return code!" << endl;
      break;
    }
    
    cout<<Form(" Simu trgSize=%d, firedID:", simuTrig->mTriggerList.size());
    int j;
    for(j=0; j<simuTrig->mTriggerList.size();j++) 
      cout<<Form("s%d, ", simuTrig->mTriggerList[j]);
    cout<<"\n";
      

     // Access to muDst .......................
    StMuEvent* muEve = muDstMaker->muDst()->event();

    StEventInfo &info=muEve->eventInfo();
    StMuTriggerIdCollection &tic=muEve->triggerIdCollection();
    vector<unsigned int> trgL=tic.nominal().triggerIds();
    cout<<Form(" real trgSize=%d, firedID:",trgL.size());
    for(j=0; j<trgL.size();j++) 
      cout<<Form("r%d, ", trgL[j]);
    cout<<"\n";

    //.. compare Simu vs.  real
    for(j=0;j<mxTr;j++) {
      int trgId=myTrgList[j];
      bool realT=tic.nominal().isTrigger(trgId);
      bool simT=simuTrig->isTrigger(trgId); // endcap only, tmp
      if(realT) nR[j]++;
      if(realT && simT) nRS[j]++;
      if(simT) nS[j]++;
        cout <<Form("C:j=%d  trg=%d  R=%d S=%d  RS=%d",j, myTrgList[j],realT,simT,realT && simT )<<endl;
    }

  }
  int t2=time(0);
  if(t2==t1) t2=t1+1;
  float tMnt=(t2-t1)/60.;
  float rate=1.*total/(t2-t1);



  chain->Finish();
  cout << "****************************************** " << endl;
  cout << "total number of events  " << total << endl;
  cout << "****************************************** " << endl;

 

  TString fileMu=file;
  printf("=%s=\n",fileMu.Data());
  TString outF=outDir+fileMu.ReplaceAll(".lis",".trgSim");
  outF+=".hist.root";
  printf("=%s=\n",outF.Data());
  hf=new TFile(outF,"recreate");
  // HList->ls();
  HList->Write();
  printf("\n Histo saved -->%s<\n",outF.Data());
  
  cout <<Form("sorting done %d of   nEve=%d, CPU rate=%.1f Hz, total time %.1f minute(s) \n\n",total,nEntries,rate,tMnt)<<endl;


   for(j=0;j<mxTr;j++) {
     cout <<Form("SUM  trg=%d  nR=%d nS=%d  nRS=%d  %s", myTrgList[j], nR[j],nS[j],nRS[j],myTrgListN[j])<<endl;
   }
 
}


