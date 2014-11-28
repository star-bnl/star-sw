class StChain;
class StMuEmcCollection;

class StEEmcDb;
class StMuDstMaker;
class TChain;
class TObjArray;

class StEEmcA2EMaker;


StEEmcDb *myDb;
StMuDstMaker* muMk;
StChain *chain=0;
TObjArray *HList;

StEEmcA2EMaker *mEEbefore;
StEEmcA2EMaker *mEEafter;

// Comment out the line below to read EEMC data from StEvent (default is MuDst)
#define useMuDst

int runEEmcSlowMaker( int nEve=2000 ){

  int firstSec=1;
  int lastSec=12;

  Int_t nFiles=10; 

#ifdef useMuDst
  char* file="rcf1308_01_2000evts.MuDst.root";
#else
  char* file="rcf1308_01_2000evts.event.root";
#endif
  char* inDir="/star/data47/reco/pp200/pythia6_410/11_15gev/cdf_a/y2006c/gheisha_on/p07ic/";

#ifdef useMuDst
  char *outname="mudst";
#else
  char *outname="stevent";
#endif
  TString fullName=file;

  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  gROOT->Macro("LoadLogger.C");
  cout << " loading done " << endl;
  //gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEEmcSimulatorMaker");
  gSystem->Load("StEEmcA2EMaker");

  // create chain    
  chain = new StChain("StChain"); 

  printf("adding MuDst/StEvent from run '%s' ....\n",fullName.Data());

  // Now we add Makers to the chain...
#ifdef useMuDst
  StMuDstMaker* muMk = new StMuDstMaker(0,0,inDir,fullName,".MuDst.root",nFiles);
  TChain* tree=muMk->chain(); assert(tree); 
  int nEntries=tree->GetEntries();
  printf("total eve in chain =%d\n",nEntries);
#else
  StIOMaker* ioMaker = new StIOMaker;
  ioMaker->SetFile(gSystem->ConcatFileName(inDir, file));
  ioMaker->SetBranch("*", 0, "0");
  ioMaker->SetBranch("eventBranch", 0, "r");
  ioMaker->SetBranch("geantBranch", 0, "r");
  Int_t nEntries=nEve;
#endif

  St_db_Maker *stDb = new St_db_Maker("StarDb", "MySQL:StarDb");
  stDb->SetDateTime(20031120,0);
  stDb->SetFlavor("sim","eemcPMTcal");
  stDb->SetFlavor("sim","eemcPIXcal");
  stDb->SetFlavor("sim","eemcPMTped");
  stDb->SetFlavor("sim","eemcPMTstat");
  stDb->SetFlavor("sim","eemcPMTname");
  stDb->SetFlavor("sim","eemcADCconf");

  new StEEmcDbMaker("eemcDb");
  HList=new TObjArray;

  // ADC 2 energy before slow simulator
  mEEbefore = new StEEmcA2EMaker("before");
  mEEbefore->database("eemcDb");
#ifdef useMuDst
  mEEbefore->source("MuDst",1);
#else
  mEEbefore->source("StEvent",2);
#endif



  /*  
   * The slow simulator is setup by default with the best
   * estimates we have for the single p.e. resolution, energy
   * deposited by a single mip in an smd/pre/postshower layer,
   * and number of photoelectrons produced per mip.  If one
   * really wants to play with fire, these are documented
   * in the header file.
   *
   */
  StEEmcSlowMaker *slowSim=new StEEmcSlowMaker();
  slowSim->setDropBad(0);   // 0=no action, 1=drop chn marked bad in db
  slowSim->setAddPed(0);    // 0=no action, 1=ped offset from db
  slowSim->setSmearPed(0);  // 0=no action, 1=gaussian ped, width from db
  slowSim->setOverwrite(1); // 0=no action, 1=overwrite muDst/StEvent


#ifdef useMuDst
  slowSim->setSource("MuDst");
#else
  slowSim->setSource("StEvent");
#endif

  /// Uncomment the following line to save debuging histograms
  slowSim->SetHList(HList);


  // ADC 2 energy after slow simulator
  mEEafter = new StEEmcA2EMaker("after");
  mEEafter->database("eemcDb");
#ifdef useMuDst
  mEEafter->source("MuDst",1);
#else
  mEEafter->source("StEvent",2);
#endif



  // some test histograms
  TH2F *h_compare=new TH2F("h_compare","ADC after slow maker vs before; uncorrected ADC; corrected ADC",512,0.,1024.,512,0.,1024. );
  TH2F *h_compare_pre1=new TH2F("h_compare_pre1","ADC after slow maker vs before",256,0.,1024.,256,0.,256. );
  TH2F *h_compare_pre2=new TH2F("h_compare_pre2","ADC after slow maker vs before",256,0.,1024.,256,0.,256. );
  TH2F *h_testit=new TH2F("h_testit","ADC after-before vs ADC pre1+pre2; pre1+pre2 AD; corrected-uncorrected",256,0.,512.,100,-50.,50.);
  TH2F *h_perinc=new TH2F("h_perinc","percent increase vs before; ADC [before simu]; % increase",128,0.,1024.,120,-10.,50.);
  TH2F *h_perincE=new TH2F("h_perincE","percent increase vs original E; Energy [before simu]; % increase",150,0.,30.,120,-10.,50.);
  TH2F *h_perincEZ=new TH2F("h_perincEZ","percent increase vs original E (zeros supressed); Energy [before simu]; % increase",150,0.,30.,120,-10.,50.);




  
  // Put your analysis maker here
  // StMyAnaMaker *bhla =new  StMyAnaMaker;

  chain->ls(3);
  chain->Init();

  StEEmcDb* myDb = (StEEmcDb*)chain->GetDataSet("StEEmcDb");
  myDb->setSectors(firstSec, lastSec);

  int eventCounter=0;
  int stat=0;
  int t1=time(0);
  //---------------------------------------------------
  while ( stat==0 ) {// loop over events
    if(eventCounter>=nEve) break;
    eventCounter++;
    chain->Clear();
    stat = chain->Make();
    if(eventCounter%100==0)
      printf("\n====================%d  processing  ============\n", eventCounter);

    for ( Int_t ii=0;ii<mEEafter->numberOfHitTowers(0);ii++ )
      {
	StEEmcTower tafter=mEEafter->hittower(ii,0);
	StEEmcTower tbefore=mEEbefore->tower(tafter.index(),0);
	StEEmcTower pbefore=mEEbefore->tower(tafter.index(),1);
	StEEmcTower qbefore=mEEbefore->tower(tafter.index(),1);
	h_compare->Fill( tbefore.adc(), tafter.adc());

	h_testit->Fill( pbefore.adc()+pbefore.adc(), tafter.adc()-tbefore.adc() );

	h_perinc->Fill( tbefore.adc(), 100.0* (tafter.adc()-tbefore.adc() )/tafter.adc() );
	h_perincE->Fill( tbefore.energy(), 100.0* (tafter.adc()-tbefore.adc() )/tafter.adc() );
	if ( TMath::Abs(tbefore.adc() - tafter.adc()) >1. )
	  h_perincEZ->Fill( tbefore.energy(), 100.0* (tafter.adc()-tbefore.adc() )/tafter.adc() );

      }

    
  }
  printf("sorting done, nEve=%d of %d\n",nEve, nEntries);
  //  return;
  int t2=time(0);
  if(t2==t1) t2=t1+1;
  float tMnt=(t2-t1)/60.;
  float rate=1.*nEve/(t2-t1);
  printf("sorting done %d of   nEve=%d, CPU rate=%.1f Hz, total time %.1f minute(s) \n",eventCounter,nEntries,rate,tMnt);

  chain->Finish();

  //  return;
  // save output histograms 
  // HList.ls();



  /// Change to 1 to save debugging histograms
#if 1  
  TString out="./";
  out+=outname;
  out+=".hist.root";
  TFile f( out,"recreate");
  assert(f.IsOpen());
  printf("%d histos are written  to '%s' ...\n",HList->GetEntries(),out.Data());
  HList->Write();
  h_compare->Write();
  h_testit->Write();
  h_perinc->Write();h_perincE->Write();h_perincEZ->Write();
  f.Close();
#endif  

}
