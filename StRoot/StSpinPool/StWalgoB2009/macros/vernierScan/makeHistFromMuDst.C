class StChain;
class StMuDstMaker;
class St_db_Maker;
class StEmcADCtoEMaker;
class TH1F;

//--
//-- globals
//--
StChain            *mChain        = 0;
StMuDstMaker       *mMuDstMaker   = 0;
St_db_Maker        *mDb           = 0;
StEmcADCtoEMaker   *bemcAdc2E     = 0;

Int_t               count         = 0;
Int_t               stat          = 0; 

Int_t prescale = 1; 

void makeHistFromMuDst( Int_t nevents = -1, 
		//			 Char_t *name = "6149020.lis", 
		//			 Char_t *ofile= "6149020.root",
		//Char_t *name="10103044.lis",
		Char_t *name="10097097.lis",
		Char_t *ofile="temp.hist.root",
		Char_t *path = "", 
		Int_t nfiles = 100
		)
{
  int mRunNo=10103044;
  //int mRunNo=10097097;
  //int triggerChoice=1;//ZDC
  int triggerChoice=230531;//BHT3
  //int triggerChoice=230010;//BBC


  //gROOT -> LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  //loadSharedLibraries();
  //gSystem->Load("TH1F");
  //  gSystem->Load("libgeometry_Tables");
  //  gSystem->Load("StDaqLib");
  //gSystem->Load("StEmcRawMaker");
  //gSystem->Load("StEmcADCtoEMaker");

  gROOT->Macro("loadMuDst.C");
  gSystem->Load("StDaqLib");
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StEmcRawMaker");
  gSystem->Load("StEmcADCtoEMaker");
  gSystem->Load("StDbBroker");


  gMessMgr -> SwitchOff("D");
  gMessMgr -> SwitchOff("I");
  TString pathname = path; 
  pathname += name;
  mChain = new StChain("chain");



  mMuDstMaker = new StMuDstMaker(0,0,path,name,"MuDst",nfiles);
  mDb = new St_db_Maker("StarDb", "MySQL:StarDb", "$STAR/StarDb");
  bemcAdc2E = new StEmcADCtoEMaker();

#if 0
  mMuDstMaker->SetStatus("*",0);
  mMuDstMaker->SetStatus("MuEvent",1);
  mMuDstMaker->SetStatus("EmcAll",1);
  mMuDstMaker->SetStatus("PrimaryTracks",1);
#endif 

  //StMuDbReader *db = StMuDbReader::instance();
  //StDetectorDbMaker *detdb = new StDetectorDbMaker();  
  //  mStarDatabase = new St_db_Maker("StarDb", "MySQL:StarDb");
  //  mStarDatabase = new St_db_Maker("StarDb", "MySQL:StarDb", "$STAR/StarDb");

  //get BEMC calibration 
  //  StEmcADCtoEMaker *bemcAdc2E = new StEmcADCtoEMaker(); // this will just convert what's in MuDst to ADC, use for data only!
  //  bemcAdc2E->setPrint(true);


  mChain->ls(3);
  mChain->Init();

  int seconds=1500;
  int bins=1500;
  TH1F *hTime=new TH1F("time","timing plot;t (s);hz",bins,0,seconds);
  TH2F *hModuleTime=new TH2F("moduleTime","module hits per second;t(s);module #",bins,0,seconds,120,0,120);
  TH2F *hPatchTime=new TH2F("patchTime","patch hits per second;t(s);module #",bins,0,seconds,300,0,300);
  TH1F *hNoHotTowers=new TH1F("noHotTowers","rate plot for good towers;t(s);hz",bins,0,seconds);
  TH2F *hPatch=new TH2F("patchADC","HT ADC spectrum per patch;ADC;Patch ID",80,-10,70,300,0,300);
  TH3F *hPatchXing=new TH3F("patchX","hits by time, patch, and bXing",bins,0,seconds,300,0,300,120,0,120);
  TH2F *hXingTime=new TH2F("timeX","hits by time and bXing",bins,0,seconds,120,0,120);
  TH2F *hZTime=new TH2F("zX","hits by time and BBC vertex z",bins,0,seconds,20,-100,100);

  //-----------------------------------------------------------------
  //--
  Int_t stat  = 0;    // error flag
  Int_t count = 0;    // event count
  Int_t baseTime=0;
  Int_t BHT3_trigger_id=230531;
  while ( stat == 0 ) {

    if (count%1000==0)
      {
	std::cout << "------------------------------------------------";
	std::cout << "event=" << count << std::endl;
      }

    if ( count++ >= nevents ) if ( nevents > 0 ) break;

    mChain -> Clear();
    stat = mChain -> Make();
    StMuDst *mudst=mMuDstMaker->muDst();
   //rezero at the time of the first event:
    if (count == 1 ) baseTime=mudst->event()->eventInfo().time();

    if ( mMuDstMaker->muDst()->event()->triggerIdCollection().nominal()->isTrigger(triggerChoice)){
	int eventTime=mudst->event()->eventInfo().time()-baseTime;
      hTime->Fill(eventTime,bins*1.0/(1.0*seconds));
      hZTime->Fill(eventTime,mudst->event()->bbcTriggerDetector().zVertex());
      //hXingTime->Fill(eventTime,mudst->event()->l0Trigger().bunchCrossingId7bit(mRunNo));

      //  TH3F *hPatchXing=new TH3F("patchX","hits by time, patch, and bXing",bins,0,seconds,300,0,300,120,0,120);
    
      if (triggerChoice==230531){//only makes sense to check high towers if we're using the BHT3 trig.   
	for (int m=0;m<300;m++)
	  {
	    hPatch->Fill(mudst->event()->emcTriggerDetector().highTower(m),m);
	    if (mudst->event()->emcTriggerDetector().highTower(m)>30)
	      {
		//printf("trigger on %d.  bXing=%d\n",m,mudst->event()->l0Trigger().bunchCrossingId7bit(mRunNo));
		hPatchXing->Fill(eventTime,m,mudst->event()->l0Trigger().bunchCrossingId7bit(mRunNo));
		hPatchTime->Fill(eventTime,m);
	      }
	  }
      }
      else {
	hPatchXing->Fill(eventTime,1,mudst->event()->l0Trigger().bunchCrossingId7bit(mRunNo));
      }

      
      
      }
  }
    printf("Total events: %d\n",count);
  //--
  //-----------------------------------------------------------------

  mChain -> Finish(); 

  //  hTime->Draw();

  /*  TH1F *hdHzdT=new TH1F("dHzdT","Rate change in each bin (bin+1)-(bin-1);dHz;t(s)",bins,0,seconds);

  for (int i=1;i<bins;i++)
    {
      hdHzdT->SetBinContent(i,hTime->GetBinContent(i+1)-hTime->GetBinContent(i-1));
    }

  hdHzdT->Draw();
  */
  TFile *out=new TFile(ofile,"recreate");
  out->cd();
  hTime->Write();
  hPatchTime->Write();
  hPatch->Write();
  hPatchXing->Write();
  hZTime->Write();
  //hdHzdT->Write();

  return;
    
}

