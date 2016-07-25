#include "Riostream.h"
#include "StDAQMaker/StDAQMaker.h"
#include "StDAQMaker/StDAQReader.h"
#include "StDAQMaker/StTPCReader.h"
#include "StSequence.hh"
#include "StDaqLib/TPC/trans_table.hh"
#include "StDaqLib/TPC/fee_pin.h"
#include "StDetectorDbMaker/StDetectorDbTpcRDOMasks.h"
#include "tpcReader.h"
static TFile *hfile = 0;
static TTree *tree = 0;
static Event *event = 0;
#ifdef Pixels
TClonesArray *Event::fgPixels = 0;
#endif
static Int_t OldRun = 0;
static StTPCReader *TpcReader = 0;
static StDAQReader *daq = 0;
static StMaker     *chain=StMaker::GetChain();
static Int_t nb = 0;
static const Int_t   Nruns = 9;
static const Int_t   NDefault = 4;
static const Int_t   NRUNS = Nruns; //NDefault;
static const Int_t   Runs[Nruns] = {4029062,4029063,4029064,4029065, 4031027,4029066,4029067, 4029068, 4029069};
//static const Float_t Volt[Nruns] = {     0,     25,     50,    100,     200,    400,    800,    1000,    1200};
static const Int_t Outer[Nruns]= {      0,    394,    862,   1780,    3600,   7330,  14700,   18500,   21600};
static const Int_t Inner[Nruns]= {      0,    394,    856,   1760,    3600,   7360,  14800,   18400,   21600};
//static Int_t iRun = -1;
static Int_t Mag = -1;
//______________________________________________________________________________
bool validPad(int aSector, int aRow, int aPad){
  static StDetectorDbTpcRDOMasks* mask = 0;
  static int tRDOFromRowAndPad[45][182];
  if(!mask) {
     mask = StDetectorDbTpcRDOMasks::instance();
    for(int tiFee=0;tiFee<182;tiFee++){
      for(int tiPin=0;tiPin<32;tiPin++){
        if(row_vs_fee[tiFee][tiPin]!=0 && pad_vs_fee[tiFee][tiPin]!=0){
	  tRDOFromRowAndPad[(row_vs_fee[tiFee][tiPin]-1)]
	    [(pad_vs_fee[tiFee][tiPin]-1)]=
	    rdo_vs_fee[tiFee][tiPin];
        }
      } 
    }
  }
  return mask->isOn(aSector,tRDOFromRowAndPad[aRow-1][aPad-1]);
}
//______________________________________________________________________________
Int_t tpcReader(){
  if (! daq ) {
    daq = (StDAQReader *) chain->GetDataSet("StDAQReader")->GetObject();
    if (! daq ) {cout << "no StDAQReader" << endl; return 3;}
  }
  if (! TpcReader) { 
    TpcReader = daq->getTPCReader();
    if (! TpcReader) {cout << " no TpcReader" << endl;  return 3;}
  }
  const Int_t EvtNum = daq->getEventNumber();
  const Int_t Run    = daq->getRunNumber();
  const UInt_t Date  = daq->getUnixTime();
  TDatime date(Date);
  cout << "Run\t" << Run << "\tEvent\t" << EvtNum 
       << "\tDate/Time\t" << date.GetDate() << "/" << date.GetTime() << endl;
  if (OldRun != Run) {
#if 0
    for (int i = 0; i < Nruns; i++) if (Run == Runs[i]) {iRun = i; break;}
    if (iRun < 0) return 3;
#endif
    if (hfile) {hfile->Write(); delete hfile;}
    hfile = new TFile(Form("run%i.root",Run),"RECREATE","TPC pulser run Tree");
    hfile->SetCompressionLevel(2);
    tree = new TTree("PulserP","TPC pulser tree");
    tree->SetAutoSave(1000000000);  // autosave when 1 Gbyte written
    Int_t bufsize = 64000;
    Int_t split = 9;
    if (split)  bufsize /= 4;
    if (! event) event = new Event();
    Int_t branchStyle = 1; //new style by default
    TTree::SetBranchStyle(branchStyle);
    TBranch *branch = tree->Branch("event", "Event", &event, bufsize,split);
    branch->SetAutoDelete(kFALSE);
    OldRun = Run;
  }
  const Int_t maxTB = 400;
  Int_t ADCs[maxTB];
  for (Int_t sector = 1;  sector <= 24; sector++) {
    for (Int_t row = 1; row <=45 ; row++) {
      UChar_t* padListRaw;
      Int_t NoPads = TpcReader->getPadList(sector,row,padListRaw);
      Float_t Max = 0;
      Float_t Sum = 0;
      for (Int_t pad = 1; pad <= NoPads; pad++) {
	if (validPad(sector,row,pad)) {
	  TPCSequence *lst;
	  Int_t rv,nseq;
	  rv = TpcReader->getSequences(sector,row,pad,nseq,lst);
	  StSequence *listOfSequences = (StSequence *) lst;
	  //	cout << "rv\t" << rv << "\tnseq\t" << nseq << endl;
	  // find time sequence with maximum sum
	  memset(ADCs, 0, maxTB*sizeof(Int_t));
	  Double_t AvADC = 0;
	  Int_t   NAvADC = 0;
	  Double_t AdcMax = 0;
	  Int_t   timeMax = 0;
	  event->Clear();
	  event->SetHeader(EvtNum,Run,Date,Mag);
	  event->SetSecRowPad(sector,row,pad,0); 
	  for(Int_t iseq=0;iseq<nseq;iseq++) {
	    Int_t startTimeBin=listOfSequences[iseq].startTimeBin;
	    UChar_t *pointerToAdc=listOfSequences[iseq].firstAdc;
	    Int_t seqLen=listOfSequences[iseq].length;
	    Int_t time = startTimeBin;
	    for(Int_t ibin = 0; ibin < seqLen && time < maxTB; ibin++, time++) {
	      //	    Int_t ADC=log8to10_table[*(pointerToAdc++)]; 
	      Int_t ADC=log8to10_table[pointerToAdc[ibin]]; 
	      ADCs[time] = ADC;
	      if (ibin > 10 && (NAvADC == 0 || TMath::Abs(ADC-AvADC) < 3)) {
		NAvADC++;
		AvADC = (AvADC*(NAvADC-1))/NAvADC +  ((Float_t) ADC)/NAvADC;
	      }
	      if (ADC > AdcMax) {AdcMax = ADC; timeMax = time;}
	    }
	  }
	  Double_t ADCsum = 0;
	  Double_t TimeAv = 0;
	  for (Int_t i = timeMax-1; i <= timeMax+1; i++) {
	    ADCsum += ADCs[i] - AvADC;
	    TimeAv += i*(ADCs[i] - AvADC);
	  }
	  TimeAv /= ADCsum; 
	  for (Int_t time = 10; time < maxTB; time++) {
	    if (TMath::Abs(ADCs[time]-AvADC) > 1) 
	      event->AddPixel(time-TimeAv,(ADCs[time]-AvADC)/ADCsum);
	  }
	  event->SetSumMax(Sum,Max,0,TimeAv);
	  nb += tree->Fill();  //fill the tree
	}
      }
    }
  }
  
  //  hfile->Write();
  //  tree->Print();
  return 0;
}
void Finish() {
  if (hfile) {
    hfile->Write(); 
    delete hfile;
  }
}
