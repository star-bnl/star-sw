#include "StMuEEmcCrateTimingMaker.h"

#include "StChain.h"
#include "TF1.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"

#include "StMuDSTMaker/EZTREE/EztEventHeader.h"
#include "StMuDSTMaker/EZTREE/EztEmcRawData.h"

ClassImp(StMuEEmcCrateTimingMaker)

StMuEEmcCrateTimingMaker::StMuEEmcCrateTimingMaker(StMuDstMaker* mudstmaker){
  mMuDstMaker = mudstmaker;
  assert(mMuDstMaker);
  mNsigma = -1.0;
  mNchannels = 25;
  mPhase = 0;
  mCycle = 16;
  mMinCounts = 0;
}

// ----------------------------------------------------------------------------
Int_t StMuEEmcCrateTimingMaker::Init() {
  
  /// Setup output file
  TString outputFile;
  outputFile = mDirectory + "/eemc" + mFlavor + "Crates-timeDelay";
  char samus[100];
  sprintf(samus,"%0.2f.root",mTimeDelay);
  outputFile += samus;
  mOutputFile = new TFile(outputFile.Data(),"RECREATE");
  mOutputTree = new TTree("ints","Integrals of Each Channel");
  kludge = MxMapmtFeeCh;

  /// TTree branches hold a single entry, the total number
  /// of events, channel integrals, and delay settings
  mOutputTree->Branch("kludge",&kludge,"kludge/I");
  mOutputTree->Branch("chanint",totalIntegral,"chanint[kludge]/F");
  mOutputTree->Branch("chanerr",totalError,"chanerr[kludge]/F");
  mOutputTree->Branch("delay",&mTimeDelay,"delay/F");
  mOutputTree->Branch("channelIds",channelIds,"channelIds[kludge]/I");
  mOutputTree->Branch("crateIds",crateIds,"crateIds[kludge]/I");
  mOutputTree->Branch("mPhase",&mPhase,"mPhase/I");// for selecting which channels
  mOutputTree->Branch("mCycle",&mCycle,"mCycle/I");
  mOutputFile->cd();
  TString tmpstr = "histograms for " + mFlavor + " crate channels";

  /// Histogram of ADC response vs channel 
  cratehist = new TH2F("cratehistogram",tmpstr.Data(),MxMapmtFeeCh,-0.5,MxMapmtFeeCh + 0.5,500,0,500);
  cratehist->GetXaxis()->SetTitle("crate channel id");
  cratehist->GetYaxis()->SetTitle("adc");

  /// Initialize output arrays to zero
  for(int i=0; i<MxMapmtFeeCh; i++) {
    totalIntegral[i] = 0;
    totalError[i]=0;
    channelIds[i]=0;
  }

  return 0;
}

// ----------------------------------------------------------------------------
Int_t StMuEEmcCrateTimingMaker::Make(){
    
  //..........  acuire EztHeader
  EztEventHeader* header= mMuDstMaker->muDst()->eztHeader();
  if(header==0)
    return kStOK;
 
  /// Perform corruption checking and reject events
  int token= header->getToken();
  EztEmcRawData* eE;
  int lenCount;
  if(mFlavor == "tower") {
    eE=mMuDstMaker->muDst()->eztETow();
    lenCount = 4+160;
  } else if(mFlavor == "mapmt") {
    eE=mMuDstMaker->muDst()->eztESmd();
    lenCount = 4+192;
  }
  else {
    std::cout << "Unknown flavor you fool! " << mFlavor << std::endl;
    return kStWarn;
  }
    
  assert(eE);
  //$$$  eE->print(0);

  // test for corruption, accept only healthy ETOW events
  int errFlag=0;
  int trigComm=0x4; // physics, 9=laser/LED, 8=??
  
  int nOK=0;
  for(int icr=0;icr<eE->getNBlocks();icr++) {
    if(eE->isCrateVoid(icr)) continue;
    if(eE->purgeCrateOFF(icr)) continue;
    int crID=icr+1;
    if (mFlavor=="mapmt") crID=64+icr;    
    eE->tagHeadValid(icr,token, crID,lenCount,trigComm,errFlag);
    UShort_t isSick=eE->getCorruption(icr);
    if(isSick) continue;
    nOK++;
  }

  if((mFlavor == "mapmt" && nOK!=MaxMapmtCrates) ||
     (mFlavor == "tower" && nOK!=MaxTwCrates)) {
    cout << "SICK EVENTS IN THIS RUN" << endl;
    return kStOK; // drop sick events
  }

  //eE->print(0);
  totalIntegral[MxMapmtFeeCh-1]++;



//  this takes each crate, and loops over every sixteenth channel,
//  starting from "mPhase"
//  this allows some flexibility, in case there is a problem with
//  starting on the first channel in each crate, or if there is
//  a definitely unplugged channel in each crate
//  it then saves the crate hit to histogram


  ///
  /// Looks to me like tower crates will be mapped as follows in 
  /// the TTree array.
  ///
  /// crate 1 = elements 00-11
  /// crate 2 = elements 12-23
  /// crate 3 = elements 24-35
  /// crate 4 = elements 36-47
  /// crate 5 = elements 48-49
  /// crate 6 = elements 60-71
  ///



  /// Loop over all blocks within the mapmt or tower data
  for(int icr=0; icr < eE->getNBlocks(); icr++ ) {

    if(eE->isCrateVoid(icr)) continue;
    const UShort_t* data=eE->data(icr);
    int j = icr*MaxMapmtCrateCh/16;

    ///
    /// Loop over every 16 channels in mapmt box or tower crate
    ///
    for(int i=mPhase;i<eE->sizeData(icr);i+=mCycle,j++) {
      
      if(i>=MaxMapmtCrateCh) continue; // ignore nonexistient channels
      float adc=data[i];
      int k=(icr*MaxMapmtCrateCh+i)/16;
      assert(k>=0 && k<MxMapmtFeeCh);
      cratehist->Fill(j,adc);
      /// Store the channel ID associated with
      /// the jth histogram bin (redundant, but
      /// for now safer...)
      channelIds[j]=i;
      crateIds[j]=icr;

    }  

  }
  return kStOK;
}


// ----------------------------------------------------------------------------
Int_t StMuEEmcCrateTimingMaker::Finish() {


  /// Final processing.  Loop over all channels in the channel histogram,
  /// and determine the number of ADC hits exceeding a user-specified cut
  /// above pedestal.


  /// ASCII output file
  TString outputFile = mDirectory + "/eemc" + mFlavor + "Crates-timeDelay";
  char samus[100];
  sprintf(samus,"%0.2f.txt",mTimeDelay);
  outputFile += samus;
  ofstream ofs(outputFile.Data()); 
  ofs << "-1" << "\t" << setprecision(5) << totalIntegral[MxMapmtFeeCh-1] << endl;  //normalization
  
  for (Int_t i = 2; i <= cratehist->GetXaxis()->GetNbins(); i++) {
    Int_t chanId = static_cast<Int_t>(cratehist->GetXaxis()->GetBinCenter(i));

    TH1D* proj = cratehist->ProjectionY("projTemp",i,i);
    if (proj) {
      
      /// Require a minimum number of counts (default
      /// is zero).
      if ( proj -> GetEntries() < mMinCounts ) 
	continue;

      Int_t maxBin = 0;
      Float_t maxValue = -1;
      for (Int_t j = 1; j < proj->GetXaxis()->GetNbins(); j++) {
        if (proj->GetBinContent(j) > maxValue) {
          maxBin = j;
          maxValue = proj->GetBinContent(j);
        }
      }
      Float_t pedMean = proj->GetXaxis()->GetBinCenter(maxBin);

      if ( !(( i - mPhase )%mCycle) ) {

	TString myname="channel"; myname+=chanId;
	TH1D *cl=(TH1D*)proj->Clone(myname);
	cl->Write();

      }

      
      // fit a gaussian to the pedestal peak
      TF1* gaus = new TF1("gaus","gaus");
      gaus->SetParameter(0,maxValue);
      gaus->SetParameter(1,pedMean);
      gaus->SetParameter(2,3.5);
      gaus->SetRange(pedMean-10,pedMean+10);
      proj->Fit(gaus,"0RQ");
      Float_t pedestalmean = gaus->GetParameter(1);
      Float_t pedestalwidth = gaus->GetParameter(2);

      /// Determine the minimum for the integration range
      Float_t xmin = pedestalmean;
      if ( mNsigma > 0. )
	xmin += mNsigma * pedestalwidth;
      else
	xmin += mNchannels;	      
      Int_t minBin = proj->GetXaxis()->FindFixBin( xmin );

      maxBin = proj->GetXaxis()->GetNbins() - 1;
      Stat_t nHitsAbovePedestal = proj->Integral(minBin,maxBin);

      /// Compute statistical error on ratio
      Float_t delR = 0.;
      if ( nHitsAbovePedestal > 0. && totalIntegral[MxMapmtFeeCh-1] > 0. ) {
	delR = 1.0 / nHitsAbovePedestal + 1.0 / totalIntegral[MxMapmtFeeCh-1];
      }
      
      totalIntegral[chanId] = nHitsAbovePedestal/totalIntegral[MxMapmtFeeCh-1];
      totalError[chanId] = delR * totalIntegral[chanId];
            
      ofs << chanId << "\t" << setprecision(5) << nHitsAbovePedestal/totalIntegral[MxMapmtFeeCh-1] << endl;
    }

  }
  ofs.close();
  
  if (mOutputFile) {
    mOutputTree->Fill();
    mOutputFile->Write();
    delete mOutputFile;
  }
  return kStOk;
}
