/***************************************************************************
 *
 * $Id: StHiSpectra.cxx,v 1.1 2002/04/02 20:05:18 jklay Exp $                                    
 *
 * Author: Bum Choi, UT Austin, Apr 2002
 *
 ***************************************************************************
 *
 * Description:  Class for making highpt inclusive spectra from highpt
 *               uDST's
 *
 ***************************************************************************
 * 
 * $Log: StHiSpectra.cxx,v $
 * Revision 1.1  2002/04/02 20:05:18  jklay
 * Bums analysis tools for highpt uDSTs
 *
 *
 **************************************************************************/
#include "StHiSpectra.h"


const char* border = "*************************************************";

//__________________

StHiSpectra::StHiSpectra(const char* inputDir,
			 const char* outRootName,
			 const char* f)
  : StHiBaseAnalysis(inputDir,outRootName)
     
{

  mEfficiencyString = ""; mEfficiencyFileName = f;
  mEfficiencyMap = 0;
}

//__________________

StHiSpectra::~StHiSpectra()
{
}

//__________________
//
// reads in all the files in the directory
// by default, looks for files ending in minimc.root 
//

Int_t
StHiSpectra::initMore()
{

  //  char name[100];
  Int_t stat=0;

  TString fName = "fEfficiency";
  TFile* file=0; 
  if(mEfficiencyFileName==""){
    cout << "no efficiency file set " << endl;
  }
  else{
    file=new TFile(mEfficiencyFileName.Data());
    if(!file ||!file->IsOpen()){
      cout << "Cannot find efficiency file : " << mEfficiencyFileName << endl;
    }
    else{
        mEfficiencyMap = (TF2*)file->Get(fName.Data());
	delete file;
    }
  }
  if(!mEfficiencyMap){
    cout << "Cannot find efficency function : " << fName.Data() <<endl;
    cout << "WARNING-USING DUMMY EFFIENCY" << endl;
    mEfficiencyMap = new TF2("fEfficiency","1");
  }
  
  
  cout << "\tefficiency file : " << mEfficiencyFileName << endl;
  cout << "\tefficiency fcn title : " << mEfficiencyMap->GetTitle() << endl;

  /*
  if(mEfficiencyString==""){
    cout << "no efficiency string" << endl;
    stat++;
  }
  
  else{
    // create the efficiency function
    // x is eta, y is pt
    cout << "efficiency : " << mEfficiencyString << endl;
    
    mEfficiencyMap = new TF2("fEfficiencyMap",mEfficiencyString,-.55,.55,1.6,6);
  }
  */
  return stat;
}


void
StHiSpectra::initHistograms()
{
  cout << "StHiSpectra::initHistograms()" << endl;

  //*********************

  using namespace Bin;

  //*********************

  //  gStyle->SetPalette(1,0);

  char title[500],name[500];

  h1CentralityCut = new TH1D("h1CentralityCut","centrality cut",
			     nFlowCentBin,flowCentMin,flowCentMax);

  h1Centrality = new TH1D("h1Centrality","centrality",
			     nFlowCentBin,flowCentMin,flowCentMax);

  h1VertexZ = new TH1D("h1VertexZ","vertex z",
		       nVertexZEvtBin,vertexZEvtMin,vertexZEvtMax);

  sprintf(title,"vertex z (%d<=zdc cent<=%d)",
	  CutRc::mZdcCtbCent[1],CutRc::mZdcCtbCent[0]);
  h1VertexZCut = new TH1D("h1VertexZCut",title,
			  nVertexZEvtBin,vertexZEvtMin,vertexZEvtMax);

  h1VertexZThin = new TH1D("h1VertexZThin","vertex z",
			   nVertexZEvtThinBin,vertexZEvtMin,vertexZEvtMax);

  sprintf(title,"vertex r %d<=zdc cent<=%d)",
	  CutRc::mZdcCtbCent[1],CutRc::mZdcCtbCent[0]);
  h1VertexRCut = new TH1D("h1VertexRCut",title,
			  nVertexREvtBin,vertexREvtMin,vertexREvtMax);
  /*
  Double_t ary0[] 
    = {
    1.6,1.7,1.8,1.9,2.0,2.25,2.55,3.0,3.5,4.1,4.9,6.0,7.0,8.0
      };
  Double_t ary1[]
    = {
    1.6,1.7,1.8,1.9,2.0,2.2,2.45,2.7,3.0,3.35,3.8,4.4,5.1,6.0,7.0,8.0
      };
   
  varBin[0].nPtBinAry = sizeof(ary0)/sizeof(Double_t) - 1;
  varBin[1].nPtBinAry = sizeof(ary1)/sizeof(Double_t) - 1;
  
  varBin[0].ptBinsAry = new TArrayD(varBin[0].nPtBinAry+1,ary0);
  varBin[1].ptBinsAry = new TArrayD(varBin[1].nPtBinAry+1,ary1);
   */
  cout << "using bins" << endl;
  for(int iBin=0; iBin<2; iBin++){
    varBin[iBin].ptBinsAry = new TArrayD;
    initPtAry(varBin[iBin].ptBinsAry,iBin);
    int nBin = varBin[iBin].ptBinsAry->GetSize()-1;
    varBin[iBin].nPtBinAry = nBin;;
    cout << "Bin " << iBin << " : " << endl;
    for(int i=0; i<nBin; i++){
      cout << varBin[iBin].ptBinsAry->At(i) << ", ";
    }
    cout << varBin[iBin].ptBinsAry->At(nBin) << endl;
  }
  //------------------------------------------------------------

  TString sPM[2] = { "Plus","Minus"};

  //********************************************************
  // number of events used to scale the spectra

  h1NEvent = new TH1D("h1NEvent","h1NEvent",1,1,2);

  // eta cut

  h1EtaCut = new TH1D("h1EtaCut","h1EtaCut",2,0,2);


  //********************************************************
  // yield for both signs

  h2Yield
    = new TH2D("h2Yield","h2Yield",
	       nEtaSmallBin,etaSmallMin,etaSmallMax,
	       nPtBin,ptMin,ptMax);
  h2Yield->Sumw2();

  
  

  //********************************************************
  // var bin
  
  for(Int_t iBin=0; iBin<mNVarBin; iBin++){

    //**** both charge signs

    setName(name,"h1OneOverPt",iBin);
    varBin[iBin].mean.h1OneOverPt =
      new TH1D(name,name,varBin[iBin].nPtBinAry,
	       varBin[iBin].ptBinsAry->GetArray());
    varBin[iBin].mean.h1OneOverPt->Sumw2();
    
    setName(name,"h1WeightedMean",iBin);
    varBin[iBin].mean.h1WeightedMean =
      new TH1D(name,name,varBin[iBin].nPtBinAry,
	       varBin[iBin].ptBinsAry->GetArray());
    varBin[iBin].mean.h1WeightedMean->Sumw2();


    //
    // raw
    //
    setName(name,"h1Raw",iBin);
    varBin[iBin].spec.h1Raw =
      new TH1D(name,name,varBin[iBin].nPtBinAry,
	       varBin[iBin].ptBinsAry->GetArray());
    varBin[iBin].spec.h1Raw->Sumw2();
    
    //
    // efficiency corrected
    //
    setName(name,"h1EffCorrected",iBin);
    varBin[iBin].spec.h1EffCorrected =
      new TH1D(name,name,varBin[iBin].nPtBinAry,
	       varBin[iBin].ptBinsAry->GetArray());
    varBin[iBin].spec.h1EffCorrected->Sumw2();
    
    //
    // corrected (background + momentum resolution)
    //
    setName(name,"h1Corrected",iBin);
    varBin[iBin].spec.h1Corrected =
      new TH1D(name,name,varBin[iBin].nPtBinAry,
	       varBin[iBin].ptBinsAry->GetArray());
    varBin[iBin].spec.h1Corrected->Sumw2();


    //**** spectra plus, minus

    for(Int_t iCharge=0; iCharge<2; iCharge++){

      // mean stuff
      //

      setName(name,"h1OneOverPt",iBin,sPM[iCharge].Data());
      varBin[iBin].meanPM[iCharge].h1OneOverPt =
	new TH1D(name,name,varBin[iBin].nPtBinAry,
		 varBin[iBin].ptBinsAry->GetArray());
      varBin[iBin].meanPM[iCharge].h1OneOverPt->Sumw2();

      setName(name,"h1WeightedMean",iBin,sPM[iCharge].Data());
      varBin[iBin].meanPM[iCharge].h1WeightedMean =
	new TH1D(name,name,varBin[iBin].nPtBinAry,
		 varBin[iBin].ptBinsAry->GetArray());
      varBin[iBin].meanPM[iCharge].h1WeightedMean->Sumw2();

      //
      // raw
      //
      setName(name,"h1Raw",iBin,sPM[iCharge].Data());
      varBin[iBin].specPM[iCharge].h1Raw =
	new TH1D(name,name,varBin[iBin].nPtBinAry,
		 varBin[iBin].ptBinsAry->GetArray());
      varBin[iBin].specPM[iCharge].h1Raw->Sumw2();

      //
      // efficiency corrected
      //
      setName(name,"h1EffCorrected",iBin,sPM[iCharge].Data());
      varBin[iBin].specPM[iCharge].h1EffCorrected =
	new TH1D(name,name,varBin[iBin].nPtBinAry,
		 varBin[iBin].ptBinsAry->GetArray());
      varBin[iBin].specPM[iCharge].h1EffCorrected->Sumw2();
      
      //
      // corrected (background + momentum resolution)
      //
      
      setName(name,"h1Corrected",iBin,sPM[iCharge].Data());
      varBin[iBin].specPM[iCharge].h1Corrected =
	new TH1D(name,name,varBin[iBin].nPtBinAry,
		 varBin[iBin].ptBinsAry->GetArray());
      varBin[iBin].specPM[iCharge].h1Corrected->Sumw2();
      
      
    }

    //***** ratios

    setName(name,"h1RawRatio",iBin);
    varBin[iBin].h1RawRatio
      = new TH1D(name,name,varBin[iBin].nPtBinAry,
		 varBin[iBin].ptBinsAry->GetArray());
    varBin[iBin].h1RawRatio->Sumw2();

    setName(name,"h1CorrectedRatio",iBin);
    varBin[iBin].h1CorrectedRatio
      = new TH1D(name,name,varBin[iBin].nPtBinAry,
		 varBin[iBin].ptBinsAry->GetArray());
    varBin[iBin].h1CorrectedRatio->Sumw2();

  }  // varBin
}

//_____________________

void 
StHiSpectra::trackLoop()
{
  if(mDebug)
    cout << "StHiSpectra::spectraTrackLoop()" << endl;

  Int_t nTrack = mHiMicroEvent->mNTrack;
  StHiMicroTrack* track;
  
  Float_t vertexZ = mHiMicroEvent->mVertexZ;

  for(Int_t i=0; i<nTrack; i++){
    track =(StHiMicroTrack*) mHiMicroEvent->tracks()->At(i);

    //***************** spectra  ***********************************
    
    if(!CutRc::AcceptTrackHalf(track,vertexZ)) continue;
    if(!CutRc::Accept(track)) continue; // cuts
    
    //**************************************************************
    Float_t ptPr = track->mPtPr;
    Float_t eta = track->mEtaPr;
    Float_t ptGl = track->mPtGl;
    //Float_t midZ = 100*TMath::Tan(track->mDipAnglePr) + vertexZ ;
    //Float_t exitZ = 200*TMath::Tan(track->mDipAnglePr) + vertexZ ;
    //
    // efficiency correction here
    //
    Float_t eff = mEfficiencyMap->Eval(eta,ptPr);
    
    Float_t weight = (1./eff);

    //
    // yield for both charge signs

    h2Yield->Fill(eta,ptPr);

    //
    // counts
    //
    if(ptPr>1.5&&ptPr<6){
      mNHiPtTrack++;
      if(mNHiPtTrack%10000==0){
	cout << "pt: " << ptPr << " eta: " << eta << " eff : " << eff << endl;
      }
    }
    //
    // spectra stuff here
    //

    Int_t iCharge = (track->mCharge>0) ? 0 : 1; //plus is 0

    for(Int_t iBin=0; iBin<mNVarBin; iBin++){
      
      //
      // both
      //
      
      varBin[iBin].spec.h1Raw->Fill(ptPr);
      varBin[iBin].mean.h1OneOverPt->Fill(ptPr,1./ptPr);
      varBin[iBin].spec.h1EffCorrected->Fill(ptPr,weight);
      varBin[iBin].spec.h1Corrected->Fill(ptPr,weight);
     

      //
      // plus/minus
      //

      // mean stuff
      //
      varBin[iBin].meanPM[iCharge].h1OneOverPt->Fill(ptPr,1./ptPr);

      varBin[iBin].specPM[iCharge].h1Raw->Fill(ptPr);
      varBin[iBin].specPM[iCharge].h1EffCorrected->Fill(ptPr,weight);
      varBin[iBin].specPM[iCharge].h1Corrected->Fill(ptPr,weight);

      // vertex z east, west - make sure they stay on the same side
      //

    } // varbin

  }
}

//_____________________

void
StHiSpectra::fillEventHistograms()
{
  h1Centrality->Fill(mHiMicroEvent->mCentrality);

  h1VertexZ->Fill(mHiMicroEvent->mVertexZ);
		  
  h1VertexZThin->Fill(mHiMicroEvent->mVertexZ);

  if(CutRc::AcceptVertexZ(mHiMicroEvent))
    h1CentralityCut->Fill(mHiMicroEvent->mCentrality);

  if(CutRc::AcceptZdcCtbCent(mHiMicroEvent)){
    h1VertexZCut->Fill(mHiMicroEvent->mVertexZ);
    //h1VertexRCut->Fill(mHiMicroEvent->mVertexR);
  }
}


//______________________

Bool_t
StHiSpectra::acceptEvent(StHiMicroEvent* event)
{
  return CutRc::Accept(event);

}


//______________________

void
StHiSpectra::finishHistograms()
{
  h1NEvent->SetBinContent(1,mNEventAccepted);
  h1EtaCut->SetBinContent(1,CutRc::mEta[0]);
  h1EtaCut->SetBinContent(2,CutRc::mEta[1]);

  for(Int_t iBin=0; iBin<mNVarBin; iBin++){

    varBin[iBin].mean.h1WeightedMean->Divide(varBin[iBin].spec.h1Raw,varBin[iBin].mean.h1OneOverPt);
    
    for(Int_t iCharge=0; iCharge<2; iCharge++){

      varBin[iBin].meanPM[iCharge].h1WeightedMean->Divide(varBin[iBin].specPM[iCharge].h1Raw,varBin[iBin].meanPM[iCharge].h1OneOverPt);

    }
  }

}



//_____________________________________

ClassImp(StHiSpectra)
