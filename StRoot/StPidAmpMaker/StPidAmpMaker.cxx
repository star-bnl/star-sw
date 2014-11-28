////////////////////////////////////////////////////////////////////////////
//
// $Id: StPidAmpMaker.cxx,v 1.12 2003/09/02 17:58:46 perev Exp $
//
// Authors: Aihong Tang
//
////////////////////////////////////////////////////////////////////////////
//
//  Description: produce hist. for PIDFitter, which builds PID tables.
//
////////////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
#include <stdlib.h>
#include <math.h>
#include "StMaker.h"
#include "StPidAmpMaker.h"
#include "StFlowMaker/StFlowMaker.h"
#include "StFlowMaker/StFlowEvent.h"
#include "StFlowMaker/StFlowConstants.h"
#include "StFlowMaker/StFlowSelection.h"
#include "StFlowMaker/StFlowCutTrack.h"
#include "StEnumerations.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "TVector2.h"
#include "TFile.h"
#include "TString.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TF1.h"
#include "StMessMgr.h"
#include "TMath.h"
#define PR(x) cout << "##### PidAmp " << (#x) << " = " << (x) << endl;

ClassImp(StPidAmpMaker)

//-----------------------------------------------------------------------

StPidAmpMaker::StPidAmpMaker(const Char_t* name): 
  StMaker(name),
  mSingleMultiplicityBin(1),
  MakerName(name) {

}

//-----------------------------------------------------------------------

StPidAmpMaker::~StPidAmpMaker() {
}

//-----------------------------------------------------------------------

Int_t StPidAmpMaker::Make() {
  // Make histograms


  // Get a pointer to StFlowEvent
  StFlowMaker* pFlowMaker = NULL;
  pFlowMaker = (StFlowMaker*)GetMaker("Flow");
  if (pFlowMaker) pFlowEvent = pFlowMaker->FlowEventPointer();
  if (pFlowEvent && pFlowSelect->Select(pFlowEvent)) {     // event selected

  if (pFlowEvent) FillParticleHistograms(); // fill particle histograms
    
  if (Debug()) StMaker::PrintInfo();

  }
  
  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StPidAmpMaker::Init() {
  // Book histograms

  
  TH1F dummyHisto("dummy","dummy",mNDedxBins,mDedxStart,mDedxEnd);

  pidHisto
  =new TH1F*[mSingleMultiplicityBin*mNDcaBins*mNChargeBins*mNPBins*mNEtaBins*mNNHitsBins];

  for(int m=0;m<mSingleMultiplicityBin;m++)        //
    for(int d=0; d<mNDcaBins;d++)             //0_3_inf
      for(int e=0; e<mNChargeBins;e++)        //0_minus 1_plus
        for(int i=0; i<mNPBins;i++)           //0-99
          for(int j=0; j<mNEtaBins;j++)       //0-9
            for(int k=0; k<mNNHitsBins; k++){ //0-5
	char *theName = new char[80];

        if (i<10){
	  sprintf(theName,"%d%d%d0%d%d%d",theMultBin,d,e,i,j,k);
	}else {
          sprintf(theName,"%d%d%d%d%d%d",theMultBin,d,e,i,j,k);
	}

	pidHisto[GetPositionInArray(m,d,e,i,j,k)]=
         new TH1F(dummyHisto);     
	pidHisto[GetPositionInArray(m,d,e,i,j,k)]->SetName(theName);
	pidHisto[GetPositionInArray(m,d,e,i,j,k)]->SetTitle(theName);

        cout<<"booking histo, mult. # "<<theMultBin<<", dca # "<<d<<", charge # "<<e<<", p # "<<i<<", eta # "<<j<<", ndedx # "<<k<<endl;
      }


  gMessMgr->SetLimit("##### StPidAmp", 2);
  gMessMgr->Info("##### StPidAmp: $Id: StPidAmpMaker.cxx,v 1.12 2003/09/02 17:58:46 perev Exp $");

  return StMaker::Init();
}




//-----------------------------------------------------------------------

void StPidAmpMaker::FillParticleHistograms() {
  // Fill histograms from the particles

     int multBin=0; //single multbin here.

  // Initialize Iterator
  StFlowTrackCollection* pFlowTracks = pFlowEvent->TrackCollection();
  StFlowTrackIterator itr;
  
  for (itr = pFlowTracks->begin(); itr != pFlowTracks->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;

    float eta       = pFlowTrack->Eta();
    int   charge    = pFlowTrack->Charge();
//VPunused    float dca       = pFlowTrack->Dca();
//VPunused    float dcaGlobal = pFlowTrack->DcaGlobal();
    int   fitPts    = pFlowTrack->FitPts();
    float mtm       = pFlowTrack->P();
    float dedx      = pFlowTrack->Dedx();
    float NDedxUsed = (fitPts-1.39589)/1.41276; //linear relation between mFitPts and NDedxUsed.

  int dcaBin=0;
  int chargeBin=0;
  int pBin=0;
  int etaBin=0;
  int nhitsBin=0;

  dcaBin=0;  //for p00he dst, they only produced primary tracks, so I let dcaBin=0.

     chargeBin= (charge>0) ? 1 : 0;
      
    pBin=int(mtm/((mPEnd-mPStart)/mNPBins));
    if (pBin>(mNPBins-1)) continue;
    
    etaBin=int(fabs(eta)/((mEtaEnd-mEtaStart)/mNEtaBins));
    if (etaBin>(mNEtaBins-1)) continue;

    nhitsBin=
      int(float(NDedxUsed)/(float(mNNHitsEnd-mNNHitsStart)/mNNHitsBins));
    nhitsBin=(nhitsBin>(mNNHitsBins-1)) ? (mNNHitsBins-1) : nhitsBin;
    nhitsBin=(nhitsBin<0) ? 0 : nhitsBin;
  
    if (dedx>mDedxStart && dedx<mDedxEnd) 
      pidHisto[GetPositionInArray(multBin,dcaBin,chargeBin,pBin,etaBin,nhitsBin)]->Fill(dedx);//multBin shoud be 0 if produce histo for mult bin seperatly.

  }


}


//---------------------------------------------------------------------------
Int_t StPidAmpMaker::GetPositionInArray(Int_t theMultBin, Int_t theDcaBin, Int_t theChargeBin, Int_t thePBin, Int_t theEtaBin, Int_t theNHitsBin){

    int totalEntry
      = mSingleMultiplicityBin*mNDcaBins*mNChargeBins*mNPBins*mNEtaBins*mNNHitsBins;
    
    int positionPointer=0;
    
    totalEntry=totalEntry/mSingleMultiplicityBin;
    positionPointer=positionPointer+totalEntry*theMultBin;
    
    totalEntry=totalEntry/mNDcaBins;
    positionPointer=positionPointer+totalEntry*theDcaBin;
    
    totalEntry=totalEntry/mNChargeBins;
    positionPointer=positionPointer+totalEntry*theChargeBin;
    
    totalEntry=totalEntry/mNPBins;
    positionPointer=positionPointer+totalEntry*thePBin;
    
    totalEntry=totalEntry/mNEtaBins;
    positionPointer=positionPointer+totalEntry*theEtaBin;
    
    totalEntry=totalEntry/mNNHitsBins;
    positionPointer=positionPointer+totalEntry*theNHitsBin;
    
    return positionPointer;
}    




//-----------------------------------------------------------------------

Int_t StPidAmpMaker::Finish() {

   //write out histograms
   for(int m=0;m<mSingleMultiplicityBin;m++)        //
    for(int d=0; d<mNDcaBins;d++)             //0_3_inf
      for(int e=0; e<mNChargeBins;e++) {      //0_minus 1_plus  

	char *theHistoFileName = new char[200];
	  sprintf(theHistoFileName,"./PidHisto_%d%d%d.root",theMultBin,d,e);

  TFile histoFile(theHistoFileName,"RECREATE");
  cout<<" writting histogram file "<<theHistoFileName<<endl;

   histoFile.cd();

  //a name for PIDFitter to identify
  /////////nameTag convention   1N+
  //  1 -> the second multiplicity bin
  //  N -> no primary tracks
  //  + -> positive charge tracks

  char* multName=new char[80];
  sprintf(multName,"%d",theMultBin);

  TString tempString;
  tempString.Append(multName);

  if (d==0) tempString.Append("P"); // dca<3
  else tempString.Append("N"); //dca>3
  
  if (e==0) tempString.Append("-"); //negative particle
  else tempString.Append("+"); //positive particle


   TNamed fileNameTag(tempString,tempString);

   fileNameTag.Write();


  TH1F* tempHisto=0;

   for (int i =0; i<mNPBins; i++)
     for (int j=0; j<mNEtaBins;j++)
       for (int k=0; k<mNNHitsBins;k++) {


	char *theName = new char[80];
        if (i<10)
	  sprintf(theName,"h0%d%d%d",i,j,k);
        else sprintf(theName,"h%d%d%d",i,j,k);

          histoFile.cd();
          tempHisto=new TH1F(*(pidHisto[GetPositionInArray(m,d,e,i,j,k)]));
          tempHisto->SetName(theName);
          tempHisto->SetTitle(theName);
          tempHisto->Write();
          delete tempHisto;

       }

   histoFile.Write();

   histoFile.Close();

      }


  // Print the selection object details
   //  pFlowSelect->PrintList();

  delete pFlowSelect;

  cout << endl;
  gMessMgr->Summary(3);
  cout << endl;

  return StMaker::Finish();
}

////////////////////////////////////////////////////////////////////////////
//
// $Log: StPidAmpMaker.cxx,v $
// Revision 1.12  2003/09/02 17:58:46  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.11  2003/04/30 20:37:56  perev
// Warnings cleanup. Modified lines marked VP
//
// Revision 1.10  2002/02/25 19:30:07  jeromel
// ... SetFormat() again
//
// Revision 1.9  2002/02/14 21:25:56  aihong
// re-install the new version
//
//  
////////////////////////////////////////////////////////////////////////////
