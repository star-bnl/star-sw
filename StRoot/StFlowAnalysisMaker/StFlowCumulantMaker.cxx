///////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCumulantMaker.cxx,v 1.3 2001/11/09 21:14:50 posk Exp $
//
// Authors:  Aihong Tang, Kent State U. Oct 2001
//           Frame adopted from Art and Raimond's StFlowAnalysisMaker.
//
///////////////////////////////////////////////////////////////////////////////
//
// Description:  Maker to analyze Flow using the latest cumulant method.
//                refer to Phy. Rev. C63 (2001) 054906 (new new method)
//                and      Phy. Rev. C62 (2000) 034902 (old new method)
//            all Eq. number tag are from new method paper if not specified.
//
///////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <math.h>
#include "float.h"
#include "StMaker.h"
#include "StFlowCumulantMaker.h"
#include "StFlowAnalysisMaker.h"
#include "StFlowMaker/StFlowMaker.h"
#include "StFlowMaker/StFlowEvent.h"
#include "StFlowTagMaker/StFlowTagMaker.h"
#include "StFlowMaker/StFlowConstants.h"
#include "StFlowMaker/StFlowSelection.h"
#include "StFlowMaker/StFlowCutTrack.h"
#include "StEnumerations.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "TFile.h"
#include "TString.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TProfile.h"
#include "TProfile2D.h"
#include "TF1.h"
#include "TOrdCollection.h"
#include "StMessMgr.h"
#include "TMath.h"
#define PR(x) cout << "##### FlowCumulantAnalysis: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowCumulantMaker)

//-----------------------------------------------------------------------

StFlowCumulantMaker::StFlowCumulantMaker(const Char_t* name): StMaker(name),
  MakerName(name) {
  pFlowSelect = new StFlowSelection();
  mOldMethod=kFALSE;
}

StFlowCumulantMaker::StFlowCumulantMaker(const Char_t* name,
					 const StFlowSelection& flowSelect) :
  StMaker(name), MakerName(name) {
  pFlowSelect = new StFlowSelection(flowSelect); //copy constructor
  mOldMethod=kFALSE;
}

//-----------------------------------------------------------------------

StFlowCumulantMaker::~StFlowCumulantMaker() {
}

//-----------------------------------------------------------------------

Int_t StFlowCumulantMaker::Make() {
  // Make histograms

  // Get a pointer to the flow tags
  StFlowTagMaker* pFlowTagMaker = NULL;
  pFlowTag = NULL;
  pFlowTagMaker = (StFlowTagMaker*)GetMaker("FlowTag");
  if (pFlowTagMaker) pFlowTag = pFlowTagMaker->TagPointer();

  // Get a pointer to StFlowEvent
  StFlowMaker* pFlowMaker = NULL;
  pFlowMaker = (StFlowMaker*)GetMaker("Flow");
  if (pFlowMaker) pFlowEvent = pFlowMaker->FlowEventPointer();
  if (pFlowEvent && pFlowSelect->Select(pFlowEvent)) {     // event selected

    // Event quantities
    if (pFlowTag) {
      FillFromTags();                        // get event quantities
      FillEventHistograms();                 // fill from Flow Tags
    } else if (pFlowEvent) {
      gMessMgr->Info("##### FlowCumulantAnalysis: FlowTag pointer null");
      FillFromFlowEvent();                   // get event quantities
      FillEventHistograms();                 // fill from FlowEvent
    } else {
      gMessMgr->Info("##### FlowCumulantAnalysis: FlowEvent and FlowTag pointers null");
      return kStOK;
    }
    // Particle quantities
    if (pFlowEvent) FillParticleHistograms(); // fill particle histograms
    
    if (Debug()) StMaker::PrintInfo();
  }
  
  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StFlowCumulantMaker::Init() {
  // Book histograms

  xLabel = "Pseudorapidity";
  if (strlen(pFlowSelect->PidPart()) != 0) { xLabel = "Rapidity"; }

  // Commit with these values.
  const float etaMin          =  -4.5;
  const float etaMax          =   4.5;
  const float ptMin           =    0.;
  const float ptMax           =    8.;

  enum { // commit with this value
	 nEtaBins          = 90,
	 nPtBins           = 40,
  };
  

  if (mOldMethod) r0=0.06;
   else  r0=1.5; //  this number should be small, but it could bring numerical 
                 //error if it is too small.

   m_M=1;  //  if m_M =2, what measured is v2,v4, v6... etc. for harmonic 1,2,3
           //  m_M=2 is not working. do not know why at the moment.
 
   Bool_t   mNoDenomeFileWarned=kFALSE;
   TString* histTitle;

  for (int k = 0; k < Flow::nSels; k++) {
    char countSels[2];
    sprintf(countSels,"%d",k+1);

    // for each selection

      histFull[k].mHistCumulant
           = new TProfile*[Flow::nCumulantDifferentialOrders];


     for (int ddd=0; ddd<Flow::nCumulantDifferentialOrders; ddd++){
      char theCumulantOrder[2]; //if >10, need to use char*
        sprintf(theCumulantOrder,"%d",(ddd+1)*2);
      histTitle = new TString("Flow_Cumulant_Order"); 
      histTitle->Append(*theCumulantOrder);           
      histTitle->Append("_Sel");                      
      histTitle->Append(*countSels);          
      histFull[k].mHistCumulant[ddd] =  
          new TProfile(histTitle->Data(), histTitle->Data(), Flow::nHars, 0.5, (float)(Flow::nHars) + 0.5, -1.*FLT_MAX, FLT_MAX, "");
      histFull[k].mHistCumulant[ddd]->SetXTitle("harmonic");
      delete histTitle;
     }



    
    // for each harmonic
    for (int j = 0; j < Flow::nHars; j++) {

      char countHars[2];
      sprintf(countHars,"%d",j+1);

    //            ****  for differential flow   ****

    // cumulant        dp/n{k}      
    //p is the harmonic with which we want to look at differential flow 
    //n is the harmonic of integrated flow whith which differential flow
    //is measured with respect to. p = either n or 2n.

    //if m_M=1, k=2    dn/n{2}  : cumulant from 2-part corr.
    //if m_M=1, k=4    dn/n{4}  : cumulant from 4-part corr.
    //if m_M=2, k=2    d2n/n{3} : cumulant from 3-part corr., mixed harmonic
    //if m_M=2, k=4    d2n/n{5} : cumulant from 5-part corr., mixed harmonic
    //where {2},{3} corresponds to theCumulantOrder=1 below.
    //{4},{5} corresponds to theCumulantOrder=2 below.

      histFull[k].histFullHar[j].mHistCumulant2D  =  
          new TProfile2D*[Flow::nCumulantDifferentialOrders];
      histFull[k].histFullHar[j].mHistCumulantEta = 
          new TProfile*[Flow::nCumulantDifferentialOrders];
      histFull[k].histFullHar[j].mHistCumulantPt  = 
          new TProfile*[Flow::nCumulantDifferentialOrders];


     for (int ddd=0; ddd<Flow::nCumulantDifferentialOrders; ddd++){

      char theCumulantOrder[2]; //if >10, need to use char*

        sprintf(theCumulantOrder,"%d",(ddd+1)*2);

      histTitle = new TString("Flow_Cumulant2D_Order");
      histTitle->Append(*theCumulantOrder);
      histTitle->Append("_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistCumulant2D[ddd] =	
        new TProfile2D(histTitle->Data(),histTitle->Data(), nEtaBins, etaMin, etaMax, nPtBins, ptMin, ptMax, -1.*FLT_MAX, FLT_MAX, "");
      histFull[k].histFullHar[j].mHistCumulant2D[ddd]->SetXTitle((char*)xLabel.Data());
      histFull[k].histFullHar[j].mHistCumulant2D[ddd]->SetYTitle("Pt (GeV)");
      delete histTitle;


      histTitle = new TString("Flow_CumulantEta_Order");
      histTitle->Append(*theCumulantOrder);
      histTitle->Append("_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistCumulantEta[ddd] =  
            new TProfile(histTitle->Data(),histTitle->Data(), nEtaBins, etaMin, etaMax, -1.*FLT_MAX, FLT_MAX, "");
      histFull[k].histFullHar[j].mHistCumulantEta[ddd]->SetXTitle((char*)xLabel.Data());
      delete histTitle;

      histTitle = new TString("Flow_CumulantPt_Order");
      histTitle->Append(*theCumulantOrder);
      histTitle->Append("_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mHistCumulantPt[ddd] =  
          new TProfile(histTitle->Data(), histTitle->Data(), nPtBins, ptMin, ptMax, -1.*FLT_MAX, FLT_MAX, "");
      histFull[k].histFullHar[j].mHistCumulantPt[ddd]->SetXTitle("Pt (GeV/c)");
      delete histTitle;



     }


      histFull[k].histFullHar[j].mCumuDifferentialG0Denominator2D = 
          new TProfile2D*[Flow::nCumulantDifferentialOrders*Flow::nCumulantDifferential_qMax];
      histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorEta = 
          new TProfile*[Flow::nCumulantDifferentialOrders*Flow::nCumulantDifferential_qMax];
      histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorPt = 
          new TProfile*[Flow::nCumulantDifferentialOrders*Flow::nCumulantDifferential_qMax];



      for (int ifcn =0; ifcn <  Flow::nCumulantDifferentialOrders*Flow::nCumulantDifferential_qMax; ifcn++){ 

	TString* histTitleDifferentialDenomPt; //for read in

	int cumulantIndex = (ifcn)/(Flow::nCumulantDifferential_qMax)+1; //like 1,2,3. (for cumulant order 2,4,6) not begin with 0. which is "p" in Eq. (B1) in the paper.
        int qIndex        = (ifcn)%(Flow::nCumulantDifferential_qMax); //like 0,1,..._qMax-1  begin with 0. which is "q" in Eq. (B3) in the paper.

	char theCumulantOrderChar[2];
        char qIndexOrderChar[2]; //if >10, need to use char*

	sprintf(theCumulantOrderChar,"%d",cumulantIndex*2); 
        sprintf(qIndexOrderChar,"%d",qIndex);

      histTitle = new TString("Flow_");
      histTitle->Append("CumulantDenom2D_Order");
      histTitle->Append(*theCumulantOrderChar);
      histTitle->Append("_GenFcnqIdx");
      histTitle->Append(*qIndexOrderChar);
      histTitle->Append("_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mCumuDifferentialG0Denominator2D[ifcn] =
            new TProfile2D(histTitle->Data(),histTitle->Data(), nEtaBins, etaMin, etaMax, nPtBins, ptMin, ptMax,-1.*FLT_MAX, FLT_MAX, "");
      histFull[k].histFullHar[j].mCumuDifferentialG0Denominator2D[ifcn]->SetXTitle((char*)xLabel.Data());
      histFull[k].histFullHar[j].mCumuDifferentialG0Denominator2D[ifcn]->SetYTitle("Pt (GeV)");
      delete histTitle;

      histTitle = new TString("Flow_");
      histTitle->Append("CumulantDenomEta_Order");
      histTitle->Append(*theCumulantOrderChar);
      histTitle->Append("_GenFcnqIdx");
      histTitle->Append(*qIndexOrderChar);
      histTitle->Append("_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorEta[ifcn] =
           new TProfile(histTitle->Data(),histTitle->Data(), nEtaBins, etaMin, etaMax, -1.*FLT_MAX, FLT_MAX, "");
      histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorEta[ifcn]->SetXTitle((char*)xLabel.Data());
      delete histTitle;

      histTitle = new TString("Flow_");
      histTitle->Append("CumulantDenomPt_Order");
      histTitle->Append(*theCumulantOrderChar);
      histTitle->Append("_GenFcnqIdx");
      histTitle->Append(*qIndexOrderChar);
      histTitle->Append("_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
      histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorPt[ifcn] = 
            new TProfile(histTitle->Data(),histTitle->Data(), nPtBins, ptMin, ptMax, -1.*FLT_MAX, FLT_MAX, "");
      histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorPt[ifcn]->SetXTitle("Pt (GeV/c)");
      histTitleDifferentialDenomPt = new TString(histTitle->Data());
      delete histTitle;

      TFile f("denominator.root","R"); //do not move this and f.close() around

      if (f.IsOpen()) {

      f.cd();

      TProfile* tempDenomPtProfile = 
         (TProfile *)f.Get(histTitleDifferentialDenomPt->Data());
      delete  histTitleDifferentialDenomPt;     

      double tempDenomIntegral =0.;
      double tempDenomInteNoZeroBins =0.;

      for (int theBin =0; theBin<tempDenomPtProfile->GetNbinsX(); theBin++){

        if (tempDenomPtProfile->GetBinContent(theBin)){
          tempDenomIntegral +=(tempDenomPtProfile->GetBinContent(theBin))*
                               (tempDenomPtProfile->GetBinEntries(theBin));
          tempDenomInteNoZeroBins +=tempDenomPtProfile->GetBinEntries(theBin);

        }
      } //could use TProfile::GetMean(2), but it seems there is bug in ROOT.

      histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorRead[ifcn]
        = tempDenomIntegral/tempDenomInteNoZeroBins;

      f.Close();

      } else {
        if(!mNoDenomeFileWarned) {
        gMessMgr->Info("##### FlowCumulantAnalysis:denominator.root is not present,assumming this run is just for producing denominator.root. that means cumulant flow result in flow.cumulant.root is nonsense for this run. ");
	mNoDenomeFileWarned=kTRUE;
	}

       histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorRead[ifcn]=1.; 
      }

      Double_t theTempPhi =  twopi*((Double_t)(qIndex))/((Double_t)(Flow::nCumulantDifferential_qMax)); 
      Double_t theRz = r0*sqrt(cumulantIndex);
      histFull[k].histFullHar[j].mDifferentialXz[ifcn]= theRz*cos(theTempPhi);
      histFull[k].histFullHar[j].mDifferentialYz[ifcn]= theRz*sin(theTempPhi);

      }

      //      for integrated flow stuff  

      for (int ifcn =0; ifcn <  Flow::nCumulantIntegratedOrders*Flow::nCumulantIntegrated_qMax; ifcn++){   

	int cumulantIndex = (ifcn)/(Flow::nCumulantIntegrated_qMax)+1; //like 1,2,3.  not begin with 0. which is "p" in Eq. (B1) in the paper.
        int qIndex        = (ifcn)%(Flow::nCumulantIntegrated_qMax); //like 0,1,...5  begin with 0. just lie Eq. (B3) in the paper.


      Double_t theTempPhi =  twopi*((Double_t)(qIndex))/((Double_t)(Flow::nCumulantIntegrated_qMax)); 
      Double_t theRz = r0*sqrt(cumulantIndex);

      histFull[k].histFullHar[j].mIntegratedXz[ifcn]= theRz*cos(theTempPhi);
      histFull[k].histFullHar[j].mIntegratedYz[ifcn]= theRz*sin(theTempPhi);

      histFull[k].histFullHar[j].mCumuIntegratedG0[ifcn] = 0.;

      }

      histFull[k].histFullHar[j].mMultSum=0.;
      histFull[k].histFullHar[j].mWgtMultSum_q4=0.;
      histFull[k].histFullHar[j].mWgtMultSum_q6=0.;


    }
  }

  gMessMgr->SetLimit("##### FlowCumulantAnalysis", 2);
  gMessMgr->Info("##### FlowCumulantAnalysis: $Id: StFlowCumulantMaker.cxx,v 1.3 2001/11/09 21:14:50 posk Exp $");

  return StMaker::Init();
}

//-----------------------------------------------------------------------

void StFlowCumulantMaker::FillFromTags() {
//   Get the flow tags and calculate the full event quantities

  int nSels = 2, nHars = 6;

  for (int j = 0; j < nHars; j++) {

    mMultSub[0][j] = pFlowTag->na[j];
    mMultSub[1][j] = pFlowTag->nb[j];
    mMultSub[2][j] = pFlowTag->nc[j];
    mMultSub[3][j] = pFlowTag->nd[j];

    // full event quantities
    for (int k = 0; k < nSels; k++) {
      mMult[k][j] = mMultSub[nSels*k][j] + mMultSub[nSels*k+1][j];
    }
  }

}

//-----------------------------------------------------------------------

void StFlowCumulantMaker::FillFromFlowEvent() {
  // Get event quantities from StFlowEvent
  for (int k = 0; k < Flow::nSels; k++) {
    pFlowSelect->SetSelection(k);
    for (int j = 0; j < Flow::nHars; j++) {
      pFlowSelect->SetHarmonic(j);

      pFlowSelect->SetSubevent(-1);
      // full event quantities
      mMult[k][j]       = pFlowEvent->Mult(pFlowSelect);
      mWgtMult_q4[k][j] = pFlowEvent->WgtMult_q4(pFlowSelect);              
      mWgtMult_q6[k][j] = pFlowEvent->WgtMult_q6(pFlowSelect);              

    }
  }
}

//-----------------------------------------------------------------------

void StFlowCumulantMaker::FillEventHistograms() {
  // Fill histograms with event quantities


    for (int k = 0; k < Flow::nSels; k++) {
      pFlowSelect->SetSelection(k);
      for (int j = 0; j < Flow::nHars; j++) {
	pFlowSelect->SetHarmonic(j);

      histFull[k].histFullHar[j].mMultSum +=float(mMult[k][j]);
      histFull[k].histFullHar[j].mWgtMultSum_q4 +=mWgtMult_q4[k][j];
      histFull[k].histFullHar[j].mWgtMultSum_q6 +=mWgtMult_q6[k][j];

      histFull[k].histFullHar[j].mNEvent++;

      for (int ifcn =0; ifcn <  Flow::nCumulantIntegratedOrders*Flow::nCumulantIntegrated_qMax; ifcn++){   
     
	if (mOldMethod)
      histFull[k].histFullHar[j].mCumuIntegratedG0[ifcn] += 
	pFlowEvent->G_Old( pFlowSelect,  
                       histFull[k].histFullHar[j].mIntegratedXz[ifcn], 
                       histFull[k].histFullHar[j].mIntegratedYz[ifcn] );
        else
      histFull[k].histFullHar[j].mCumuIntegratedG0[ifcn] += 
	pFlowEvent->G_New( pFlowSelect,  
                       histFull[k].histFullHar[j].mIntegratedXz[ifcn], 
                       histFull[k].histFullHar[j].mIntegratedYz[ifcn] );
      }
     }
    }

}

//-----------------------------------------------------------------------

void StFlowCumulantMaker::FillParticleHistograms() {
  // Fill histograms from the particles

  // Initialize Iterator
  StFlowTrackCollection* pFlowTracks = pFlowEvent->TrackCollection();
  StFlowTrackIterator itr;

//In the view of a good coding, the following block should be placed inside
//the track loop, It is put here to reduce run time.
//If sb. change the pFlowSelect to select tracks for v-part, 
// he/she has to change pFlowSelect in this block for the consistency. 

  double* theEvtCrossTerm[Flow::nSels][Flow::nHars];
  double  theSqrtOfSumWgtSqr[Flow::nSels][Flow::nHars];


   for (int k = 0; k < Flow::nSels; k++) 
     for (int j = 0; j < Flow::nHars; j++) 
       theEvtCrossTerm[k][j] = 
 new double[Flow::nCumulantDifferentialOrders*Flow::nCumulantDifferential_qMax];

   for (int k = 0; k < Flow::nSels; k++) 
     for (int j = 0; j < Flow::nHars; j++) {
        pFlowSelect->SetSelection(k);
	pFlowSelect->SetHarmonic(j);

	theSqrtOfSumWgtSqr[k][j] = pFlowEvent->SumWeightSquare(pFlowSelect);

	    if (theSqrtOfSumWgtSqr[k][j]<0) 
            theSqrtOfSumWgtSqr[k][j]=double(mMult[k][j]);

	theSqrtOfSumWgtSqr[k][j]=sqrt( theSqrtOfSumWgtSqr[k][j] );   
        


       for (int ifcn =0; ifcn<Flow::nCumulantDifferentialOrders*Flow::nCumulantDifferential_qMax; ifcn++){

	theEvtCrossTerm[k][j][ifcn]= (mOldMethod) ?
        (pFlowEvent->G_Old( pFlowSelect,
                       histFull[k].histFullHar[j].mDifferentialXz[ifcn],
		       histFull[k].histFullHar[j].mDifferentialYz[ifcn] )) :
        (pFlowEvent->G_New( pFlowSelect,
                       histFull[k].histFullHar[j].mDifferentialXz[ifcn],
                       histFull[k].histFullHar[j].mDifferentialYz[ifcn] )) ;

       } 
     }
//////////////////end of the block




  for (itr = pFlowTracks->begin(); itr != pFlowTracks->end(); itr++) {
    StFlowTrack* pFlowTrack = *itr;

    float phi       = pFlowTrack->Phi();
    if (phi < 0.) phi += twopi;
    float eta       = pFlowTrack->Eta();
    float pt        = pFlowTrack->Pt();

    for (int k = 0; k < Flow::nSels; k++) {
      pFlowSelect->SetSelection(k);
       double cumuTemp[Flow::nCumulantDifferentialOrders];
       double cumuTempFlip[Flow::nCumulantDifferentialOrders];
       double order;
       double phiWgt;
      for (int j = 0; j < Flow::nHars; j++) {
	bool oddHar = (j+1) % 2;
	pFlowSelect->SetHarmonic(j);
	      order  = (double)(j+1);

	if (pFlowSelect->Select(pFlowTrack)) {
	  // Get detID
	  StDetectorId detId;
	  if (pFlowTrack->TopologyMap().numberOfHits(kTpcId) || 
	      (pFlowTrack->TopologyMap().data(0) == 0 && 
	       pFlowTrack->TopologyMap().data(1) == 0)) {
	    // Tpc track, or TopologyMap not available
	    detId = kTpcId;
	  } else if (pFlowTrack->TopologyMap().numberOfHits(kFtpcEastId)) {
	    detId = kFtpcEastId;
	  } else if (pFlowTrack->TopologyMap().numberOfHits(kFtpcWestId)) {
	    detId = kFtpcWestId;
	  } else {
	    detId = kUnknownId;
	  }

	  // Get phiWgt
	  phiWgt = pFlowEvent->PhiWeight(phi, k, j, 
						pFlowTrack->TopologyMap());


	  if (!pFlowEvent->EtaSubs()) {
	    if (eta < 0 && oddHar) phiWgt *= -1.;
	    if (pFlowEvent->PtWgt()) phiWgt *= pt;
	  }
	  

	}

       	// Caculate v for all particles selected for correlation analysis
	if (pFlowSelect->SelectPart(pFlowTrack)) {


	  float YOrEta = 
             (strlen(pFlowSelect->PidPart()) != 0) ?  pFlowTrack->Y() : eta;

	  double Dp[Flow::nCumulantDifferentialOrders]; // the Dp in (D6)
	    for (int ifcn=0; ifcn<Flow::nCumulantDifferentialOrders; ifcn++)
              Dp[ifcn]=0.;


       for (int ifcn =0; ifcn <  Flow::nCumulantDifferentialOrders*Flow::nCumulantDifferential_qMax; ifcn++){   

	int theCumulantOrder = (ifcn)/(Flow::nCumulantDifferential_qMax)+1; //like 1,2.  not begin with 0. which is "p" in (B6)
        int qIndex        = (ifcn)%(Flow::nCumulantDifferential_qMax); //like 0,1,...5  begin with 0. which is "q" in (B6)

        double theCoeff=pow(r0*sqrt(theCumulantOrder),m_M)/float(Flow::nCumulantDifferential_qMax); //first term in (B7)
        double theCosTerm=cos(twopi*float(qIndex)*float(m_M)/float(Flow::nCumulantDifferential_qMax)); //cos() in (B7)
        double theSinTerm=sin(twopi*float(qIndex)*float(m_M)/float(Flow::nCumulantDifferential_qMax)); //sin() in (B7)

	double theCrossterm = 
	  theEvtCrossTerm[k][j][ifcn]; //Gn(Zp,q) in (B6)


          if ( (pFlowSelect->SelectPart(pFlowTrack)) && 
               (pFlowSelect->Select(pFlowTrack)) ) { //remove auto term.

            if (mOldMethod)            
	    theCrossterm /= exp( (phiWgt/theSqrtOfSumWgtSqr[k][j]) *(2.*histFull[k].histFullHar[j].mDifferentialXz[ifcn]*cos(phi * order) + 2. *histFull[k].histFullHar[j].mDifferentialYz[ifcn]*sin(phi * order) ) );
            else
	    theCrossterm /= (1. + (phiWgt/mMult[k][j]) *(2.*histFull[k].histFullHar[j].mDifferentialXz[ifcn]*cos(phi * order) + 2. *histFull[k].histFullHar[j].mDifferentialYz[ifcn]*sin(phi * order) ) ); //the argument in the last paragraph in page 9. 

	  }

       double theXpq = ( theCrossterm*cos(float(m_M) * order * (phi)) )/ //(B6)
      histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorRead[ifcn]; 
       double theYpq = ( theCrossterm*sin(float(m_M) * order * (phi)) )/ //(B6)
      histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorRead[ifcn];



     Dp[theCumulantOrder-1] 
        += theCoeff*(theCosTerm*theXpq+theSinTerm*theYpq);//(B7)

     //for writting out denomenator, which is the denominator in (B6)
      histFull[k].histFullHar[j].mCumuDifferentialG0Denominator2D[ifcn]->
       Fill(YOrEta, pt, theCrossterm ); 
      histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorPt[ifcn]->
       Fill(pt, theCrossterm ); 
      histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorEta[ifcn]->
	Fill(YOrEta, theCrossterm );  
       }

       if (m_M==1){
         cumuTemp[0]=((2.*Dp[1-1])-(0.5*Dp[2-1]))/(r0*r0); //(B9)
         cumuTemp[1]=((-2.*Dp[1-1])+Dp[2-1])/(r0*r0*r0*r0);
       }else if (m_M==2){
         cumuTemp[0]=((4.*Dp[1-1])-(0.5*Dp[2-1]))/(r0*r0*r0*r0); //(B10)
         cumuTemp[1]=((-6.*Dp[1-1])+(1.5*Dp[2-1]))/(r0*r0*r0*r0*r0*r0);
       }

       cumuTempFlip[0]=cumuTemp[0];
       cumuTempFlip[1]=cumuTemp[1];
      if (eta < 0 && oddHar) {cumuTempFlip[0] *=-1.; cumuTempFlip[1] *=-1.;}

       for (int uuu=0; uuu<Flow::nCumulantDifferentialOrders; uuu++) {
      histFull[k].histFullHar[j].mHistCumulant2D[uuu]->
       Fill(YOrEta, pt,cumuTemp[uuu] ); 
      histFull[k].histFullHar[j].mHistCumulantEta[uuu]->
       Fill(YOrEta, cumuTemp[uuu] ); 
      histFull[k].histFullHar[j].mHistCumulantPt[uuu]->
       Fill(pt, cumuTempFlip[uuu] ); 
        }	  

       for (int uuu=0; uuu<Flow::nCumulantDifferentialOrders; uuu++)
      histFull[k].mHistCumulant[uuu]->Fill(order, cumuTempFlip[uuu] );

	}
      }
    }  
  }



}



//-----------------------------------------------------------------------

Int_t StFlowCumulantMaker::Finish() {

  TString* histTitle;

  TOrdCollection* XpqYpqDenomNames = new TOrdCollection(Flow::nSels*Flow::nHars); 


  for (int k = 0; k < Flow::nSels; k++) {
    char countSels[2];
    sprintf(countSels,"%d",k+1);

    cout << "##### selection "<<k+1<<"  #### "<<endl;

      //integrated flow from cumulant
       histFull[k].mHist_v
           = new TH1D*[Flow::nCumulantDifferentialOrders];

      for (int ddd =0; ddd<Flow::nCumulantDifferentialOrders; ddd++){

      char theCumulantOrder[2]; //if >10, need to use char*
      sprintf(theCumulantOrder,"%d",(ddd+1)*2);

      histTitle = new TString("Flow_v_cumulantOrder");
      histTitle->Append(*theCumulantOrder);
      histTitle->Append("_Sel");
      histTitle->Append(*countSels);
       histFull[k].mHist_v[ddd] = 
                new  TH1D(*(histFull[k].mHistCumulant[ddd]->ProjectionX(histTitle->Data(),"e")));
       histFull[k].mHist_v[ddd]->SetTitle(histTitle->Data());
       histFull[k].mHist_v[ddd]->SetXTitle("harmonic");
       histFull[k].mHist_v[ddd]->SetYTitle("v (%)");
      delete histTitle;
    AddHist(histFull[k].mHist_v[ddd]);

   }

      double  meanIntegratedV[Flow::nHars];     //V**1
      double  meanIntegratedV2[Flow::nHars];    //V**2
      double  meanIntegratedV3[Flow::nHars];    //V**3
      double  meanIntegratedV4[Flow::nHars];    //V**4
      double  cumulantIntegrated1[Flow::nHars]; //outside of harmonic loop
      double  cumulantIntegrated2[Flow::nHars];
      double  cumulantIntegrated3[Flow::nHars];
      double  q2[Flow::nHars]; //for old method. <Q>**2 in (74) old paper.
      double  q4[Flow::nHars];
      double  q6[Flow::nHars];



   for (int j=0; j< Flow::nHars; j++) {
      meanIntegratedV[j]=0.;
      meanIntegratedV2[j]=0.;
      meanIntegratedV3[j]=0.;
      meanIntegratedV4[j]=0.;
      cumulantIntegrated1[j]=0.;
      cumulantIntegrated2[j]=0.;
      cumulantIntegrated3[j]=0.;
   }

    for (int j = 0; j < Flow::nHars; j++) {
      char countHars[2];
      sprintf(countHars,"%d",j+1);

      //for cumulant method
      Double_t mAvMult = //average multiplicity
     float(histFull[k].histFullHar[j].mMultSum)/
      (float(histFull[k].histFullHar[j].mNEvent));

      Double_t mAvWgtMult_q4 = //for getting q4 with wgt
     float(histFull[k].histFullHar[j].mWgtMultSum_q4)/
      (float(histFull[k].histFullHar[j].mNEvent));

      Double_t mAvWgtMult_q6 = //for getting q6 with wgt
     float(histFull[k].histFullHar[j].mWgtMultSum_q6)/
      (float(histFull[k].histFullHar[j].mNEvent));




      Double_t CpIntegrated[Flow::nCumulantIntegratedOrders]; //Cp in (B4)

      for (int ifcn =0; ifcn < Flow::nCumulantIntegratedOrders; ifcn ++)
	CpIntegrated[ifcn]=0.;
      for (int ifcn =0; ifcn <  Flow::nCumulantIntegratedOrders*Flow::nCumulantIntegrated_qMax; ifcn++){   
	int theCumulantOrder = (ifcn)/(Flow::nCumulantIntegrated_qMax)+1; //like 1,2,3.  not begin with 0. which is "p" in (B3)
	//        int qIndex        = (ifcn)%(Flow::nCumulantIntegrated_qMax); //like 0,1,...5  begin with 0. "q" in (B3)
      histFull[k].histFullHar[j].mCumuIntegratedG0[ifcn] /= 
       float(histFull[k].histFullHar[j].mNEvent);        //<Gn(z)> 

      if (mOldMethod)
      CpIntegrated[theCumulantOrder-1] +=
	(log(histFull[k].histFullHar[j].mCumuIntegratedG0[ifcn])/float(Flow::nCumulantIntegrated_qMax));
      else
      CpIntegrated[theCumulantOrder-1] +=
	(mAvMult*(pow(histFull[k].histFullHar[j].mCumuIntegratedG0[ifcn],1./mAvMult)-1.)/float(Flow::nCumulantIntegrated_qMax)); //(B3) 
      }


//add Xpq Ypq denominator to write out file list
      for (int ifcn =0; ifcn <  Flow::nCumulantDifferentialOrders*Flow::nCumulantDifferential_qMax; ifcn++){
      XpqYpqDenomNames->AddLast(histFull[k].histFullHar[j].mCumuDifferentialG0Denominator2D[ifcn]);
      XpqYpqDenomNames->AddLast(histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorPt[ifcn]);
      XpqYpqDenomNames->AddLast(histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorEta[ifcn]);
		    }
         cumulantIntegrated1[j] = //(B5)
            (3.*CpIntegrated[1-1] -
        (3./2.)*CpIntegrated[2-1] +
        (1./3.)*CpIntegrated[3-1])/(r0*r0);
 
         cumulantIntegrated2[j] = 
           ((((-10.)*CpIntegrated[1-1]) + 
            (8.*CpIntegrated[2-1]) - 
            (2.*CpIntegrated[3-1]))/(r0*r0*r0*r0)); 
 
         cumulantIntegrated3[j]=(( (18.*CpIntegrated[1-1]) - (18.*CpIntegrated[2-1]) + (6.*CpIntegrated[3-1]))/(r0*r0*r0*r0*r0*r0));
 

     //now histograms for flow results:
       histFull[k].histFullHar[j].mHist_v2D
= new TH2D*[Flow::nCumulantDifferentialOrders];
       histFull[k].histFullHar[j].mHist_vEta
= new TH1D*[Flow::nCumulantDifferentialOrders];
       histFull[k].histFullHar[j].mHist_vPt
= new TH1D*[Flow::nCumulantDifferentialOrders];


      for (int ddd =0; ddd<Flow::nCumulantDifferentialOrders; ddd++){

      char theCumulantOrder[2]; //if >10, need to use char*
      sprintf(theCumulantOrder,"%d",(ddd+1)*2);

      histTitle = new TString("Flow_v2D_cumulantOrder");
      histTitle->Append(*theCumulantOrder);
      histTitle->Append("_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
       histFull[k].histFullHar[j].mHist_v2D[ddd] = 
                new TH2D(*(histFull[k].histFullHar[j].mHistCumulant2D[ddd]->ProjectionXY(histTitle->Data(),"e")));
       histFull[k].histFullHar[j].mHist_v2D[ddd]->SetTitle(histTitle->Data());
      histFull[k].histFullHar[j].mHist_v2D[ddd]->SetXTitle((char*)xLabel.Data());
      histFull[k].histFullHar[j].mHist_v2D[ddd]->SetYTitle("Pt (GeV)");
      histFull[k].histFullHar[j].mHist_v2D[ddd]->SetZTitle("v (%)");
      delete histTitle;

      histTitle = new TString("Flow_vEta_cumulantOrder");
      histTitle->Append(*theCumulantOrder);
      histTitle->Append("_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
       histFull[k].histFullHar[j].mHist_vEta[ddd] = 
                new  TH1D(*(histFull[k].histFullHar[j].mHistCumulantEta[ddd]->ProjectionX(histTitle->Data(),"e")));
       histFull[k].histFullHar[j].mHist_vEta[ddd]->SetTitle(histTitle->Data());
       histFull[k].histFullHar[j].mHist_vEta[ddd]->SetXTitle((char*)xLabel.Data());
       histFull[k].histFullHar[j].mHist_vEta[ddd]->SetYTitle("v (%)");
      delete histTitle;

      histTitle = new TString("Flow_vPt_cumulantOrder");
      histTitle->Append(*theCumulantOrder);
      histTitle->Append("_Sel");
      histTitle->Append(*countSels);
      histTitle->Append("_Har");
      histTitle->Append(*countHars);
       histFull[k].histFullHar[j].mHist_vPt[ddd] = 
                new  TH1D(*(histFull[k].histFullHar[j].mHistCumulantPt[ddd]->ProjectionX(histTitle->Data(),"e")));
       histFull[k].histFullHar[j].mHist_vPt[ddd]->SetTitle(histTitle->Data());
       histFull[k].histFullHar[j].mHist_vPt[ddd]->SetXTitle("Pt (GeV)");
       histFull[k].histFullHar[j].mHist_vPt[ddd]->SetYTitle("v (%)");
      delete histTitle;



}

     


       if (mOldMethod) {
           q2[j] = cumulantIntegrated1[j]-1.;  // old paper (Eq. 74a)
  	   q4[j] = -1.*cumulantIntegrated2[j]-(1./mAvWgtMult_q4); 
	   q6[j] = (1./4.)*cumulantIntegrated3[j]-(1./(mAvWgtMult_q6)); 
           meanIntegratedV[j]   = sqrt(q2[j]);        // <Q>  for 2-part,  m=1
           meanIntegratedV2[j]  = q2[j];              //<Q**2>for 2-part , m=2
           meanIntegratedV3[j]  = pow(q4[j],3./4.);   //<Q**3>for 4-part,  m=1
           meanIntegratedV4[j]  = q4[j];              //<Q**4>for 4-part,  m=2
       } else { //new method
          meanIntegratedV[j]     
            = sqrt(cumulantIntegrated1[j]);          //<v>    for 2-part,  m=1
          meanIntegratedV2[j]    
            = cumulantIntegrated1[j];                //<v**2> for 2-part , m=2
          meanIntegratedV3[j]    
            = pow(-1.*cumulantIntegrated2[j],3./4.); //<v**3> 4-part,  m=1
          meanIntegratedV4[j]    
            = -1.*cumulantIntegrated2[j];            //<v**4> 4-part,  m=2
       }


        if (m_M==1){

        histFull[k].histFullHar[j].mHist_v2D[0]->Scale(1./(meanIntegratedV[j]*perCent) ); 	//  (34a)

        histFull[k].histFullHar[j].mHist_v2D[1]->Scale(-1./(meanIntegratedV3[j]*perCent) ); //  (34b)

        histFull[k].histFullHar[j].mHist_vEta[0]->Scale(1./(meanIntegratedV[j]*perCent) ); 	//  (34a)

        histFull[k].histFullHar[j].mHist_vEta[1]->Scale(-1./(meanIntegratedV3[j]*perCent) ); //  (34b)


        histFull[k].histFullHar[j].mHist_vPt[0]->Scale(1./(meanIntegratedV[j]*perCent) ); 	//  (34a)

        histFull[k].histFullHar[j].mHist_vPt[1]->Scale(-1./(meanIntegratedV3[j]*perCent) ); //  (34b)


	}else if(m_M==2){

        histFull[k].histFullHar[j].mHist_v2D[0]->Scale(1./(meanIntegratedV2[j]*perCent) ); 	//  (35a)
        histFull[k].histFullHar[j].mHist_v2D[1]->Scale(-0.5/(meanIntegratedV4[j]*perCent) ); //  (35b)


        histFull[k].histFullHar[j].mHist_vEta[0]->Scale(1./(meanIntegratedV2[j]*perCent) ); 	//  (35a)
        histFull[k].histFullHar[j].mHist_vEta[1]->Scale(-0.5/(meanIntegratedV4[j]*perCent) ); //  (35b)


        histFull[k].histFullHar[j].mHist_vPt[0]->Scale(1./(meanIntegratedV2[j]*perCent) ); 	//  (35a)
        histFull[k].histFullHar[j].mHist_vPt[1]->Scale(-0.5/(meanIntegratedV4[j]*perCent) ); //  (35b)


	}

      for (int asdf=0; asdf<Flow::nCumulantDifferentialOrders;asdf++){
	AddHist(histFull[k].histFullHar[j].mHist_v2D[asdf]);
	AddHist(histFull[k].histFullHar[j].mHist_vEta[asdf]);
	AddHist(histFull[k].histFullHar[j].mHist_vPt[asdf]);
     }

    }



        if (m_M==1){

	  TH1D* hisOfMeanIntegratedV;
	  hisOfMeanIntegratedV = 
            new TH1D(*(histFull[k].mHist_v[0]));
          hisOfMeanIntegratedV->Reset();

	  TH1D* hisOfMeanIntegratedV3;
	  hisOfMeanIntegratedV3 = 
            new TH1D(*(histFull[k].mHist_v[1]));
          hisOfMeanIntegratedV3->Reset();

	  for (int nx = 1; nx <  Flow::nHars+1; nx++){
	    hisOfMeanIntegratedV->SetBinContent(nx, 1./(meanIntegratedV[nx-1]*perCent));
	    hisOfMeanIntegratedV->SetBinError(nx,0.);
	    hisOfMeanIntegratedV3->SetBinContent(nx, -1./(meanIntegratedV3[nx-1]*perCent));
	    hisOfMeanIntegratedV3->SetBinError(nx,0.);
	  }

	  histFull[k].mHist_v[0]->Multiply(hisOfMeanIntegratedV);
          histFull[k].mHist_v[1]->Multiply(hisOfMeanIntegratedV3);

          for (int nx = 1; nx <  Flow::nHars+1; nx++){
          cout << "##### 2-part v" << nx << " = (" 
   << histFull[k].mHist_v[0]->GetBinContent(nx) 
   <<" +/- "<< histFull[k].mHist_v[0]->GetBinError(nx)<<" )"<<endl;
          cout << "##### 4-part v" << nx << " = (" 
   << histFull[k].mHist_v[1]->GetBinContent(nx) 
   <<" +/- "<< histFull[k].mHist_v[1]->GetBinError(nx)<<" )"<<endl;
	  }

	  delete hisOfMeanIntegratedV; delete hisOfMeanIntegratedV3;

	}else if(m_M==2){

	  TH1D* hisOfMeanIntegratedV2;
	  hisOfMeanIntegratedV2 = 
            new TH1D(*(histFull[k].mHist_v[0]));
          hisOfMeanIntegratedV2->Reset();

	  TH1D* hisOfMeanIntegratedV4;
	  hisOfMeanIntegratedV4 = 
            new TH1D(*(histFull[k].mHist_v[1]));
          hisOfMeanIntegratedV4->Reset();

	  for (int nx = 1; nx <  Flow::nHars+1; nx++){
	    hisOfMeanIntegratedV2->SetBinContent(nx, 1./(meanIntegratedV2[nx-1]*perCent));
	    hisOfMeanIntegratedV2->SetBinError(nx,0.);
	    hisOfMeanIntegratedV4->SetBinContent(nx, -0.5/(meanIntegratedV4[nx-1]*perCent));
	    hisOfMeanIntegratedV4->SetBinError(nx,0.);
	  }

        histFull[k].mHist_v[0]->Multiply(hisOfMeanIntegratedV2);
        histFull[k].mHist_v[1]->Multiply(hisOfMeanIntegratedV4);
          for (int nx = 1; nx <  Flow::nHars+1; nx++){
          cout << "##### 2-part v" << nx << " = (" 
   << histFull[k].mHist_v[0]->GetBinContent(nx) 
   <<") +/- "<< histFull[k].mHist_v[0]->GetBinError(nx)<<endl;
          cout << "##### 4-part v" << nx << " = (" 
   << histFull[k].mHist_v[1]->GetBinContent(nx) 
   <<") +/- "<< histFull[k].mHist_v[1]->GetBinError(nx)<<endl;
	  }
          
	  delete hisOfMeanIntegratedV2; delete hisOfMeanIntegratedV4;
	}

      for (int asdf=0; asdf<Flow::nCumulantDifferentialOrders;asdf++)
        AddHist( histFull[k].mHist_v[asdf]);




  }

  //GetHistList()->ls();

  // Write all histograms
  TFile histFile("flow.cumulant.root", "RECREATE");
  TList* hisList = GetHistList(); 
  for (int k = 0; k < Flow::nSels; k++) 
    for (int j =0; j < Flow::nHars; j++)
      for (int ifcn =0; ifcn <  Flow::nCumulantDifferentialOrders*Flow::nCumulantDifferential_qMax; ifcn++){
      hisList->Remove(histFull[k].histFullHar[j].mCumuDifferentialG0Denominator2D[ifcn]);
      hisList->Remove(histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorPt[ifcn]);
      hisList->Remove(histFull[k].histFullHar[j].mCumuDifferentialG0DenominatorEta[ifcn]);
		    }
  hisList->Write();

  //write histograms from standard analysis maker
  StFlowAnalysisMaker* pFlowAnalysisMaker = NULL;
  pFlowAnalysisMaker = (StFlowAnalysisMaker*)GetMaker("FlowAnalysis");

  if (pFlowAnalysisMaker) 
    pFlowAnalysisMaker->GetHistList()->Write();

  histFile.Write();
  histFile.Close();


  //write profile for the denominator of XpqYpq.
  TString*  fileName = new TString("denominatorNew.root");
  TFile XpqYpqDenomNewFile(fileName->Data(),"RECREATE"); 
  XpqYpqDenomNewFile.SetFormat(1);
  XpqYpqDenomNames->Write();
  XpqYpqDenomNewFile.Close();
  delete fileName;
  delete XpqYpqDenomNames;

  // Print the selection object details
  pFlowSelect->PrintList();

  delete pFlowSelect;

  cout << endl;
  gMessMgr->Summary(3);
  cout << endl;

  return StMaker::Finish();
}


////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowCumulantMaker.cxx,v $
// Revision 1.3  2001/11/09 21:14:50  posk
// Switched from CERNLIB to TMath. Using global dca instead of dca.
//
// Revision 1.2  2001/11/08 03:12:24  aihong
// clean up redundant histograms
//
// Revision 1.1  2001/11/02 04:47:42  aihong
// install cumulant maker
//
////////////////////////////////////////////////////////////////////////////
