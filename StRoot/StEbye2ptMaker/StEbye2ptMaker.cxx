 /***************************************************************************
 *
 * $Id: StEbye2ptMaker.cxx,v 1.4 2000/08/14 22:05:19 jseger Exp $
 *
 * StEbye2ptMaker.cxx
 *
 * Author: Jeff Reid, UW
 *         with design advice from Thomas Ullrich, Yale
 *
 ***************************************************************************
 *
 * Description:  This is a maker to generate 2-point correlation
 *                spaces (App) from p_t spectra
 *
 ***************************************************************************
 *
 * $Log: StEbye2ptMaker.cxx,v $
 * Revision 1.4  2000/08/14 22:05:19  jseger
 * Added eta-spectra.  Now reads Ebye mini-DST as input.  Bins events in
 * multiplicity and z-vertex position.  Name of output file is no longer hard-wired.
 *
 * Revision 1.2  2000/02/22 00:04:14  jgreid
 * changed from global to primary tracks, added more track quality cuts
 *
 * Revision 1.1.1.1  2000/02/05 03:15:21  jgreid
 * Two particle correlationspace generation package
 *
 *
 **************************************************************************/
#include <iostream.h>
#include <stdlib.h>
#include <math.h>
#include "StEbye2ptMaker.h"
#include "StChain.h"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "StParticleTypes.hh"
#include "TTree.h"
#include "TBranch.h"
#include "PhysicalConstants.h"
#include "StThreeVector.hh"
#include "StPionPlus.hh"
#include "SystemOfUnits.h"
#include "StEbyeDSTMaker.h"
#include "StEbyeEvent.h"
#include "StEbyeTrack.h"

#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

static const char rcsid[] = "$Id: StEbye2ptMaker.cxx,v 1.4 2000/08/14 22:05:19 jseger Exp $";

ClassImp(StEbye2ptMaker)

StEbye2ptMaker::StEbye2ptMaker(const Char_t *name) : StMaker(name), 
  mEbyeDSTRead(kFALSE) {
  SetEbyeDSTFileName("ebyeevent.root");
  }

StEbye2ptMaker::~StEbye2ptMaker() { /* noop */ }

Int_t StEbye2ptMaker::Init()
{
  Int_t kRETURN = kStOK;

  Int_t aMax = 5000;
  Int_t HistBinNumber = 25;

  // allocate the necessary histograms
  mMt = new TH1F("Mt","Transverse Mass",HistBinNumber,0,5);
  mX = new TH1F("X","X",HistBinNumber,0,1);
  mEta = new TH1F("Eta","Pseudorapidity",HistBinNumber,-3,3);
  mEtaX = new TH1F("EtaX","Transformed Pseudorapidity",HistBinNumber,-1,1);
  
  mSibPPEta = new TH2F("SibppEta","Pseudorapidity,Sibling:+.+",HistBinNumber,-1,1,HistBinNumber,-1,1);
  mSibPMEta = new TH2F("SibpmEta","Pseudorapidity,Sibling:+.-",HistBinNumber,-1,1,HistBinNumber,-1,1);
  mSibMPEta = new TH2F("SibmpEta","Pseudorapidity,Sibling:-.+",HistBinNumber,-1,1,HistBinNumber,-1,1);
  mSibMMEta = new TH2F("SibmmEta","Pseudorapidity,Sibling:-.-",HistBinNumber,-1,1,HistBinNumber,-1,1);

  mSibPP = new TH2F("SPP","Sibling :+.+",HistBinNumber,0,1,HistBinNumber,0,1);
  mSibPM = new TH2F("SPM","Sibling :+.-",HistBinNumber,0,1,HistBinNumber,0,1);
  mSibMP = new TH2F("SMP","Sibling :-.+",HistBinNumber,0,1,HistBinNumber,0,1);
  mSibMM = new TH2F("SMM","Sibling :-.-",HistBinNumber,0,1,HistBinNumber,0,1);

  mMixPPEta = new TH2F("MixppEta","Pseudorapidity,Mixed:+.+",HistBinNumber,-1,1,HistBinNumber,-1,1);
  mMixPMEta = new TH2F("MixpmEta","Pseudorapidity,Mixed:+.-",HistBinNumber,-1,1,HistBinNumber,-1,1);
  mMixMPEta = new TH2F("MixmpEta","Pseudorapidity,Mixed:-.+",HistBinNumber,-1,1,HistBinNumber,-1,1);
  mMixMMEta = new TH2F("MixmmEta","Pseudorapidity,Mixed:-.-",HistBinNumber,-1,1,HistBinNumber,-1,1);

  mMixPP = new TH2F("MPP","Mixed :+.+",HistBinNumber,0,1,HistBinNumber,0,1);
  mMixPM = new TH2F("MPM","Mixed :+.-",HistBinNumber,0,1,HistBinNumber,0,1);
  mMixMP = new TH2F("MMP","Mixed :-.+",HistBinNumber,0,1,HistBinNumber,0,1);
  mMixMM = new TH2F("MMM","Mixed :-.-",HistBinNumber,0,1,HistBinNumber,0,1);

  // define the binnumbers for mixing
  Int_t mthisEventBinNumber=0;
  Int_t mpreviousEventBinNumber=0;

  // allocate the particle arrays for mixing
  mPreviousEventPlus = new Double_t[aMax];
  assert (mPreviousEventPlus != 0);
  mPreviousEventMinus = new Double_t[aMax]; 
  assert (mPreviousEventMinus != 0);
  mEtaPreviousEventPlus = new Double_t[aMax];
  assert (mEtaPreviousEventPlus != 0);
  mEtaPreviousEventMinus = new Double_t[aMax]; 
  assert (mEtaPreviousEventMinus != 0);
  
  mThisEventPlus = new Double_t[aMax];
  assert (mThisEventPlus != 0);
  mThisEventMinus = new Double_t[aMax];
  assert (mThisEventMinus != 0);
  mEtaThisEventPlus = new Double_t[aMax];
  assert (mEtaThisEventPlus != 0);
  mEtaThisEventMinus = new Double_t[aMax];
  assert (mEtaThisEventMinus != 0);

  // set the first element of the event arrays to zero
  //  (we use Event[0] to keep track of the number of valid 
  //   elements in the array, and they start out empty)
  mPreviousEventPlus[0] = 0;
  mPreviousEventMinus[0] = 0;
  mEtaPreviousEventPlus[0] = 0;
  mEtaPreviousEventMinus[0] = 0;

  mThisEventPlus[0] = 0;
  mThisEventMinus[0] = 0;
  mEtaThisEventPlus[0] = 0;
  mEtaThisEventMinus[0] = 0;
    
  //Set up to read Ebye mini-DST
  if (mEbyeDSTRead) kRETURN += InitEbyeDSTRead();

  //Bin the events by multiplicity and z-vertex position
  SortEvents();

  return kRETURN;
}

void
StEbye2ptMaker::Clear(Option_t *opt)
{
  StMaker::Clear();
}

Int_t
StEbye2ptMaker::Finish()
{
  // create the histogram output file
  //  (file name is currently hardwired, fix this)
//  TFile histogramFile("ebye2pt.root","recreate");
  Char_t* outfile = mEbye2ptFileName;
//  histogramFile = new TFile(outfile);
   TFile histogramFile(outfile,"recreate");

  // write out histograms to file

  mMt->Write();
  mX->Write();
  mEta->Write();
  mEtaX->Write();

  mSibPPEta->Write();
  mSibPMEta->Write();
  mSibMPEta->Write();
  mSibMMEta->Write();

  mSibPP->Write();
  mSibPM->Write();
  mSibMP->Write();
  mSibMM->Write();

  mMixPPEta->Write();
  mMixPMEta->Write();
  mMixMPEta->Write();
  mMixMMEta->Write();

  mMixPP->Write();
  mMixPM->Write();
  mMixMP->Write();
  mMixMM->Write();

  histogramFile.Close();

  // give back the allocated memory for the histograms
  //   and particle arrays
  delete mMt;
  delete mX;
  delete mEta;
  delete mEtaX;

  delete mSibPPEta;
  delete mSibPMEta;
  delete mSibMPEta;
  delete mSibMMEta;

  delete mSibPP; 
  delete mSibPM;
  delete mSibMP;
  delete mSibMM;

  delete mMixPPEta;
  delete mMixPMEta;
  delete mMixMPEta;
  delete mMixMMEta;

  delete mMixPP;
  delete mMixPM;
  delete mMixMP;
  delete mMixMM;

  delete [] mPreviousEventPlus;
  delete [] mPreviousEventMinus;
  delete [] mEtaPreviousEventPlus;
  delete [] mEtaPreviousEventMinus;

  delete [] mThisEventPlus;
  delete [] mThisEventMinus;
  delete [] mEtaThisEventPlus;
  delete [] mEtaThisEventMinus;

  if (mEbyeDSTRead && pEbyeDST->IsOpen() ) {pEbyeDST->Close(); }

  return kStOK;
}

Int_t
StEbye2ptMaker::Make()
{
    Double_t *tempEventPlus,*tempEventMinus;
    Double_t *EtatempEventPlus,*EtatempEventMinus;

    StEbyeEvent& ev = *mEbyeEvent;

    Int_t entryNum = mIndex[mEventCounter];
    mthisEventBinNumber = mSortArray[entryNum];
    mEventCounter++;
  
    if (!mEbyeEvent || !pEbyeTree->GetEntry(entryNum)) return kStOK;
  
    pEbyeTree->GetEntry(entryNum);

    // processEvent() applies the event and track cuts to the current event
    //  and fills thisEvent with the p_t values which pass these cuts.
    if((processEvent(ev)) == kStOk) {
      // if there is a non-empty event previous to this one then mix them
      if ((mPreviousEventPlus[0] != 0) &&(mthisEventBinNumber==mpreviousEventBinNumber)){	
	mixEvents();
      }
      // set the previous event to be the current event (and the current to be
      //  the previous so we don't have to reallocate memory)

      mpreviousEventBinNumber = mthisEventBinNumber;

      tempEventPlus = mPreviousEventPlus;
      tempEventMinus = mPreviousEventMinus;
      EtatempEventPlus = mEtaPreviousEventPlus;
      EtatempEventMinus = mEtaPreviousEventMinus;

      mPreviousEventPlus = mThisEventPlus;
      mPreviousEventMinus = mThisEventMinus;
      mEtaPreviousEventPlus = mEtaThisEventPlus;
      mEtaPreviousEventMinus = mEtaThisEventMinus;

      mThisEventPlus = tempEventPlus;
      mThisEventMinus = tempEventMinus;
      mEtaThisEventPlus = EtatempEventPlus;
      mEtaThisEventMinus = EtatempEventMinus;

      mThisEventPlus[0] = 0;
      mThisEventMinus[0] = 0;
      mEtaThisEventPlus[0] = 0;
      mEtaThisEventMinus[0] = 0;

      gMessMgr->Info() << " StEbye2ptMaker::Make() completed OK " << endm;
      return kStOK;

    } else {
       
      gMessMgr->Info() << " StEbye2ptMaker::Make() FAILED" << endm;
      return kStErr;

    } 
}

Int_t
StEbye2ptMaker::processEvent(StEbyeEvent& event) 
{
  // hardwire cut values temporarily
  float pt_min = 0;
  float pt_max = 20;
   
  float dcaX_min = -0.1/centimeter;
  float dcaX_max = 0.1/centimeter;

  float dcaY_min = dcaX_min;
  float dcaY_max = dcaX_max;

  // define variables
  float px,py,pt, mtOnly;
  Int_t charge;
  float eta;

  float nFound, nMax;

  float trackCount = 0.0;
  float meanPt = 0.0;
  float meanPtSquared = 0.0;
  float meanEta = 0.0;
  float meanEtaSquared = 0.0;

  double VertexX, VertexY, dcaX, dcaY, dcaM;

  int minusCount = 0;
  int plusCount = 0;
  
  float minusPt = 0;
  float minusPt2 = 0;
  float PlusPt = 0;
  float PlusPt2 = 0;
	    
  double PionMass = StPionPlus::instance()->mass();
  float Temperature = 0.25;
  float width = 1.4;

  float Minimum = (1+(PionMass/Temperature))*exp(-PionMass/Temperature);

  // ** track loop **
  for (Int_t nt=0; nt<mEbyeEvent->Ntrack(); nt++) {
    StEbyeTrack* ntrack = (StEbyeTrack*)mEbyeEvent->Tracks()->UncheckedAt(nt);
      // Instantiate new StEbyeTrack
      StEbyeTrack* pEbyeTrack = new StEbyeTrack;
      if (!pEbyeTrack) return kFALSE;

        // cut out tracks marked as bad [cut #1]
	//if (ntrack->Flag() > 0) {

	 // cut out tracks which have large x or y for primary vertex [cut #1.5]
         VertexX = mEbyeEvent->Vx();
         VertexY = mEbyeEvent->Vy();
	 //if ((VertexX < 0.5) && (VertexY < 0.5)){
          // cut out tracks with bad goodness of fit [cut #2]
          //if (ntrack->Chi2() < 3) {

            // get the momentum of the current track	    
            px = ntrack->Px();
            py = ntrack->Py();
	    pt = sqrt(px*px +py*py);

            // get the charge of the current track
             charge = ntrack->Charge();
            // get Nfound & Nmax
            nFound = ntrack->NFitPoints();
            nMax = ntrack->NMaxPoints();

            // ** nFound/nMax cut [cut #3] 
            //if ( (nFound/nMax) > 0.5 ) {

              // calculate distance of closest approach to the primary vertex position
              dcaX = ntrack->Bx();
              dcaY = ntrack->By();
  	      dcaM = ntrack->Dca();
           
              // ** transverse DCA cut [cut #4] 
              //if (((dcaX > dcaX_min) && (dcaX < dcaX_max)) && ((dcaY >dcaY_min) && (dcaY < dcaY_max))) {

                // calculate mt (needed for temperature calculation)
                mtOnly = sqrt(pt*pt + PionMass*PionMass);

                // fill eta
		eta = ntrack->Eta();
		mEta->Fill(eta);

                // ** cut out extreme pt values [cut #5]
                if ((pt > pt_min) && (pt < pt_max)) {
		  mMt->Fill(mtOnly);

                  if (charge < 0) {
                    // precrement so that the 0th element of the pt arrays can be used
                    //  for the number of quality tracks in the event
                    minusCount++;
                    mThisEventMinus[minusCount]=1-(1+(mtOnly/Temperature))*exp(-mtOnly/Temperature)/Minimum;
		    mX->Fill(mThisEventMinus[minusCount]);
		    mEtaThisEventMinus[minusCount]=erf(eta/(sqrt(2)*width));
		    mEtaX->Fill(mEtaThisEventMinus[minusCount]);
                    minusPt += pt;
                    minusPt2 += pt*pt;
                  }
                  else if (charge > 0) {
                    plusCount++;
                    mThisEventPlus[plusCount]=1-(1+(mtOnly/Temperature))*exp(-mtOnly/Temperature)/Minimum;
		    mX->Fill(mThisEventPlus[plusCount]);
		    mEtaThisEventPlus[plusCount]=erf(eta/(sqrt(2)*width));
		    mEtaX->Fill(mEtaThisEventPlus[plusCount]);
                    PlusPt += pt;
                    PlusPt2 += pt*pt;
                  }

                  // calculate number of particles that make the cuts, and the first two pt moments
                  trackCount++;
                  meanPtSquared += pt*pt;
                  meanPt += pt;

                  meanEtaSquared += eta*eta;
                  meanEta += eta;

	        }// [cut #5]
	      //}// [cut #4]
            //} // [cut #3]
          //} // [cut #2]
	 //} // [cut #1.5]
        //} // [cut #1]

  } // end track loop

  mThisEventMinus[0] = minusCount;
  mThisEventPlus[0] = plusCount;
  mEtaThisEventMinus[0] = minusCount;
  mEtaThisEventPlus[0] = plusCount;

 // mPt->Fill(minusPt+PlusPt);

  //uncomment this line to see how many tracks pass the cuts
  //cerr << minusCount << "  -|+ " << plusCount << endl;

  minusPt2 /= minusCount;
  minusPt /= minusCount;
  PlusPt2 /= plusCount;
  PlusPt /= plusCount;

  return kStOK;
}

Int_t
StEbye2ptMaker::mixEvents()
{
    // fill the manyHistos that we need

    int l, j;
    //float k, qp, qm;

    for (j = 1 ; j <= mThisEventMinus[0] ; j++) {
      for (l = 1 ; l <= mPreviousEventPlus[0] ; l++) {
        //k = (mPreviousEventPlus[l]+mThisEventMinus[j])/2;
        //qp = (mPreviousEventPlus[l]-mThisEventMinus[j])/2;
        //qm = -qp;
        mMixPM->Fill(mPreviousEventPlus[l],mThisEventMinus[j],1);
        mMixMP->Fill(mThisEventMinus[j],mPreviousEventPlus[l],1);
        mMixPMEta->Fill(mEtaPreviousEventPlus[l],mEtaThisEventMinus[j],1);
        mMixMPEta->Fill(mEtaThisEventMinus[j],mEtaPreviousEventPlus[l],1);
        //MixedHkq0->Fill(k,qp,1);
        //MixedHkq0->Fill(k,qm,1);
      }
      for (l = 1 ; l <= mPreviousEventMinus[0] ; l++) {
        //k = (mPreviousEventMinus[l]+mThisEventMinus[j])/2;
        //qp = (mPreviousEventMinus[l]-mThisEventMinus[j])/2;
        //qm = -qp;
        mMixMM->Fill(mPreviousEventMinus[l],mThisEventMinus[j],1);
        mMixMM->Fill(mThisEventMinus[j],mPreviousEventMinus[l],1);
        mMixMMEta->Fill(mEtaPreviousEventMinus[l],mEtaThisEventMinus[j],1);
        mMixMMEta->Fill(mEtaThisEventMinus[j],mEtaPreviousEventMinus[l],1);
        //MixedHkqM->Fill(k,qp,1);
        //MixedHkqM->Fill(k,qm,1);
      }
      for (l = j+1 ; l <= mThisEventMinus[0] ; l++) {
        //k = (mThisEventMinus[l]+mThisEventMinus[j])/2;
        //qp = (mThisEventMinus[l]-mThisEventMinus[j])/2;
        //qm = -qp;
        mSibMM->Fill(mThisEventMinus[l],mThisEventMinus[j],1);
        mSibMM->Fill(mThisEventMinus[j],mThisEventMinus[l],1);
        mSibMMEta->Fill(mEtaThisEventMinus[l],mEtaThisEventMinus[j],1);
        mSibMMEta->Fill(mEtaThisEventMinus[j],mEtaThisEventMinus[l],1);
        //SiblingHkqM->Fill(k,qp,1);
        //SiblingHkqM->Fill(k,qm,1);
      }
    }
    for (j = 1 ; j <= mThisEventPlus[0] ; j++) {
      for (l = 1 ; l <= mThisEventMinus[0] ; l++) {
        //k = (mThisEventMinus[l]+mThisEventPlus[j])/2;
        //qp = (mThisEventMinus[l]-mThisEventPlus[j])/2;
        //qm = -qp;
        mSibMP->Fill(mThisEventMinus[l],mThisEventPlus[j],1);
        mSibPM->Fill(mThisEventPlus[j],mThisEventMinus[l],1);
        mSibMPEta->Fill(mEtaThisEventMinus[l],mEtaThisEventPlus[j],1);
        mSibPMEta->Fill(mEtaThisEventPlus[j],mEtaThisEventMinus[l],1);
        //SiblingHkq0->Fill(k,qp,1);
        //SiblingHkq0->Fill(k,qm,1);
      }
      for (l = 1 ; l <= mPreviousEventPlus[0] ; l++) {
        //k = (mPreviousEventPlus[l]+mThisEventPlus[j])/2;
        //qp = (mPreviousEventPlus[l]-mThisEventPlus[j])/2;
        //qm = -qp;
        mMixPP->Fill(mPreviousEventPlus[l],mThisEventPlus[j],1);
        mMixPP->Fill(mThisEventPlus[j],mPreviousEventPlus[l],1);
        mMixPPEta->Fill(mEtaPreviousEventPlus[l],mEtaThisEventPlus[j],1);
        mMixPPEta->Fill(mEtaThisEventPlus[j],mEtaPreviousEventPlus[l],1);
        //MixedHkqP->Fill(k,qp,1);
        //MixedHkqP->Fill(k,qm,1);
      }
      for (l = j+1 ; l <= mThisEventPlus[0] ; l++) {
        //k = (mThisEventPlus[l]+mThisEventPlus[j])/2;
        //qp = (mThisEventPlus[l]-mThisEventPlus[j])/2;
        //qm = -qp;
        mSibPP->Fill(mThisEventPlus[l],mThisEventPlus[j],1);
        mSibPP->Fill(mThisEventPlus[j],mThisEventPlus[l],1);
        mSibPPEta->Fill(mEtaThisEventPlus[l],mEtaThisEventPlus[j],1);
        mSibPPEta->Fill(mEtaThisEventPlus[j],mEtaThisEventPlus[l],1);
        //SiblingHkqP->Fill(k,qp,1);
        //SiblingHkqP->Fill(k,qm,1);
      }
      for (l = 1 ; l <= mPreviousEventMinus[0] ; l++) {
        mMixMP->Fill(mPreviousEventMinus[l],mThisEventPlus[j],1);
        mMixPM->Fill(mThisEventPlus[j],mPreviousEventMinus[l],1);
        mMixMPEta->Fill(mEtaPreviousEventMinus[l],mEtaThisEventPlus[j],1);
        mMixPMEta->Fill(mEtaThisEventPlus[j],mEtaPreviousEventMinus[l],1);
      }
    }

    return kStOK;
}

//-----------------------------------------------------------------------
Int_t StEbye2ptMaker::InitEbyeDSTRead() {

  EbyeDSTBranch = new StEbyeEvent();
  mEbyeEvent = new StEbyeEvent();

  // Open the file
  Char_t* file = mEbyeDSTFileName;
  pEbyeDST = new TFile(file);
  if (!pEbyeDST->IsOpen()) {
    //gMessMgr->Info << "##### Ebye2ptMaker: No EbyeDST file = " << file << endm;
    return kStFatal;
  }

  gMessMgr->Info() << "##### Ebye2ptMaker: EbyeDST file = " << file << endm;

  // Get the tree, the branch, and the entries
  pEbyeTree = (TTree*)pEbyeDST->Get("EbyeTree");
  if (!pEbyeTree) {
    gMessMgr->Info() << "##### Ebye2ptMaker: Error: No Ebye DST" << endm;
    return kStFatal;
  }

  TBranch* branch = pEbyeTree->GetBranch("EbyeDSTBranch");
  branch->SetAddress(&mEbyeEvent);
  Int_t nEntries = (Int_t)pEbyeTree->GetEntries();
  gMessMgr->Info() << "##### Ebye2ptMaker: events in Ebye DST file = " << nEntries << endm;

  mEventCounter = 0;

  return kStOK;
  }
//-----------------------------------------------------------------------
void StEbye2ptMaker::SortEvents() {
  Int_t Nentries = pEbyeTree->GetEntries();

  // set binning constants
  Int_t NMultBins = 20;  //  set number of bins for multiplicity
  Int_t NZBins = 20;  //set number of bins for z vertex position
  Int_t MultMax = 1600;  // set the maximum multiplicity for the dataset
  Int_t ZMax = 450;  // set the maximum value for z vertex position

  // place events into bins
  gMessMgr->Info() << " StEbye2ptMaker::SortEvents() Binning events... " << endm;
  Int_t i=0;
  for (i=0;i<Nentries;i++) {
    pEbyeTree->GetEntry(i);
    Int_t mult = mEbyeEvent->OrigMult();
    Int_t zvertex = mEbyeEvent->Vz();
    Int_t multbin = 1+(mult*NMultBins/MultMax);
    Int_t zbin = 1+((zvertex+ZMax)*NZBins/(2*ZMax));
    Int_t BinNumber = (multbin-1)*NZBins+zbin;
   if(zbin > NZBins || zbin < 0 || multbin >NMultBins || multbin <0){
   gMessMgr->Info() << " StEbye2ptMaker::SortEvents() Bin Number Outside Allowed Range " << endm;
   }
    mSortArray[i] = BinNumber;
  }

  // sort events by bin number so they can be mixed with other events from
  // the same bin
  gMessMgr->Info() << " StEbye2ptMaker::SortEvents() Sorting events... " << endm;
  TMath::Sort(Nentries,mSortArray,mIndex);
   for (i=0;i<20;i++) {
      printf("i=%d, index=%d,param=%d\n",i,mIndex[i],mSortArray[mIndex[i]]);
   }
}
//-----------------------------------------------------------------------


