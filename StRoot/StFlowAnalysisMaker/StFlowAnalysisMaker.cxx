/***************************************************************************
 *
 * $Id: StFlowAnalysisMaker.cxx,v 1.1.1.2 1999/08/24 18:50:16 posk Exp $
 *
 * Author: Raimond Snellings and Art Poskanzer, LBNL, Aug 1999
 * Description:  Maker to analyze Flow using the FlowTags
 *
 ***************************************************************************
 *
 * $Log: StFlowAnalysisMaker.cxx,v $
 * Revision 1.1.1.2  1999/08/24 18:50:16  posk
 * ver 1.3
 *
 * Revision 1.3  1999/08/24 18:02:37  posk
 * Calculates event plane resolution.
 * Added macros for plotting histograms.
 *
 * Revision 1.2  1999/08/13 21:12:00  posk
 * corrections and polishing
 *
 * Revision 1.1.1.1  1999/08/09 19:50:37  posk
 * ver. 1.0
 *
 * Revision 1.0  1999/08/02 
 *
 *  
 **************************************************************************/
#include "StFlowTagMaker/StFlowTagMaker.h"
#include "StFlowAnalysisMaker.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StThreeVector.hh"
#include "StLorentzVector.hh"

ClassImp(StFlowAnalysisMaker) //macro for rootcint

StFlowAnalysisMaker::StFlowAnalysisMaker(const Char_t *name): StMaker(name){
  mFlowTag = 0;
  mEvent   = 0;
}

StFlowAnalysisMaker::~StFlowAnalysisMaker() {
}

Int_t StFlowAnalysisMaker::Make() {
  // Get a pointer to the DST
  mEvent = (StEvent *) GetInputDS("StEvent");
  if (!mEvent) return kStOK; // If no event, we're done

  //get Tags and calculated quantities
  getTags();

  // fill histograms from Flow Tags
  makeTagHistograms();

  // fill flow histograms
  makeFlowHistograms();

  PrintInfo();

  return kStOK;
}

void StFlowAnalysisMaker::PrintInfo() {
  cout << "$Id: StFlowAnalysisMaker.cxx,v 1.1.1.2 1999/08/24 18:50:16 posk Exp $"
       << endl;
  if (Debug()) StMaker::PrintInfo();
}

Int_t StFlowAnalysisMaker::Init() {
  const float PhiMin = -pi;
  const float PhiMax = pi; 
  const float EtaMin = -6.;
  const float EtaMax = 6.;
  const float PtMin  = 0.;
  const float PtMax  = 10.;

  const float PsiMin    = 0.;
  const float PsiMax    = twopi; 
  const float MeanPtMin = 0.;
  const float MeanPtMax = 4.;
  const float MultMin   = 0.;
  const float MultMax   = 10000.;
  const float qMin      = 0.;
  const float qMax      = 2.;

  const int nPhiBins    = 100;
  const int nEtaBins    = 100;
  const int nPtBins     = 100;
  const int nPsiBins    = 100;
  const int nMeanPtBins = 100;
  const int nMultBins   = 100;
  const int n_qBins     = 100;

  TString *mHistTitle;
  for (int i = 0; i < nSubEvents; i++) {
    char mCountSubEvents[2];
    sprintf(mCountSubEvents,"%d",i);

    // for sub-events
    for (int j = 0; j < nHarmonics; j++) {
      char mCountHarmonics[2];
      sprintf(mCountHarmonics,"%d",j);

      // event plane
      mHistTitle = new TString("Flow_Psi_Sub");
      mHistTitle->Append(*mCountSubEvents + 1);
      mHistTitle->Append("_Har");
      mHistTitle->Append(*mCountHarmonics + 1);
      histSubEvents[i].histSubEventHarmonics[j].mHistPsiSub =
	new TH1F(mHistTitle->Data(), mHistTitle->Data(), nPsiBins, PsiMin,
		 (PsiMax / (float)(j + 1)));
      histSubEvents[i].histSubEventHarmonics[j].mHistPsiSub->SetXTitle
	("Event Plane Angle (rad)");
      histSubEvents[i].histSubEventHarmonics[j].mHistPsiSub->SetYTitle("Counts");
      delete mHistTitle;
    }
  }

  // for sub-event pairs
  for (int k = 0; k < nSubEvents/2; k++) {
    char mCountSubEvents[2];
    sprintf(mCountSubEvents,"%d",k);

    // cos(n*delta_Psi)
    mHistTitle = new TString("Flow_prof_Cos_Pair");
    mHistTitle->Append(*mCountSubEvents + 1);
    histFullEvents[k].mHistCos =
      new TProfile(mHistTitle->Data(), mHistTitle->Data(), nHarmonics, 0.5, 
		   (float)(nHarmonics) + 0.5, -10., 10., "");
    histFullEvents[k].mHistCos->SetXTitle("Harmonic");
    histFullEvents[k].mHistCos->SetYTitle("cos(n*delta_Psi)");
    delete mHistTitle;
    
    // resolution
    mHistTitle = new TString("Flow_Res_Pair");
    mHistTitle->Append(*mCountSubEvents + 1);
    histFullEvents[k].mHistRes =
      new TH1F(mHistTitle->Data(), mHistTitle->Data(), nHarmonics, 0.5, 
		   (float)(nHarmonics) + 0.5);
    histFullEvents[k].mHistRes->SetXTitle("Harmonic");
    histFullEvents[k].mHistRes->SetYTitle("Resolution");
    delete mHistTitle;
    
    // for full events
    for (int j = 0; j < nHarmonics; j++) {
      char mCountHarmonics[2];
      sprintf(mCountHarmonics,"%d",j);

      // multiplicity
      mHistTitle = new TString("Flow_Mult_Pair");
      mHistTitle->Append(*mCountSubEvents + 1);
      mHistTitle->Append("_Har");
      mHistTitle->Append(*mCountHarmonics + 1);
      histFullEvents[k].histFullEventHarmonics[j].mHistMult =
	new TH1D(mHistTitle->Data(), mHistTitle->Data(), nMultBins, MultMin, 
		 MultMax);
      histFullEvents[k].histFullEventHarmonics[j].mHistMult->SetXTitle
	("Multiplicity");
      histFullEvents[k].histFullEventHarmonics[j].mHistMult->
	SetYTitle("Counts");
      delete mHistTitle;
      
      // event plane
      mHistTitle = new TString("Flow_Psi_Pair");
      mHistTitle->Append(*mCountSubEvents + 1);
      mHistTitle->Append("_Har");
      mHistTitle->Append(*mCountHarmonics + 1);
      histFullEvents[k].histFullEventHarmonics[j].mHistPsi =
	new TH1F(mHistTitle->Data(), mHistTitle->Data(), nPsiBins, PsiMin,
		 (PsiMax / (float)(j + 1)));
      histFullEvents[k].histFullEventHarmonics[j].mHistPsi->SetXTitle
	("Event Plane Angle (rad)");
      histFullEvents[k].histFullEventHarmonics[j].mHistPsi->SetYTitle("Counts");
      delete mHistTitle;
      
      // q
      mHistTitle = new TString("Flow_q_Pair");
      mHistTitle->Append(*mCountSubEvents + 1);
      mHistTitle->Append("_Har");
      mHistTitle->Append(*mCountHarmonics + 1);
      histFullEvents[k].histFullEventHarmonics[j].mHist_q =
	new TH1F(mHistTitle->Data(), mHistTitle->Data(), n_qBins, qMin, qMax);
      histFullEvents[k].histFullEventHarmonics[j].mHist_q->
	SetXTitle("q = |Q|/sqrt N");
      histFullEvents[k].histFullEventHarmonics[j].mHist_q->SetYTitle("Counts");
      delete mHistTitle;
      
      // <p_t>
      mHistTitle = new TString("Flow_MeanPt_Pair");
      mHistTitle->Append(*mCountSubEvents + 1);
      mHistTitle->Append("_Har");
      mHistTitle->Append(*mCountHarmonics + 1);
      histFullEvents[k].histFullEventHarmonics[j].mHistMeanPt =
	new TH1F(mHistTitle->Data(), mHistTitle->Data(), nMeanPtBins, MeanPtMin,
		 MeanPtMax);
      histFullEvents[k].histFullEventHarmonics[j].mHistMeanPt->
	SetXTitle("Mean Pt (GeV)");
      histFullEvents[k].histFullEventHarmonics[j].mHistMeanPt->
	SetYTitle("Counts");
      delete mHistTitle;
    }
  }
  return kStOK;
}

Int_t StFlowAnalysisMaker::getTags() {  
  //Get a pointer to the flow tags
  StFlowTagMaker *flowTagMaker;
  flowTagMaker = (StFlowTagMaker *) GetMaker("FlowTag");
  mFlowTag = flowTagMaker->tag();
  if (!mFlowTag) {
    cout << "$$$$$ empty FlowTag" << endl;
    return kStErr;
  }

  for (int j = 0; j < nHarmonics; j++) {
    float order  = (float)(j+1);
    // sub-event quantities
    QxSub[0][j]    = mFlowTag->qxa[j];
    QxSub[1][j]    = mFlowTag->qxb[j];
    QxSub[2][j]    = mFlowTag->qxc[j];
    QxSub[3][j]    = mFlowTag->qxd[j];
    QySub[0][j]    = mFlowTag->qya[j];
    QySub[1][j]    = mFlowTag->qyb[j];
    QySub[2][j]    = mFlowTag->qyc[j];
    QySub[3][j]    = mFlowTag->qyd[j];
    NSub[0][j]     = mFlowTag->na[j];
    NSub[1][j]     = mFlowTag->nb[j];
    NSub[2][j]     = mFlowTag->nc[j];
    NSub[3][j]     = mFlowTag->nd[j];
    sumPtSub[0][j] = mFlowTag->spta[j];
    sumPtSub[1][j] = mFlowTag->sptb[j];
    sumPtSub[2][j] = mFlowTag->sptc[j];
    sumPtSub[3][j] = mFlowTag->sptd[j];

    // calculate Psi
    for (int i = 0; i < nSubEvents; i++) {
      PsiSub[i][j] = atan2(QySub[i][j],QxSub[i][j]) / order;
      if (PsiSub[i][j] < 0.) {
	PsiSub[i][j] += twopi / order;
      }
    }

    // full event quantities
    for (int k = 0; k < nSubEvents/2; k++) {
      // add the sub-events
      Qx[k][j]   = QxSub[2*k][j] + QxSub[2*k+1][j];
      Qy[k][j]   = QySub[2*k][j] + QySub[2*k+1][j];
      N[k][j]    = NSub[2*k][j]  + NSub[2*k+1][j];
      sumPt[k][j]= sumPtSub[2*k][j] + sumPtSub[2*k+1][j];

      // calculate
      Q[k][j]    = sqrt(Qx[k][j]*Qx[k][j] + Qy[k][j]*Qy[k][j]);
      if (N[k][j] > 0) {
	q[k][j]  = Q[k][j] / sqrt(N[k][j]);
      }
      else {
	q[k][j]  = 0.;
      }
      Psi[k][j]  = atan2(Qy[k][j],Qx[k][j]) / order;
      if (Psi[k][j] < 0.) {
	Psi[k][j] += twopi / order;
      }
      CosDiffSubs[k][j]  = cos(order*(PsiSub[2*k][j] - PsiSub[2*k+1][j]));
    }
  }
}

void StFlowAnalysisMaker::makeTagHistograms() {
  // sub-event Psi_Sub
  for (int i = 0; i < nSubEvents; i++) {
    for (int j = 0; j < nHarmonics; j++) {
      histSubEvents[i].histSubEventHarmonics[j].mHistPsiSub->Fill(PsiSub[i][j]);
    }
  }

  // full event Psi, N, q, cos, <Pt>
  for (int k = 0; k < nSubEvents/2; k++) {
    for (int j = 0; j < nHarmonics; j++) {
      histFullEvents[k].histFullEventHarmonics[j].mHistPsi->Fill(Psi[k][j]);
      histFullEvents[k].histFullEventHarmonics[j].mHistMult->
	Fill(N[k][j]);
      histFullEvents[k].histFullEventHarmonics[j].mHist_q->Fill(q[k][j]);
      histFullEvents[k].mHistCos->Fill((float)(j+1),CosDiffSubs[k][j]);    
      if (N[k][j] > 0) {
	histFullEvents[k].histFullEventHarmonics[j].mHistMeanPt->
	  Fill(sumPt[k][j] / (float)N[k][j]);
      }
    }
  }
}

void StFlowAnalysisMaker::makeFlowHistograms() {
  // will go to header file
  const double    trackquality[3][2] = {{10, 0}, 
					{ 0, 0},
					{ 0, 0}}; 
  const double    bField             = 0.5*tesla;
  
  // Initialize Iterator, loop variables, and arrays
  StTrackCollection* tracks          = mEvent->trackCollection();
  StTrackIterator    itr;
  long initialMultiplicity = tracks->size();
  float *mPhiAngle = new float[initialMultiplicity];
  float *mPseudoRapidity = new float[initialMultiplicity];
  float *mPt = new float[initialMultiplicity];
  
  // track loop
  long TrackCount = 0;
  int i;
  for (itr = tracks->begin(), i=0; itr != tracks->end(); itr++) {
    StGlobalTrack* gtrk = *itr;
    StTrackFitTraits& fitTraits = gtrk->fitTraits();
    int nFitPoints = fitTraits.numberOfFitPoints();
    int nMaxPoints = fitTraits.numberOfPossiblePoints();
    if ((double) nFitPoints > trackquality[0][0]) {
      StThreeVectorD p = gtrk->helix().momentum(bField); 
      mPhiAngle[i] = p.phi();
      mPseudoRapidity[i] = p.pseudoRapidity();
      mPt[i] = p.perp();

      // Remove autocorrelations and fill flow histograms

      // Finish loop
      TrackCount++;
      i++;
    }
  }
  delete [] mPhiAngle;
  delete [] mPseudoRapidity;
  delete [] mPt;
}

Int_t StFlowAnalysisMaker::Finish() {
  // Calculate resolution = sqrt(2)*sqrt(mHistCos)
  float cosPair[nSubEvents/2][nHarmonics];
  float cosPairErr[nSubEvents/2][nHarmonics];
  for (int k = 0; k < nSubEvents/2; k++) {
    for (int j = 0; j < nHarmonics; j++) {
      cosPair[k][j] = histFullEvents[k].mHistCos->GetBinContent(j+1);
      cosPairErr[k][j] = histFullEvents[k].mHistCos->GetBinError(j+1);
      if (cosPair[k][j] > 0.) {
	res[k][j] = sqrt(2*cosPair[k][j]);
	resErr[k][j] = cosPairErr[k][j] / res[k][j];
      }
      else {
	res[k][j] = 0.;
	resErr[k][j] = 0.;
      }
      histFullEvents[k].mHistRes->SetBinContent(j+1, res[k][j]);
      histFullEvents[k].mHistRes->SetBinError(j+1, resErr[k][j]);
    }
  }
  return kStOK;
}


