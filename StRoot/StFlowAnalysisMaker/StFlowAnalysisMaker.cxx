/***************************************************************************
 *
 * $Id: StFlowAnalysisMaker.cxx,v 1.1.1.1 1999/08/09 19:50:37 posk Exp $
 *
 * Author: Raimond Snellings and Art Poskanzer, LBNL, Aug 1999
 * Description:  Maker to analyze the Flow EbyE Tags
 *
 ***************************************************************************
 *
 * $Log: StFlowAnalysisMaker.cxx,v $
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

ClassImp(StFlowAnalysisMaker)

StFlowAnalysisMaker::StFlowAnalysisMaker(const Char_t *name) 
  : StMaker(name)
{
  mFlowTag = 0;
  mEvent = 0;
}

StFlowAnalysisMaker::~StFlowAnalysisMaker() 
{
  //  delete flowTagMaker;
}

Int_t StFlowAnalysisMaker::Make() 
{
  // Get a pointer to the DST
  mEvent = (StEvent *) GetInputDS("StEvent");
  if (!mEvent) return kStOK; // If no event, we're done
  
  //Get a pointer to the flow tags
  StFlowTagMaker *flowTagMaker;
  flowTagMaker = (StFlowTagMaker *) GetMaker("FlowTag");
  mFlowTag = flowTagMaker->tag();
  cout << "$$$$$$$$$$$$ pointer to flow tag: " << mFlowTag << endl;
  if (!mFlowTag) {
    cout << "$$$$$$$$$$$$ empty FlowTag" << endl;
    cout << endl;
    return kStErr;
  }

  PrintInfo();

  // fill histograms from Flow Tags
  makeTagHistograms();

  // fill flow histograms
  makeFlowHistograms();

  return kStOK;
}

void StFlowAnalysisMaker::PrintInfo() 
{
  cout << "$Id: StFlowAnalysisMaker.cxx,v 1.1.1.1 1999/08/09 19:50:37 posk Exp $" << endl;
  if (Debug()) StMaker::PrintInfo();
}

Int_t StFlowAnalysisMaker::Init()
{

  // will go to header file soon 
  const float PhiMin = -pi;
  const float PhiMax = pi; 
  const float EtaMin = -6.;
  const float EtaMax = 6.;
  const float PtMin = 0.;
  const float PtMax = 10.;

  const float PsiMin = 0.;
  const float PsiMax = twopi; 
  const float SumPtMin = 0.;
  const float SumPtMax = 4.;
  const float MultMin = 0.;
  const float MultMax = 10000.;
  const float qMin = 0.;
  const float qMax = 2.;

  for (int i = 0; i < nSubEvents; i++) {
    TString *mHistTitle;
    char mCountSubEvents[nHarmonics];
    sprintf(mCountSubEvents,"%d",i);

    mHistTitle = new TString("ProfHistResolution");
    mHistTitle->Append(*mCountSubEvents);
    histSubEvents[i].mHistResolution =
      new TProfile(mHistTitle->Data(), mHistTitle->Data(), nHarmonics, 1., 
		   (float)(nHarmonics + 1), -10., 10., "");
    histSubEvents[i].mHistResolution->SetXTitle("Harmonic");
    histSubEvents[i].mHistResolution->SetYTitle("Resolution");
    delete mHistTitle;

    for (int j = 0; j < nHarmonics; j++) {
      char mCountHarmonics[5];
      sprintf(mCountHarmonics,"%d",j);

      mHistTitle = new TString("HistPsiSubevent");
      mHistTitle->Append(*mCountSubEvents);
      mHistTitle->Append("Harmonic");
      mHistTitle->Append(*mCountHarmonics + 1);
      histSubEvents[i].histHarmonics[j].mHistPsi =
	new TH1F(mHistTitle->Data(), mHistTitle->Data(), nPsiBins,PsiMin,
		 (PsiMax / (float)(j + 1)));
      histSubEvents[i].histHarmonics[j].mHistPsi->SetXTitle("Psi");
      histSubEvents[i].histHarmonics[j].mHistPsi->SetYTitle("Counts");
      delete mHistTitle;

      mHistTitle = new TString("HistFlowTagMeanPt");
      mHistTitle->Append(*mCountSubEvents);
      mHistTitle->Append("Harmonic");
      mHistTitle->Append(*mCountHarmonics + 1);
      histSubEvents[i].histHarmonics[j].mHistFlowTagSumPt =
	new TH1F(mHistTitle->Data(), mHistTitle->Data(), nSumPtBins,SumPtMin,
		 SumPtMax);
      histSubEvents[i].histHarmonics[j].mHistFlowTagSumPt->
	SetXTitle("Sum Pt / Multiplicity");
      histSubEvents[i].histHarmonics[j].mHistFlowTagSumPt->SetYTitle("Counts");
      delete mHistTitle;

      mHistTitle = new TString("HistFlowTagMult");
      mHistTitle->Append(*mCountSubEvents);
      mHistTitle->Append("Harmonic");
      mHistTitle->Append(*mCountHarmonics + 1);
      histSubEvents[i].histHarmonics[j].mHistFlowTagMult =
	new TH1D(mHistTitle->Data(), mHistTitle->Data(), nMultBins,MultMin,MultMax);
      histSubEvents[i].histHarmonics[j].mHistFlowTagMult->SetXTitle("Multiplicity");
      histSubEvents[i].histHarmonics[j].mHistFlowTagMult->SetYTitle("Counts");
      delete mHistTitle;

      mHistTitle = new TString("Hist_q");
      mHistTitle->Append(*mCountSubEvents);
      mHistTitle->Append("Harmonic");
      mHistTitle->Append(*mCountHarmonics + 1);
      histSubEvents[i].histHarmonics[j].mHist_q =
	new TH1F(mHistTitle->Data(), mHistTitle->Data(), n_qBins,qMin,qMax);
      histSubEvents[i].histHarmonics[j].mHist_q->SetXTitle("q (|Q|/sqrt N)");
      histSubEvents[i].histHarmonics[j].mHist_q->SetYTitle("Counts");
      delete mHistTitle;
    }
  }
  
  return kStOK;
}

Int_t StFlowAnalysisMaker::makeTagHistograms()
{
  // i should be defined inside every loop but compiler does not like that
  int       i;

  // sub-event quantities
  for (int j = 0; j < nHarmonics; j++) {
    QxSub[0][j]  = mFlowTag->qxa[j]; // get the tags
    QxSub[1][j]  = mFlowTag->qxb[j];
    QxSub[2][j]  = mFlowTag->qxc[j];
    QxSub[3][j]  = mFlowTag->qxd[j];
    QySub[0][j]  = mFlowTag->qya[j];
    QySub[1][j]  = mFlowTag->qyb[j];
    QySub[2][j]  = mFlowTag->qyc[j];
    QySub[3][j]  = mFlowTag->qxd[j];
    NSub[0][j]   = mFlowTag->na[j];
    NSub[1][j]   = mFlowTag->nb[j];
    NSub[2][j]   = mFlowTag->nc[j];
    NSub[3][j]   = mFlowTag->nd[j];
    sPtSub[0][j] = mFlowTag->spta[j];
    sPtSub[1][j] = mFlowTag->sptb[j];
    sPtSub[2][j] = mFlowTag->sptc[j];
    sPtSub[3][j] = mFlowTag->sptd[j];
    for (int i = 0; i < nSubEvents; i++) { // calculate
      psiSub[i][j] = atan2(QySub[i][j],QxSub[i][j]) / (float)(j+1);
      if (psiSub[i][j] < 0.) {
	psiSub[i][j] =+ twopi / (float)(j+1);
      }
    }

    // full event quantities
    for (int k = 0; k < nSubEvents/2; k++) {
      Qx[k][j]   = QxSub[2*k][j] + QxSub[2*k+1][j]; // add the sub-event parts
      Qy[k][j]   = QySub[2*k][j] + QySub[2*k+1][j];
      N[k][j]    = NSub[2*k][j] + NSub[2*k+1][j];
      sPt[k][j]  = sPtSub[2*k][j] + sPtSub[2*k+1][j];
      Q[k][j]    = sqrt(Qx[k][j]*Qx[k][j] + Qy[k][j]*Qy[k][j]); // calculate
      q[k][j]    = Q[k][j] / sqrt(N[k][j]);
      psi[k][j]  = atan2(Qy[k][j],Qx[k][j]) / (float)(j+1);
      if (psi[k][j] < 0.) {
	psi[k][j] =+ twopi / (float)(j+1);
      }
      res[k][j]  = sqrt(2.) * sqrt(cos((float)(j+1)*(psiSub[2*k][j] - 
						     psiSub[2*k+1][j])));
    }
  }

  for (i = 0; i < nSubEvents; i++) {
    for (int j = 0; j < nHarmonics; j++) {
      // <Pt>, N, psi
      histSubEvents[i].histHarmonics[j].mHistFlowTagSumPt->
	Fill(sPtSub[i][j] / (float)NSub[i][j]);
      histSubEvents[i].histHarmonics[j].mHistFlowTagMult->Fill(NSub[i][j]);
      histSubEvents[i].histHarmonics[j].mHistPsi->Fill(psiSub[i][j]);
    }
  }
  for (i = 0; i < nSubEvents/2; i++) {
    for (int j = 0; j < nHarmonics; j++) {
      // q, res
      histSubEvents[i].histHarmonics[j].mHist_q->Fill(q[i][j]);
      histSubEvents[i].mHistResolution->Fill((float)(j+1),res[i][j]);    
    }
  }

  return kStOK;
}

void StFlowAnalysisMaker::makeFlowHistograms() 
{
  // will go te header file
  const double    trackquality[3][2] = {{10, 0}, 
					{ 0, 0},
					{ 0, 0}}; 
  const double    bField             = 0.5*tesla;
  
  // Initialize Iterator, loop variables
  StTrackCollection* tracks          = mEvent->trackCollection();
  StTrackIterator    itr;

  // i should be defined inside every loop but compiler does not like that
  int       i;

  // track loop
  long initialMultiplicity = tracks->size();
  float *mPhiAngle = new float[initialMultiplicity];
  float *mPseudoRapidity = new float[initialMultiplicity];
  float *mPt = new float[initialMultiplicity];
  
  long TrackCount = 0;
  for (itr = tracks->begin(), i=0; itr != tracks->end(); itr++) {
    StGlobalTrack* gtrk = *itr;
    StTrackFitTraits& fitTraits = gtrk->fitTraits();
    int nFitPoints = fitTraits.numberOfFitPoints();
    if ((double) nFitPoints > trackquality[0][0]) {
      StThreeVectorD p = gtrk->helix().momentum(bField); 
      mPhiAngle[i] = p.phi();
      mPseudoRapidity[i] = p.pseudoRapidity();
      mPt[i] = p.perp(); 
      TrackCount++;
      i++;
    }
  }

  delete [] mPhiAngle;
  delete [] mPseudoRapidity;
  delete [] mPt;

}


