//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowTagMaker.cxx,v 1.10 2000/01/13 23:22:05 snelling Exp $
//
// Authors: Raimond Snellings and Art Poskanzer, LBNL, Jun 1999
//
//////////////////////////////////////////////////////////////////////
//
// Description:  Maker to fill the Flow EbyE Tag database
//
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowTagMaker.cxx,v $
// Revision 1.10  2000/01/13 23:22:05  snelling
// modified sum pt (spt) to mean pt (mpt)
//
// Revision 1.9  2000/01/13 21:49:14  posk
// Updates and corrections.
//
// Revision 1.8  1999/12/21 21:28:33  posk
// Updated the README file.
//
// Revision 1.7  1999/12/15 21:56:21  posk
// Increased number of harmonics from 4 to 6.
//
// Revision 1.6  1999/12/07 23:39:15  snelling
// Fixed Linux warnings
//
// Revision 1.5  1999/12/04 00:13:35  posk
// Works with StFlowEvent which works with the new StEvent
//
// Revision 1.4  1999/11/11 23:12:59  posk
// Rearrangement of files.
//
// Revision 1.3  1999/08/09 21:43:05  snelling
// removed parameters from cxx file
//
// Revision 1.2  1999/07/26 23:39:34  snelling
// added histograms for FlowTag QA
//
//  
//////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <math.h>
#include "../StFlowMaker/StFlowMaker.hh"
#include "../StFlowMaker/StFlowEvent.hh"
#include "../StFlowMaker/StFlowConstants.hh"
#include "StFlowTagMaker.hh"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "TString.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TVector2.h"
#define PR(x) cout << "##### FlowTag: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowTagMaker)

//-------------------------------------------------------------

StFlowTagMaker::StFlowTagMaker(const Char_t* name) : StMaker(name)
{
}

//-------------------------------------------------------------

StFlowTagMaker::~StFlowTagMaker() 
{
}

//-------------------------------------------------------------

Int_t StFlowTagMaker::Make() 
{
  // Create a new tag
  pFlowEvent = 0;
  pFlowTag   = 0;
  pFlowTag   = new FlowTag_st;

  // print pointer to flowtag 
  //cout << "TagPointer: " << pFlowTag << endl;

  // fill the Flow Tags 
  StFlowMaker* pFlowMaker = (StFlowMaker*)GetMaker("Flow");
  if (pFlowMaker) pFlowEvent = pFlowMaker->FlowEventPointer();
  if (pFlowEvent && pFlowTag) {
    fillFlowTag();    // fill the tag database
  } else {
    pFlowTag = 0;
    return kStOK;     // no StFlowEvent or no Tag pointer
  }

  if (Debug()) printTag();

  // fill histograms from the Flow Tags
  fillHistograms();

  return kStOK;
}

//-------------------------------------------------------------

void StFlowTagMaker::PrintInfo() 
{
  cout << "$Id: StFlowTagMaker.cxx,v 1.10 2000/01/13 23:22:05 snelling Exp $" << endl;
  if (Debug()) StMaker::PrintInfo();
}

//-------------------------------------------------------------

void StFlowTagMaker::printTag(ostream& os) 
{
  os << "##### Event-by-Event Flow Tag Table ---" << endl; 
  if (!pFlowTag) 
    os << "(empty FlowTag)" << endl;
  else {
    int i;
    os <<  "qxa";
    for(i=0; i<4; i++) os << "[" << i << "] =" << pFlowTag->qxa[i] << ' ';
    os << endl;
  }
}

//-------------------------------------------------------------

Int_t StFlowTagMaker::Finish() {

  return StMaker::Finish();
}

//-------------------------------------------------------------

Int_t StFlowTagMaker::Init()
{
  // Book histograms

  static const int& nHars = Flow::nHars;
  static const int& nSels = Flow::nSels;
  static const int& nSubs = Flow::nSubs;

  enum { nPsiBins    = 100,
	 nMeanPtBins = 100,
	 nMultBins   = 100,
	 n_qBins     = 100 }; 

  const Float_t PsiMin    =  0.;
  const Float_t PsiMax    =  twopi; 
  const Float_t MeanPtMin =  0.;
  const Float_t MeanPtMax =  1.;
  const Float_t MultMin   =  0.;
  const Float_t MultMax   =  1000.;
  const Float_t qMin      =  0.;
  const Float_t qMax      =  2.;

  for (int i = 0; i < nSels+nSubs; i++) {
    TString *mHistTitle;
    char mCountSubEvents[5];
    sprintf(mCountSubEvents,"%d",i);

    for (int j = 0; j < nHars; j++) {
      char mCountHarmonics[5];
      sprintf(mCountHarmonics,"%d",j);

      mHistTitle = new TString("HistPsi");
      mHistTitle->Append(*mCountSubEvents);
      mHistTitle->Append("Harmonic");
      mHistTitle->Append(*mCountHarmonics + 1);
      histSubEvents[i].histHarmonics[j].mHistPsi =
	new TH1F(mHistTitle->Data(), mHistTitle->Data(), nPsiBins,PsiMin,
		 (PsiMax / (float)(j + 1)));
      histSubEvents[i].histHarmonics[j].mHistPsi->SetXTitle("Psi");
      histSubEvents[i].histHarmonics[j].mHistPsi->SetYTitle("Counts");
      delete mHistTitle;

      mHistTitle = new TString("HistMeanPt");
      mHistTitle->Append(*mCountSubEvents);
      mHistTitle->Append("Harmonic");
      mHistTitle->Append(*mCountHarmonics + 1);
      histSubEvents[i].histHarmonics[j].mHistMeanPt =
	new TH1F(mHistTitle->Data(), mHistTitle->Data(), nMeanPtBins, MeanPtMin,
		 MeanPtMax);
      histSubEvents[i].histHarmonics[j].mHistMeanPt->
	SetXTitle("Mean Pt");
      histSubEvents[i].histHarmonics[j].mHistMeanPt->SetYTitle("Counts");
      delete mHistTitle;

      mHistTitle = new TString("HistMult");
      mHistTitle->Append(*mCountSubEvents);
      mHistTitle->Append("Harmonic");
      mHistTitle->Append(*mCountHarmonics + 1);
      histSubEvents[i].histHarmonics[j].mHistMult =
	new TH1D(mHistTitle->Data(), mHistTitle->Data(), nMultBins, MultMin,
		 MultMax);
      histSubEvents[i].histHarmonics[j].mHistMult->
	SetXTitle("Multiplicity");
      histSubEvents[i].histHarmonics[j].mHistMult->SetYTitle("Counts");
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

  return StMaker::Init();
}

//-------------------------------------------------------------

void StFlowTagMaker::fillFlowTag() {  
  // Fill Tag table

  static const int& nHars = Flow::nHars;
  TVector2 Q;
  int selN, subN;

  for (int j = 0; j < nHars ; j++) {

    // fill sub1 tags
    selN = 0, subN = 0;
    Q = pFlowEvent->Q(j, selN, subN);
    pFlowTag->qxa[j]  = Q.X();
    pFlowTag->qya[j]  = Q.Y();
    pFlowTag->na[j]   = pFlowEvent->Mult(j, selN, subN);
    pFlowTag->mpta[j] = pFlowEvent->MeanPt(j, selN, subN);

    // fill sub2 tags
    selN = 0, subN = 1;
    Q = pFlowEvent->Q(j, selN, subN);
    pFlowTag->qxb[j]  = Q.X();
    pFlowTag->qyb[j]  = Q.Y();
    pFlowTag->nb[j]   = pFlowEvent->Mult(j, selN, subN);
    pFlowTag->mptb[j] = pFlowEvent->MeanPt(j, selN, subN);

    // fill sub3 tags
    selN = 1, subN = 0;
    Q = pFlowEvent->Q(j, selN, subN);
    pFlowTag->qxc[j]  = Q.X();
    pFlowTag->qyc[j]  = Q.Y();
    pFlowTag->nc[j]   = pFlowEvent->Mult(j, selN, subN);
    pFlowTag->mptc[j] = pFlowEvent->MeanPt(j, selN, subN);

    // fill sub4 tags
    selN = 1, subN = 1;
    Q = pFlowEvent->Q(j, selN, subN);
    pFlowTag->qxd[j]  = Q.X();
    pFlowTag->qyd[j]  = Q.Y();
    pFlowTag->nd[j]   = pFlowEvent->Mult(j, selN, subN);
    pFlowTag->mptd[j] = pFlowEvent->MeanPt(j, selN, subN);

  }
}

//-------------------------------------------------------------

Int_t StFlowTagMaker::fillHistograms()
{
  // Fill histograms from Tag table

  static const int& nHars = Flow::nHars;

  for (int j = 0; j < nHars; j++) {
    float order = (float)(j+1);

    histSubEvents[0].histHarmonics[j].mHistMeanPt->
      Fill(pFlowTag-mpta[j]);
    histSubEvents[0].histHarmonics[j].mHistMult->Fill(pFlowTag->na[j]);
    histSubEvents[0].histHarmonics[j].mHist_q->
      Fill(sqrt(pFlowTag->qxa[j]*pFlowTag->qxa[j] +
	pFlowTag->qya[j]*pFlowTag->qya[j]) / sqrt((float)(pFlowTag->na[j])));
    histSubEvents[1].histHarmonics[j].mHistMeanPt->
      Fill(pFlowTag->mptb[j]);
    histSubEvents[1].histHarmonics[j].mHistMult->Fill(pFlowTag->nb[j]);
    histSubEvents[1].histHarmonics[j].mHist_q->
      Fill(sqrt(pFlowTag->qxb[j]*pFlowTag->qxb[j] +
	pFlowTag->qyb[j]*pFlowTag->qyb[j]) / sqrt((float)(pFlowTag->nb[j])));
    histSubEvents[2].histHarmonics[j].mHistMeanPt->
      Fill(pFlowTag->mptc[j]);
    histSubEvents[2].histHarmonics[j].mHistMult->Fill(pFlowTag->nc[j]);
    histSubEvents[2].histHarmonics[j].mHist_q->
      Fill(sqrt(pFlowTag->qxc[j]*pFlowTag->qxc[j] +
        pFlowTag->qyc[j]*pFlowTag->qyc[j]) / sqrt((float)(pFlowTag->nc[j])));
    histSubEvents[3].histHarmonics[j].mHistMeanPt->
      Fill(pFlowTag->mptd[j]);
    histSubEvents[3].histHarmonics[j].mHistMult->Fill(pFlowTag->nd[j]);
    histSubEvents[3].histHarmonics[j].mHist_q->
      Fill(sqrt(pFlowTag->qxd[j]*pFlowTag->qxd[j] +
        pFlowTag->qyd[j]*pFlowTag->qyd[j]) / sqrt((float)(pFlowTag->nd[j])));

    float EventPlaneAngle1 = atan2(pFlowTag->qya[j], pFlowTag->qxa[j]) / order;
    if (EventPlaneAngle1 < 0.) {EventPlaneAngle1 += twopi / order;}
    histSubEvents[0].histHarmonics[j].mHistPsi->Fill(EventPlaneAngle1);
    float EventPlaneAngle2 = atan2(pFlowTag->qyb[j], pFlowTag->qxb[j]) / order;
    if (EventPlaneAngle2 < 0.) {EventPlaneAngle2 += twopi / order;}
    histSubEvents[1].histHarmonics[j].mHistPsi->Fill(EventPlaneAngle2);
    float EventPlaneAngle3 = atan2(pFlowTag->qyc[j], pFlowTag->qxc[j]) / order;
    if (EventPlaneAngle3 < 0.) {EventPlaneAngle3 += twopi / order;}
    histSubEvents[2].histHarmonics[j].mHistPsi->Fill(EventPlaneAngle3);
    float EventPlaneAngle4 = atan2(pFlowTag->qyd[j], pFlowTag->qxd[j]) / order;
    if (EventPlaneAngle4 < 0.) {EventPlaneAngle4 += twopi / order;}
    histSubEvents[3].histHarmonics[j].mHistPsi->Fill(EventPlaneAngle4);
  }

  return kStOK;
}
