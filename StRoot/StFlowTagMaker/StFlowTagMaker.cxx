//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowTagMaker.cxx,v 1.6 1999/12/07 23:39:15 snelling Exp $
//
// Author: Raimond Snellings and Art Poskanzer, LBNL, Jun 1999
// Description:  Maker to fill the Flow EbyE Tag database
//
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowTagMaker.cxx,v $
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
// FlowTagMaker: fills FlowTags defined by EbE workgroup
//
//  
//////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <math.h>
#include "../StFlowMaker/StFlowMaker.hh"
#include "../StFlowMaker/StFlowEvent.hh"
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
  pFlowTag = 0;
  pFlowTag = new FlowTag_st;

  // print pointer to flowtag 
  cout << "TagPointer: " << pFlowTag << endl;

  // fill the Flow Tags 
  StFlowMaker* flowMaker = (StFlowMaker*)GetMaker("Flow");
  pFlowEvent = flowMaker->FlowEventPointer();
  if (pFlowEvent && pFlowTag) {
    fillFlowTag();    // fill the tag database
  } else {
    pFlowTag = 0;
    return kStOK;     // no StFlowEvent or no Tag pointer
  }

  printTag();

  // fill histograms from Flow Tag
  fillHistograms();

  return kStOK;
}

//-------------------------------------------------------------

void StFlowTagMaker::PrintInfo() 
{
  cout << "$Id: StFlowTagMaker.cxx,v 1.6 1999/12/07 23:39:15 snelling Exp $" << endl;
  if (Debug()) StMaker::PrintInfo();
}

//-------------------------------------------------------------

void StFlowTagMaker::printTag(ostream& os) 
{
  os << "--- Event-by-Event Flow Tag Table ---" << endl; 
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

  return kStOK;
}

//-------------------------------------------------------------

Int_t StFlowTagMaker::Init()
{

  const Float_t PsiMin   =  0.;
  const Float_t PsiMax   =  twopi; 
  const Float_t MeanPtMin =  0.;
  const Float_t MeanPtMax =  1.;
  const Float_t MultMin  =  0.;
  const Float_t MultMax  =  1000.;
  const Float_t qMin     =  0.;
  const Float_t qMax     =  2.;

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

  return kStOK;
}

//-------------------------------------------------------------

void StFlowTagMaker::fillFlowTag() {  
  TVector2 Q;
  int selN, subN;
  for (int j = 0; j < nHars ; j++) {

    // fill sub1 tags
    selN = 0, subN = 0+1;
    Q = pFlowEvent->Q(j, selN, subN);
    pFlowTag->qxa[j]  = Q.X();
    pFlowTag->qya[j]  = Q.Y();
    pFlowTag->na[j]   = pFlowEvent->Mult(j, selN, subN);
    pFlowTag->spta[j] = pFlowEvent->MeanPt(j, selN, subN);

    // fill sub2 tags
    selN = 0, subN = 1+1;
    Q = pFlowEvent->Q(j, selN, subN);
    pFlowTag->qxb[j]  = Q.X();
    pFlowTag->qyb[j]  = Q.Y();
    pFlowTag->nb[j]   = pFlowEvent->Mult(j, selN, subN);
    pFlowTag->sptb[j] = pFlowEvent->MeanPt(j, selN, subN);

    // fill sub3 tags
    selN = 1, subN = 0+1;
    Q = pFlowEvent->Q(j, selN, subN);
    pFlowTag->qxc[j]  = Q.X();
    pFlowTag->qyc[j]  = Q.Y();
    pFlowTag->nc[j]   = pFlowEvent->Mult(j, selN, subN);
    pFlowTag->sptc[j] = pFlowEvent->MeanPt(j, selN, subN);

    // fill sub4 tags
    selN = 1, subN = 1+1;
    Q = pFlowEvent->Q(j, selN, subN);
    pFlowTag->qxd[j]  = Q.X();
    pFlowTag->qyd[j]  = Q.Y();
    pFlowTag->nd[j]   = pFlowEvent->Mult(j, selN, subN);
    pFlowTag->sptd[j] = pFlowEvent->MeanPt(j, selN, subN);

  }
}

//-------------------------------------------------------------

Int_t StFlowTagMaker::fillHistograms()
{
  for (int j = 0; j < nHars; j++) {
    float order  = (float)(j+1);

    histSubEvents[0].histHarmonics[j].mHistMeanPt->
      Fill(pFlowTag->spta[j]);
    histSubEvents[0].histHarmonics[j].mHistMult->Fill(pFlowTag->na[j]);
    histSubEvents[0].histHarmonics[j].mHist_q->
      Fill(sqrt(pFlowTag->qxa[j]*pFlowTag->qxa[j] +
	pFlowTag->qya[j]*pFlowTag->qya[j]) / sqrt(pFlowTag->na[j]));
    histSubEvents[1].histHarmonics[j].mHistMeanPt->
      Fill(pFlowTag->sptb[j]);
    histSubEvents[1].histHarmonics[j].mHistMult->Fill(pFlowTag->nb[j]);
    histSubEvents[1].histHarmonics[j].mHist_q->
      Fill(sqrt(pFlowTag->qxb[j]*pFlowTag->qxb[j] +
	pFlowTag->qyb[j]*pFlowTag->qyb[j]) / sqrt(pFlowTag->nb[j]));
    histSubEvents[2].histHarmonics[j].mHistMeanPt->
      Fill(pFlowTag->sptc[j]);
    histSubEvents[2].histHarmonics[j].mHistMult->Fill(pFlowTag->nc[j]);
    histSubEvents[2].histHarmonics[j].mHist_q->
      Fill(sqrt(pFlowTag->qxc[j]*pFlowTag->qxc[j] +
        pFlowTag->qyc[j]*pFlowTag->qyc[j]) / sqrt(pFlowTag->nc[j]));
    histSubEvents[3].histHarmonics[j].mHistMeanPt->
      Fill(pFlowTag->sptd[j]);
    histSubEvents[3].histHarmonics[j].mHistMult->Fill(pFlowTag->nd[j]);
    histSubEvents[3].histHarmonics[j].mHist_q->
      Fill(sqrt(pFlowTag->qxd[j]*pFlowTag->qxd[j] +
        pFlowTag->qyd[j]*pFlowTag->qyd[j]) / sqrt(pFlowTag->nd[j]));

    Float_t EventPlaneAngle1 = atan2(pFlowTag->qya[j], pFlowTag->qxa[j]) / order;
    if (EventPlaneAngle1 < 0.) {EventPlaneAngle1 += twopi / order;}
    histSubEvents[0].histHarmonics[j].mHistPsi->Fill(EventPlaneAngle1);
    Float_t EventPlaneAngle2 = atan2(pFlowTag->qyb[j], pFlowTag->qxb[j]) / order;
    if (EventPlaneAngle2 < 0.) {EventPlaneAngle2 += twopi / order;}
    histSubEvents[1].histHarmonics[j].mHistPsi->Fill(EventPlaneAngle2);
    Float_t EventPlaneAngle3 = atan2(pFlowTag->qyc[j], pFlowTag->qxc[j]) / order;
    if (EventPlaneAngle3 < 0.) {EventPlaneAngle3 += twopi / order;}
    histSubEvents[2].histHarmonics[j].mHistPsi->Fill(EventPlaneAngle3);
    Float_t EventPlaneAngle4 = atan2(pFlowTag->qyd[j], pFlowTag->qxd[j]) / order;
    if (EventPlaneAngle4 < 0.) {EventPlaneAngle4 += twopi / order;}
    histSubEvents[3].histHarmonics[j].mHistPsi->Fill(EventPlaneAngle4);
  }

  return kStOK;
}
