// $Id: StPeCMaker.cxx,v 1.7 2000/01/20 23:03:08 nystrand Exp $
// $Log: StPeCMaker.cxx,v $
// Revision 1.7  2000/01/20 23:03:08  nystrand
// First Version of StPeCMaker with new StEvent
//
// Revision 1.6  1999/09/24 01:23:19  fisyak
// Reduced Include Path
//
// Revision 1.5  1999/07/15 13:57:20  perev
// cleanup
//
// Revision 1.4  1999/06/27 22:45:29  fisyak
// Merge StRootEvent and StEvent
//
// Revision 1.3  1999/05/01 00:57:02  fisyak
// Change Clear function to defualt
//
// Revision 1.2  1999/04/08 16:37:15  nystrand
// MakeBranch,SetBranch removed
//
// Revision 1.1  1999/04/06 20:47:27  akio
// The first version
//
// Revision 1.0  1999/03/05 11:00:00  Nystrand
// initial version
//
//
///////////////////////////////////////////////////////////////////////////////
//
// StPeCMaker
//
// Description: 
//  Sample maker to access and analyze Peripheral Collisions through StEvent
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Joakim Nystrand, LBNL
//
// History:
//
///////////////////////////////////////////////////////////////////////////////
#include "StPeCMaker.h"
#include "StEventTypes.h"
#include "Stypes.h"
#include "TH1.h"

static const char rcsid[] = "$Id: StPeCMaker.cxx,v 1.7 2000/01/20 23:03:08 nystrand Exp $";


Int_t StPeCMaker::Init() {

  cout<<"This is StPeCMaker. Under construction..."<<endl;

  m_hstat     = new TH1F("hstat","Statistics: Neve, N2-15, N2VtxTrk, Nmom",4,-0.5,3.5);
  m_hntrk     = new TH1F("hntrk","Number of TPC Tracks",50,-0.5,49.5);
  m_hntrk5    = new TH1F("hntrk5","#TPC Tracks, nhits>5",50,-0.5,49.5);
  m_hntrk10   = new TH1F("hntrk10","#TPC Tracks, nhits>10",50,-0.5,49.5);
  m_hnmwchts  = new TH1F("hnmwchts","#MWC Hits",50,-0.5,49.5);
  m_hnctbhts  = new TH1F("hnctbhts","#CTB Hits",50,-0.5,49.5);
  m_hnvtx     = new TH1F("hnvtx","#Primary Vertices",10,-0.5,9.5);
  m_hnvtxtrk  = new TH1F("hnvtxtrk","#Primary Vertex Tracks",50,-0.5,49.5);
  m_hsumq     = new TH1F("hsumq","Sum Q",11,-5.5,5.5);
  m_hsumpt    = new TH1F("hsumpt","Sum Pt",50,0.0,0.75);
  m_hzvert    = new TH1F("hzvert","Z Vertex (cm)",50,-60.0,60.0);
  m_hminvpi   = new TH1F("hminvpi","2-Track Evts. Minv pions",50,0.2,1.5);
  m_hminvk    = new TH1F("hminvk","2-Track Evts. Minv kaons",50,0.8,2.0);
  return StMaker::Init();

}


Int_t StPeCMaker::Make() {

  StEvent ev;

  GetMwcHits(ev);

  GetCtbHits(ev);

  return kStOK;
}


void StPeCMaker::GetCtbHits(StEvent& event) {
  // Not available at this time

}

void StPeCMaker::GetMwcHits(StEvent& event) {
  // Not avaliable at this time

}

StPeCMaker::~StPeCMaker() {
}


Int_t StPeCMaker::Finish() {
  return kStOK;
}

ClassImp(StPeCMaker)




