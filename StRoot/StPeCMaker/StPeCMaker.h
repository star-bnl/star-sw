// $Id: StPeCMaker.h,v 1.2 1999/04/08 16:37:27 nystrand Exp $
//
// $Log: StPeCMaker.h,v $
// Revision 1.2  1999/04/08 16:37:27  nystrand
// MakeBranch,SetBranch removed
//
// Revision 1.1  1999/04/06 20:47:35  akio
// The first version
//
// Revision 1.0  1999/03/05 11:00:00  Nystrand
// First Version
//

#ifndef StPeCMaker_HH
#define StPeCMaker_HH

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
#include "StMaker.h"
#include "tables/HighPtTag.h"
#include "TH1.h"

class StEvent;
class StRun;
class TH1F;

class StPeCMaker : public StMaker {

private:
  Bool_t drawinit;
  Char_t collectionName[256];

  // Maker generates a tag
  HighPtTag_st* theTag; //!
  void  GetMwcHits(StEvent& event);// MWC Hits
  void  GetCtbHits(StEvent& event);// CTB Hits
  void  FindVertex(StEvent& event);// Vertex

protected:
  TH1F *m_hstat;
  TH1F *m_hntrk; 
  TH1F *m_hntrk5;
  TH1F *m_hntrk10;
  TH1F *m_hnmwchts;
  TH1F *m_hnctbhts;
  TH1F *m_hnvtx;
  TH1F *m_hnvtxtrk;
  TH1F *m_hsumq;
  TH1F *m_hsumpt;
  TH1F *m_hzvert;
  TH1F *m_hminvpi;
  TH1F *m_hminvk;

public:

  StPeCMaker(const Char_t *name="analysis", const Char_t *title="analysis");
  virtual ~StPeCMaker();
  virtual void Clear(Option_t *option="");
  virtual Int_t Init();
  virtual Int_t  Make();
  virtual void   PrintInfo();
  virtual Int_t  Finish();

  // Tag accessor
  HighPtTag_st* tag() {return theTag;};

  ClassDef(StPeCMaker, 1)
};

#endif
