// $Id: StPeCMaker.h,v 1.5 2000/01/20 23:03:15 nystrand Exp $
//
// $Log: StPeCMaker.h,v $
// Revision 1.5  2000/01/20 23:03:15  nystrand
// First Version of StPeCMaker with new StEvent
//
// Revision 1.3  1999/07/15 13:57:21  perev
// cleanup
//
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
#include "TH1.h"

class StEvent;
class StRun;
class TH1F;

class StPeCMaker : public StMaker {

private:

  // Maker generates a tag
  void  GetMwcHits(StEvent& event);// MWC Hits
  void  GetCtbHits(StEvent& event);// CTB Hits

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
  virtual Int_t Init();
  virtual Int_t  Make();
  virtual Int_t  Finish();


  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StPeCMaker.h,v 1.5 2000/01/20 23:03:15 nystrand Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StPeCMaker, 1)
};

#endif



