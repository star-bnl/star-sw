// $Id: StPeCMaker.h,v 1.6 2000/03/24 22:36:16 nystrand Exp $
//
// $Log: StPeCMaker.h,v $
// Revision 1.6  2000/03/24 22:36:16  nystrand
// First version with StPeCEvent
//
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
#ifndef StPeCMaker_HH
#define StPeCMaker_HH
#include "StMaker.h"
#include "StPeCEvent.h"
#include "TH1.h"
#include "TFile.h"

class StEvent;
class StPeCEvent;
class StRun;
class TH1F;

class StPeCMaker : public StMaker {

protected:
  TFile *m_outfile;
  TH1F *m_hstat;
  TH1F *m_hntrk; 
  TH1F *m_hnvtxtrk;
  TH1F *m_hnmwchts;
  TH1F *m_hnctbhts;
  TH1F *m_hsumq;
  TH1F *m_hsumpt;
  TH1F *m_hzvert;
  TH1F *m_hminvpi;
  TH1F *m_hminvk;
  TH1F *m_hrappi;
  TH1F *m_hrapka;

public:

  StPeCMaker(const Char_t *name="analysis");
  virtual ~StPeCMaker();
  virtual void Clear(Option_t *option="");
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual Int_t  Finish();

private:

  Int_t FillStPeCEvent(StEvent *event, StPeCEvent *pevent);
  Int_t FillHistograms(StPeCEvent *pevent);
  Int_t ExampleAnalysis(StPeCEvent *pevent);

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StPeCMaker.h,v 1.6 2000/03/24 22:36:16 nystrand Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StPeCMaker, 1)
};

#endif



