///////////////////////////////////////////////////////////////////////////////
//
// StPeCLumiMaker
//
// Description: 
//  Writes a small ntuple to analyse luminosity
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Falk Meissner
//
// History:
//
///////////////////////////////////////////////////////////////////////////////
#ifndef StPeCLumiMaker_HH
#define StPeCLumiMaker_HH
#include "StMaker.h"
#include "StPeCEvent.h"
#include "StPeCLumiEntry.h"
#include "StPeCGeant.h"
#include "TH1.h"
#include "TH2.h"
#include "TNtuple.h"
#include "TFile.h"
#include "TTree.h"

class StEvent;
class StPeCEvent;
class StRun;
class TH1F;
class TH2F;

class StPeCLumiMaker : public StMaker {

protected:
  TFile *m_outfile;

  TTree   *uDstTree ;

  StPeCLumiEntry *LumiEntry ;

public:

  StPeCLumiMaker(const Char_t *name="analysis");
  virtual ~StPeCLumiMaker();
  //  virtual void Clear(Option_t *option="");
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual Int_t  Finish();


private:


  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StPeCLumiMaker.h,v 1.1 2002/03/19 22:23:42 meissner Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StPeCLumiMaker, 1)
};

#endif



