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
#include "StPeCLumiEntry.h"
#include "StPeCEvent.h"
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
class StMuDst;

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
  virtual Int_t  InitRun(Int_t runnr);
  virtual Int_t  Make();
  virtual Int_t  Finish();
  void setMuDst(StMuDst* mu) {muDst = mu;};	//Accessor for muDst pointer
  //  void setFileName ( TString name ) { treeFileName = name ; } ;

  //  TString treeFileName ;
private:

  StMuDst* muDst;
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StPeCLumiMaker.h,v 1.6 2014/08/06 11:43:32 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

  ClassDef(StPeCLumiMaker,1)
};

#endif



