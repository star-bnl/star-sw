/**********************************************************************
 *
 * $Id: StEbyeDSTMaker.h,v 1.1 2000/08/14 22:05:20 jseger Exp $
 *
 * Author: Jeff Reid, UW, July 2000
 *         incorporates elements of code by
 *         Poskanzer, Snellings, & Voloshin
 *
 **********************************************************************
 *
 * Description:  This is an maker designed to read a STAR dst and
 *               summarize it into an EbyeDST which contains only
 *               the information necessary for EbyE analysis.
 *               The EbyEDST elements are intentionally independent
 *               of any STAR infrastructure so that data from other
 *               experiments (i.e. NA49) can be written in the same
 *               format and accessed and analyzed outside of RCF/BNL.
 *
 **********************************************************************
 *
 * $Log: StEbyeDSTMaker.h,v $
 * Revision 1.1  2000/08/14 22:05:20  jseger
 * Added eta-spectra.  Now reads Ebye mini-DST as input.  Bins events in
 * multiplicity and z-vertex position.  Name of output file is no longer hard-wired.
 *
 * Revision 1.1.1.1  2000/08/01 13:57:55  jgreid
 * EbyE DST creation and access tools
 *
 *
 *********************************************************************/

#ifndef StEbyeDSTMaker_HH
#define StEbyeDSTMaker_HH

#include <iostream.h>
#include <fstream.h>
#include "TTree.h"
#include "TFile.h"
#include "StMaker.h"
#include "StEbyeEvent.h"

class StEvent;
class StRun;

class StEbyeDSTMaker : public StMaker {

private:

  StEbyeEvent* mEbyeEvent;
  TTree* mEbyeTree;
  TFile* mEbyeDST;
  char mDSTFilename[64];

public:

  StEbyeDSTMaker(const Char_t *name="EbyeDST", const Char_t *title="EbyeDST");
  ~StEbyeDSTMaker();
  void Clear(Option_t *option="");
  Int_t Init();
  Int_t Make();
  Int_t Finish();

  void SetFilename(const Char_t* name="EbyeDST.root");

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StEbyeDSTMaker.h,v 1.1 2000/08/14 22:05:20 jseger Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StEbyeDSTMaker, 1)
};

inline void StEbyeDSTMaker::SetFilename(const Char_t* name) {
  strncpy(mDSTFilename, name, 63); mDSTFilename[63] = '\0';
}


#endif
