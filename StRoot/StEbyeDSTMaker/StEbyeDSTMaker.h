/**********************************************************************
 *
 * $Id: StEbyeDSTMaker.h,v 1.2 2000/09/01 22:59:11 jgreid Exp $
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
 * Revision 1.2  2000/09/01 22:59:11  jgreid
 * version 1 revision ; multiple file handling + additional data members added
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
#include "TString.h"
#include "StIOMaker/StIOMaker.h"
#include "StMaker.h"
#include "StEbyeEvent.h"

class StEvent;
class StRun;
class StIOMaker;

class StEbyeDSTMaker : public StMaker {

private:

  StEbyeEvent* mEbyeEvent;
  TTree* mEbyeTree;
  TFile* mEbyeDST;
  TString mCurrentInputFilename;
  StIOMaker* mIOMaker;                  //! pointer to the IOMaker

  Int_t OpenCurrentFile();
  Int_t CloseCurrentFile();

public:

  StEbyeDSTMaker(const Char_t *name="EbyeDST", const Char_t *title="EbyeDST");
  ~StEbyeDSTMaker();
  void Clear(Option_t *option="");
  Int_t Init();
  Int_t Make();
  Int_t Finish();

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StEbyeDSTMaker.h,v 1.2 2000/09/01 22:59:11 jgreid Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StEbyeDSTMaker, 1)
};


#endif
