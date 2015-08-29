/**********************************************************************
 *
 * $Id: StEStructEventMaker.h,v 1.2 2015/08/29 03:24:18 perev Exp $
 *
 * Author: Jeff Porter rework of Jeff Reid's code
 *
 **********************************************************************
 *
 * Description:  This is an maker designed to read a STAR dst and
 *               summarize it into an EStructEvent which contains only
 *               the information necessary for EbyE analysis.
 *
 **********************************************************************/

#ifndef StEStructEventMaker_HH
#define StEStructEventMaker_HH

#include <Stiostream.h>
#include "TTree.h"
#include "TFile.h"
#include "TString.h"
#include "TChain.h"
#include "StMaker.h"
#include "StEStructEvent.h"


class StEStructEventMaker : public StMaker {

private:

  // input containors...
  char* mfileList; //!
  int   mfileCount; //!
  int   meventCount; //!
  TChain* mChain; //!

  // output containors...
  char* moutFile; //!
  TFile* mEStructEventFile; //!
  TTree* mEStructTree;   //!

  StEStructEvent* mEStructEvent; //->

  bool  readEvent();
  bool  openWrite();
  bool  openRead();

  void  setName(const char* name, int opt);

public:

  StEStructEventMaker(const Char_t *name="EStructEvent", const Char_t *title="EStructEvent");
  ~StEStructEventMaker();
  void Clear(Option_t *option="");
  Int_t Init();
  Int_t Make();
  Int_t Finish();

  // very simply model.... input filelist
  // output is null or a single file....

  void  setInputFileList(const char* flist);
  void  setOutputFile(const char* ofile);
  StEStructEvent* event();
  bool  writeEvent(StEStructEvent* e);
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StEStructEventMaker.h,v 1.2 2015/08/29 03:24:18 perev Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

  ClassDef(StEStructEventMaker, 1)
};

#endif


/**********************************************************************
 *
 * $Log: StEStructEventMaker.h,v $
 * Revision 1.2  2015/08/29 03:24:18  perev
 * __DATE
 *
 * Revision 1.1  2003/10/15 18:20:51  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/

