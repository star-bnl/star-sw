// $Id: StV0MiniDstMaker.h,v 1.1 1999/07/13 12:42:25 jones Exp $
// $Log: StV0MiniDstMaker.h,v $
// Revision 1.1  1999/07/13 12:42:25  jones
// *** empty log message ***
//
#ifndef STAR_StV0MiniDstMaker
#define STAR_StV0MiniDstMaker
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StV0MiniDstMaker virtual base class for Maker                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

class TFile;
class TOrdCollection;

class StV0MiniDstMaker : public StMaker {
 private:
 protected:
  TFile *output;                       //!
  const char *rootfile;                //!
  TOrdCollection *v0MiniDstCollection; //!
 public: 
  StV0MiniDstMaker(const char *name="v0dst", const char *output="V0MiniDst.root");
  virtual ~StV0MiniDstMaker();
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();
  virtual void  PrintInfo();
  ClassDef(StV0MiniDstMaker,1)   //virtual base class for Makers
};

#endif
