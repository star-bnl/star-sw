#ifndef STAR_StDAQMaker
#define STAR_StDAQMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_xdfin_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "StIOInterFace.h"

//	Forward declarations
class StDAQReader;
class TObjectSet;

class StDAQMaker : public StIOInterFace {
 private:
  StEvtHddr 	*fEvtHddr;	//! pointer to Event Header
  StDAQReader 	*fDAQReader;	//! pointer to StDAQReader
  TObjectSet 	*fDAQReaderSet;	//! pointer to StDAQReader DataSet
  char mMergeSequences;
 public:
  StDAQMaker(const char *name="DAQInput",const char *inputFile=0);
  virtual       ~StDAQMaker();
  virtual Int_t  Init();
  virtual Int_t  Open(const char *file=0);
  virtual void   Close(Option_t *opt=0);
  virtual Int_t  Make();
  virtual Int_t  MakeRead(){return Make();};
  virtual Int_t  MakeRead(const StUKey &){return MakeRead();}
  virtual Int_t  Skip(Int_t Nskip=1);
//	for compatability with StIOInterFace
  void SetBranch(const Char_t*,const Char_t*,const Char_t*, const Option_t*){};
  void MergeSequences() { mMergeSequences=1; }
  void DoNotMergeSequences() { mMergeSequences=0; }

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StDAQMaker.h,v 1.8 2004/03/04 21:51:27 ward Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StDAQMaker, 0)   //
};

#endif
