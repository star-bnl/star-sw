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
class St_ObjectSet;

class StDAQMaker : public StIOInterFace {
 private:
  StEvtHddr 	*fEvtHddr;	//! pointer to Event Header
  StDAQReader 	*fDAQReader;	//! pointer to StDAQReader
  St_ObjectSet 	*fDAQReaderSet;	//! pointer to StDAQReader DataSet
 public:
  StDAQMaker(const char *name="DAQInput",const char *inputFile=0);
  virtual       ~StDAQMaker();
  virtual Int_t  Init();
  virtual Int_t  Open(const char *file=0);
  virtual void   Close(Option_t *opt=0);
  virtual Int_t  Make();
  virtual void   Skip(Int_t Nskip=1);
//	for compatability with StIOInterFace
  void SetBranch(const Char_t *brName,const Char_t *file=0,const Char_t *iomode="r"){};



  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StDAQMaker.h,v 1.1 1999/07/13 01:54:12 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StDAQMaker, 0)   //
};

#endif
