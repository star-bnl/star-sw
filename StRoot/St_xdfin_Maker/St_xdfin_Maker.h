#ifndef STAR_St_xdfin_Maker
#define STAR_St_xdfin_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_xdfin_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "StIOInterFace.h"
#include "St_XDFFile.h"

class St_xdfin_Maker : public StIOInterFace {
 private:
  St_XDFFile fXdfin;
  Int_t m_InitDone;
  StEvtHddr *fEvtHddr;//! pointer to Event Header

 public:
  St_xdfin_Maker(const char *name="xdfin",const char *inputFile=0);
  virtual       ~St_xdfin_Maker();
  virtual Int_t  Init();
  virtual Int_t  Open(const char *file=0);
  virtual void   Close(Option_t *opt=0);
  virtual void   Init_Done (Bool_t k=kFALSE){m_InitDone = k;} // *MENU*
  virtual Int_t  Make();
  virtual Int_t   Skip(Int_t Nskip=1); 		// 
//	for compatability with StIOInterFace
  void SetBranch(const Char_t *brName,const Char_t *file=0,const Char_t *iomode="r"){};

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: St_xdfin_Maker.h,v 1.15 1999/07/15 13:58:36 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(St_xdfin_Maker, 0)   //StAF chain virtual base class for Makers
};

#endif
