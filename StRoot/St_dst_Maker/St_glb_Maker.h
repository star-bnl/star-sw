// $Id: St_glb_Maker.h,v 1.20 1999/07/01 17:27:43 fisyak Exp $
// $Log: St_glb_Maker.h,v $
// Revision 1.20  1999/07/01 17:27:43  fisyak
// New global chain from  Wensheng Deng
//
// Revision 1.19  1999/06/25 22:48:39  caines
// Added ability to perform ev0 evaluation using eval flag
//
// Revision 1.18  1999/03/11 03:12:19  perev
// new schema
//
// Revision 1.17  1999/03/04 01:19:22  fisyak
// Put release tag to run_summary table
//
// Revision 1.16  1999/02/22 21:27:20  kathy
// moved hist from St_glb_Maker to St_QA_Maker and had to rename some etc
//
// Revision 1.15  1999/02/18 16:43:12  caines
// Added in est the 4th layer tracking
//
// Revision 1.14  1999/02/16 03:03:47  fisyak
// Split Make and Histograms
//
// Revision 1.13  1999/02/13 20:22:32  caines
// Added exi and temp dir for when svt not there
//
// Revision 1.12  1999/02/12 22:27:39  ogilvie
// added in spectra/pid QA histograms
//
// Revision 1.11  1999/02/12 19:23:46  didenko
// updated v0 finding code from Helen
//
// Revision 1.10  1999/01/02 19:08:17  fisyak
// Add ctf
//
// Revision 1.9  1998/12/21 19:41:51  fisyak
// Move dst 2 glb
//
// Revision 1.8  1998/12/21 19:26:09  fisyak
// Make ROOT include non system
//
// Revision 1.7  1998/12/16 22:22:41  fisyak
// New global from Spiros
//
// Revision 1.6  1998/12/12 02:37:53  fisyak
// fix evr
//
// Revision 1.5  1998/11/01 16:42:27  fisyak
// dst analysis
//
// Revision 1.4  1998/10/31 00:26:13  fisyak
// Makers take care about branches
//
// Revision 1.3  1998/10/06 18:00:34  perev
// cleanup
//
// Revision 1.2  1998/09/08 22:43:11  fisyak
// Modify St_glb_Maker to account new calling sequence
//
// Revision 1.1  1998/08/18 14:06:07  fisyak
// Add to bfc dst
//
// Revision 1.3  1998/08/10 02:32:07  fisyak
// Clean up
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//
#ifndef STAR_St_glb_Maker
#define STAR_St_glb_Maker

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StMatchMaker.h"
#include "StPrimaryMaker.h"
#include "StV0Maker.h"
#include "StXiMaker.h"
#include "StKinkMaker.h"

class St_glb_Maker : public StMaker {
public:
  St_glb_Maker(const char *name="glb");
  ~St_glb_Maker();
#if 0
  virtual Int_t Init();
  virtual Int_t  Make();
#endif
  virtual Int_t  GetDebug() const; 
  virtual Int_t  Debug() const {return GetDebug();}
  virtual  void   SetDebug(Int_t);
protected:
private: 
  StMatchMaker   *matchMaker;
  StPrimaryMaker *primaryMaker;
  StV0Maker      *v0Maker;
  StXiMaker      *xiMaker;
  StKinkMaker    *kinkMaker;

  ClassDef(St_glb_Maker, 1)   //StAF chain virtual base class for Makers
};
 
#endif
