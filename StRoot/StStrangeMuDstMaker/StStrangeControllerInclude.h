// $Id: StStrangeControllerInclude.h,v 2.0 2000/06/05 05:19:42 genevb Exp $
// $Log: StStrangeControllerInclude.h,v $
// Revision 2.0  2000/06/05 05:19:42  genevb
// New version of Strangeness micro DST package
//
//
#ifndef STAR_StStrangeControllerInclude
#define STAR_StStrangeControllerInclude
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StStrangeControllerInclude: include file for controller classes      //
// StV0Controller strangeness micro DST controller for V0s              //
// StXiController strangeness micro DST controller for Xis              //
// StKinkController strangeness micro DST controller for Kinks          //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "StStrangeControllerBase.h"
#include "StStrangeMuDstMaker.h"
#include "StMessMgr.h"


//_____________________________________________________________________________
inline void StStrangeControllerBase::PrintNumCand(const char* text, Int_t num) {
  gMessMgr->Info() << IsA()->GetName() << ": " << text << " "
                   << num << " " << GetName() << " candidates" << endm;
}
//_____________________________________________________________________________
inline StStrangeControllerBase* StStrangeControllerBase::GetDstController() {
  return dstMaker->Get(GetName());
}
//_____________________________________________________________________________

class StV0Controller : public StStrangeControllerBase {
 public:
  StV0Controller();
  virtual ~StV0Controller();
  virtual Int_t MakeReadDst();
  virtual Int_t MakeCreateDst(StEvent& event);
  virtual Int_t MakeCreateMcDst(StMcVertex* mcVert);
  virtual Int_t MakeCreateSubDst();
  ClassDef(StV0Controller,1)
};

class StXiController : public StStrangeControllerBase {
 public:
  StXiController();
  virtual ~StXiController();
  virtual Int_t MakeReadDst();
  virtual Int_t MakeCreateDst(StEvent& event);
  virtual Int_t MakeCreateMcDst(StMcVertex* mcVert);
  virtual Int_t MakeCreateSubDst();
  ClassDef(StXiController,1)
};

class StKinkController : public StStrangeControllerBase {
 public:
  StKinkController();
  virtual ~StKinkController();
  virtual Int_t MakeReadDst();
  virtual Int_t MakeCreateDst(StEvent& event);
  virtual Int_t MakeCreateMcDst(StMcVertex* mcVert);
  virtual Int_t MakeCreateSubDst();
  ClassDef(StKinkController,1)
};

#endif
