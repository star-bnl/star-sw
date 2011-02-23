/**
 * $Id $
 * \file  StContamPair.h
 * \brief Definition of Pair for Contamination tracks (secondary tracks, weak decay tracks)
 * 
 *
 * \author Bum Choi
 * \date   March 2001
 *  
 * This class essentially just includes some parent mc Information for backgrounds
 */

#ifndef StContamPair_H
#define StContamPair_H

#include "StMiniMcPair.h"

class StContamPair : public StMiniMcPair {
 public:
  StContamPair();
  virtual ~StContamPair();

  void setGeantProcess(int val) 	{ mGeantProcess=val;  }
  void setPtMcParent(Float_t val) 	{ mPtMcParent=val ; }
  void setEtaMcParent(Float_t val) 	{ mEtaMcParent=val;}
  void setStartX(Float_t val) 		{ mStartX=val;} // where the parent stops
  void setStartY(Float_t val) 		{ mStartY=val;}
  void setStartZ(Float_t val) 		{ mStartZ=val;}
  void setPtMcParentParent(Float_t val) { mPtMcParentParent   =val;}
  void setParentParentGeantId(int val)  { mParentParentGeantId=val;}

  Int_t      mGeantProcess;
  Float_t    mPtMcParent;
  Float_t    mEtaMcParent;
  Int_t      mParentParentGeantId;
  Float_t    mPtMcParentParent;  

  Float_t    mStartX;
  Float_t    mStartY;
  Float_t    mStartZ;
  
   
  ClassDef(StContamPair,1)
};
  
#endif

//
// $Log: StContamPair.h,v $
// Revision 1.3  2011/02/23 19:34:42  perev
// Remove redundant mParentGeantId
//
// Revision 1.2  2011/02/22 19:20:17  perev
// now int mParentParentGeantId
//
// Revision 1.1  2002/05/30 01:20:57  calderon
// Classes for use in a general framework for extracting efficiencies
// from both embedding and full simulations
// (after GSTAR+TRS+StEvent+StMcEvent+StAssociationMaker)
// so that the information of the track matches gets stored persistently.
//
//
