/***********************************************************************
 *
 * $Id: StHbtTTreeXi.h,v 1.0 1999/09/07
 *
 * Authors: Frank Laue, BNL, laue@bnl.gov
 *
 ***********************************************************************/

#ifndef StHbtTTreeXi_h
#define StHbtTTreeXi_h

#include "TObject.h"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtTTreeV0.h"

class StHbtEvent;
class StHbtXi;

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
class StHbtTTreeXi : public StHbtTTreeV0  {
public:
  StHbtTTreeXi(){/* no-op */}
  StHbtTTreeXi(const StHbtEvent*, const StHbtXi* ); // copy constructor
  virtual ~StHbtTTreeXi(){/* no-op */}

  friend class StHbtTTreeReader;
  friend class StHbtXi;
protected:

  int   mCharge;                     // Written out
  float mDecayVertexXiX;
  float mDecayVertexXiY;
  float mDecayVertexXiZ;

  float mDcaXiDaughters;
  float mDcaBachelorToPrimVertex;
  float mDcaXiToPrimVertex;
  float mMomBachelorX;
  float mMomBachelorY;
  float mMomBachelorZ;

  unsigned short mKeyBachelor;
  unsigned int mTopologyMapBachelor[2];

  float mChi2Xi;
  float mClXi;
  float mChi2Bachelor;
  float mClBachelor;

  float mDedxBachelor;
  unsigned short mNumDedxBachelor;

  ClassDef(StHbtTTreeXi,1)
};

#endif

/***********************************************************************
 *
 * $Log: StHbtTTreeXi.h,v $
 * Revision 1.2  2001/12/05 15:10:34  laue
 * Boris' updates (mainly access functions)
 *
 *
 ***********************************************************************/
