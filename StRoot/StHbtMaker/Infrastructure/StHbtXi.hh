/***********************************************************************
 *
 *  StHbtXi.hh,v 1.0 1999/09/07
 *
 * Authors: Frank Laue
 *
 ***********************************************************************
 *
 * Description: Xi class with members copied from StXihbtXi.hh
 *
 ***********************************************************************/
#ifndef StHbtXi_hh
#define StHbtXi_hh

#include <fstream.h>
#include "StHbtMaker/Infrastructure/StHbtVector.hh" //same as in StHbtTrack.hh
#include "StHbtMaker/Infrastructure/StHbtV0.hh"

#ifdef __ROOT__
#include "StStrangeMuDstMaker/StXiMuDst.hh"
#endif

class StHbtTTreeEvent;
class StHbtTTreeXi;

class StHbtXi : public StHbtV0 {
public:
  StHbtXi(){/* no-op */}
#ifdef __ROOT__
  StHbtXi(StXiMuDst&); // from strangeness Xi micro dst structure
  StHbtXi(const StHbtTTreeEvent*, const StHbtTTreeXi*);
#endif
  ~StHbtXi(){/* no-op */}

  void UpdateXi();
//   friend ostream& operator<<(ostream& out, StHbtXi& v0);
//   friend istream& operator>>(istream& in,  StHbtXi& v0);

   friend class StHbtIOBinary;
   friend class StHbtTTreeXi;

protected:
  int   mCharge;                
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
  unsigned int   mTopologyMapBachelor[2];

  float mChi2Xi;
  float mClXi;
  float mChi2Bachelor;
  float mClBachelor;

  float mDedxBachelor;
  unsigned short mNumDedxBachelor;

  StHbtThreeVector mMomXi;
  float mPtXi;
  float mPtotXi;
  float mAlphaXi;
  float mPtArmXi;
  float mDecayLengthXi;
};


#endif


/***********************************************************************
 *
 * $Log: StHbtXi.hh,v $
 * Revision 1.1  2001/09/05 20:41:43  laue
 * Updates of the hbtMuDstTree microDSTs
 *
 *
 ***********************************************************************/
















