/*!
 * \class StKinkMuDst
 * \author Wensheng Deng, Kent State University, 29-Mar-2000
 * \author Gene Van Buren
 *
 *              Kink micro dst class
 *
 */

#ifndef  STAR_StKinkMuDst
#define  STAR_StKinkMuDst
#include "StKinkBase.hh"

class StKinkVertex;

class StKinkMuDst : public StKinkBase {  // StKinkBase inherits StKinkI methods
  friend class StMuMomentumShiftMaker;

public:
  StKinkMuDst();
  StKinkMuDst(StKinkVertex*);
  ~StKinkMuDst();

  Float_t  dcaParentDaughter() const;
  Float_t  dcaDaughterPrimaryVertex() const;
  Float_t  dcaParentPrimaryVertex() const;

  Float_t  hitDistanceParentDaughter() const;
  Float_t  hitDistanceParentVertex() const;
  Float_t  mindE() const;
  Float_t  decayAngle() const;
  Float_t  parentMomentum() const;
  Float_t  parentPrimMomentum() const;
  Int_t    parentCharge() const;
  Float_t  daughterMomentum() const;
  Int_t    daughterCharge() const;
  Float_t  decayLength() const;
  Float_t  transverseMomentum() const;
  Float_t  transverseMassKaon() const;
  Float_t  transverseMassPion() const;
  Float_t  rapidityKaon() const;
  Float_t  rapidityPion() const;
  Int_t    keyParent() const;          // Track id of parent
  Int_t    keyDaughter() const;        // Track id of daughter

  Float_t  chi2Kink()     const;       // Chi square of Kink
  Float_t  clKink()       const;       // Confidence level of Kink
  Float_t  chi2Parent()   const;       // Chi square of parent
  Float_t  clParent()     const;       // Confidence level of parent
  Float_t  chi2Daughter() const;       // Chi square of daughter
  Float_t  clDaughter()   const;       // Confidence level of daughter
  void     setParentBad();             // Set the parent as bad
  void     setDaughterBad();           // Set the daughter as bad
  Float_t  dedxParent()   const;       // dE/dX of parent
  Float_t  dedxDaughter() const;       // dE/dX of daughter
  Float_t  errDedxParent()   const;    // Error on mean of dE/dX of parent
  Float_t  errDedxDaughter() const;    // Error on mean of dE/dX of daughter
  UShort_t numDedxParent()   const;    // Number of dE/dX points for parent
  UShort_t numDedxDaughter() const;    // Number of dE/dX points for daughter
  Float_t  lenDedxParent()   const;    // Length of dE/dX track of parent
  Float_t  lenDedxDaughter() const;    // Length of dE/dX track of daughter

  Float_t  mDcaParentDaughter;
  Float_t  mDcaDaughterPrimaryVertex;
  Float_t  mDcaParentPrimaryVertex;
  Float_t  mHitDistanceParentDaughter;
  Float_t  mHitDistanceParentVertex;
  Float_t  mMinDeltaEnergy;
  Float_t  mDecayAngle;
  Float_t  mParentMomentum;
  Float_t  mParentPrimMomentum;
  Int_t    mParentCharge;
  Float_t  mDaughterMomentum;
  Int_t    mDaughterCharge;
  Float_t  mDecayLength;
  Float_t  mTransverseMomentum;
  Float_t  mTransverseMassKaon;
  Float_t  mTransverseMassPion;
  Float_t  mRapidityKaon;  
  Float_t  mRapidityPion;
  Float_t  mChi2Kink;
  Float_t  mClKink;
  Float_t  mChi2Parent;
  Float_t  mClParent;
  Float_t  mChi2Daughter;
  Float_t  mClDaughter;
  Float_t  mDedxParent;
  Float_t  mDedxDaughter;
  Float_t  mErrDedxParent;
  Float_t  mErrDedxDaughter;
  UShort_t mNumDedxParent;
  UShort_t mNumDedxDaughter;
  Int_t    mKeyParent;
  Int_t    mKeyDaughter;

private:
  void     findMinDeltaEnergy(StKinkVertex*);
  void     findDecayLength(StKinkVertex*);
  void     findTransverseMomentum();
  void     findTransverseMassKaon();
  void     findTransverseMassPion();
  void     findRapidityKaon();
  void     findRapidityPion();
  ClassDef(StKinkMuDst,9)
};

inline Float_t StKinkMuDst::dcaParentDaughter() const
               { return mDcaParentDaughter; }
inline Float_t StKinkMuDst::dcaDaughterPrimaryVertex() const
               { return mDcaDaughterPrimaryVertex; }
inline Float_t StKinkMuDst::dcaParentPrimaryVertex() const
               { return mDcaParentPrimaryVertex; }
inline Float_t StKinkMuDst::hitDistanceParentDaughter() const
               { return mHitDistanceParentDaughter; }
inline Float_t StKinkMuDst::hitDistanceParentVertex() const 
               { return mHitDistanceParentVertex; }
inline Float_t StKinkMuDst::mindE() const { return mMinDeltaEnergy; }
inline Float_t StKinkMuDst::decayAngle() const { return mDecayAngle; }
inline Float_t StKinkMuDst::parentMomentum() const
               { return mParentMomentum; }
inline Float_t StKinkMuDst::parentPrimMomentum() const
               { return mParentPrimMomentum; }
inline Int_t   StKinkMuDst::parentCharge() const
               { return mParentCharge; }
inline Float_t StKinkMuDst::daughterMomentum() const
               { return mDaughterMomentum; }
inline Int_t   StKinkMuDst::daughterCharge() const
               { return mDaughterCharge; }
inline Float_t StKinkMuDst::decayLength() const { return mDecayLength; } 
inline Float_t StKinkMuDst::transverseMomentum() const
               { return mTransverseMomentum; }
inline Float_t StKinkMuDst::transverseMassKaon() const
               { return mTransverseMassKaon; }
inline Float_t StKinkMuDst::transverseMassPion() const
               { return mTransverseMassPion; }
inline Float_t StKinkMuDst::rapidityKaon() const { return mRapidityKaon; }
inline Float_t StKinkMuDst::rapidityPion() const { return mRapidityPion; }
inline Float_t StKinkMuDst::chi2Kink()     const { return mChi2Kink; }
inline Float_t StKinkMuDst::clKink()       const { return mClKink; }
inline Float_t StKinkMuDst::chi2Parent()   const { return mChi2Parent; }
inline Float_t StKinkMuDst::clParent()     const { return mClParent; }
inline Float_t StKinkMuDst::chi2Daughter() const { return mChi2Daughter; }
inline Float_t StKinkMuDst::clDaughter()   const { return mClDaughter; }
inline void StKinkMuDst::setParentBad() { mChi2Parent = -TMath::Abs(mChi2Parent); }
inline void StKinkMuDst::setDaughterBad() { mChi2Daughter = -TMath::Abs(mChi2Daughter); }
inline Float_t StKinkMuDst::dedxParent()   const { return mDedxParent; }
inline Float_t StKinkMuDst::dedxDaughter() const { return mDedxDaughter; }
inline Float_t StKinkMuDst::errDedxParent()   const { return mErrDedxParent; }
inline Float_t StKinkMuDst::errDedxDaughter() const { return mErrDedxDaughter; }
inline UShort_t StKinkMuDst::numDedxParent()   const
               { return (mNumDedxParent%100); }
inline UShort_t StKinkMuDst::numDedxDaughter() const
               { return (mNumDedxDaughter%100); }
inline Float_t StKinkMuDst::lenDedxParent()   const
               { return (mNumDedxParent/100); }
inline Float_t StKinkMuDst::lenDedxDaughter() const
               { return (mNumDedxDaughter/100); }
inline Int_t StKinkMuDst::keyParent() const { return mKeyParent; }
inline Int_t StKinkMuDst::keyDaughter() const { return mKeyDaughter; }
#endif


/***********************************************************************
 * $Id: StKinkMuDst.hh,v 3.13 2011/05/27 18:25:32 genevb Exp $
 * $Log: StKinkMuDst.hh,v $
 * Revision 3.13  2011/05/27 18:25:32  genevb
 * Propagate StTrack::key => Int_t to other codes
 *
 * Revision 3.12  2008/07/11 16:23:08  genevb
 * bad() won't work unless chi2 allows to return negative values
 *
 * Revision 3.11  2008/07/10 16:16:55  genevb
 * Allow for marking of bad tracks -> bad secondary vertices
 *
 * Revision 3.10  2005/03/17 05:02:20  genevb
 * Add StMuMomentumShiftMaker friend
 *
 * Revision 3.9  2004/02/03 20:09:07  genevb
 * Update ClassDef version for added data members
 *
 * Revision 3.8  2004/02/03 03:49:27  genevb
 * Added keys (IDs) for Kink parent and daughter
 *
 * Revision 3.7  2003/06/01 04:25:19  genevb
 * Update ClassDef version for altered inheritance
 *
 * Revision 3.6  2003/05/30 21:20:19  genevb
 * doxygen savvy, encoding of FTPC mults, change virtual funcs
 *
 * Revision 3.5  2002/04/30 16:02:47  genevb
 * Common muDst, improved MC code, better kinks, StrangeCuts now a branch
 *
 * Revision 3.4  2001/11/05 23:41:06  genevb
 * Add more dEdx, B field info, careful of changes to TTree unrolling
 *
 * Revision 3.3  2001/05/04 20:15:14  genevb
 * Common interfaces and reorganization of components, add MC event info
 *
 * Revision 3.2  2000/09/06 21:09:03  wdeng
 * Added track charges and total momenta
 *
 * Revision 3.1  2000/08/10 01:16:24  genevb
 * Added number of dedx points
 *
 * Revision 3.0  2000/07/14 12:56:48  genevb
 * Revision 3 has event multiplicities and dedx information for vertex tracks
 *
 * Revision 2.1  2000/06/09 22:17:10  genevb
 * Allow MC data to be copied between DSTs, other small improvements
 *
 * Revision 2.0  2000/06/02 22:11:54  genevb
 * New version of Strangeness micro DST package
 *
 * Revision 1.1  2000/03/30 00:18:08  genevb
 * Introduction of StKinkMuDst
 *
 ***********************************************************************/
