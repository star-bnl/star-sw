/***********************************************************************
 *
 * $Id: StXiMiniDst.hh,v 1.1 1999/08/13 12:38:17 jones Exp $
 *
 * Author: Peter G. Jones, University of Birmingham, 30-Mar-1999
 *
 ***********************************************************************
 *
 * Description: Xi (cascade) mini dst class
 *
 ***********************************************************************
 *
 * $Log: StXiMiniDst.hh,v $
 * Revision 1.1  1999/08/13 12:38:17  jones
 * Major revision to merge StV0MiniDstMaker and StXiMiniDstMaker
 *
 *
 ***********************************************************************/
#ifndef StXiMiniDst_hh
#define StXiMiniDst_hh
#include "StV0MiniDst.hh"

class StVertex;
class StV0Vertex;
class StXiVertex;

class StXiMiniDst : public StV0MiniDst {
public:
  StXiMiniDst();
  ~StXiMiniDst();
  StXiMiniDst(StXiVertex*,StV0Vertex*,StVertex*);
  void UpdateXi();

  int   charge() const;
  float decayLengthXi() const;
  float *decayVertexXi() const;

  float dcaXiDaughters() const;
  float dcaBachelorToPrimVertex() const;
  float dcaXiToPrimVertex() const;
  float *momBachelor() const;

  int   tpcHitsBachelor() const;

  float *momXi();
  float alphaXi();
  float ptArmXi();
  float eOmega();
  float eXi();
  float eBachelorPion();
  float eBachelorKaon();
  float massOmega();
  float massXi();
  float rapOmega();
  float rapXi();
  float cTauOmega();
  float cTauXi();
  float ptBachelor();
  float ptotBachelor();
  float ptXi();
  float ptotXi();

protected:
  int   mCharge;
  float mDecayLengthXi;
  float mDecayVertexXi[3];

  float mDcaXiDaughters;
  float mDcaBachelorToPrimVertex;
  float mDcaXiToPrimVertex;
  float mMomBachelor[3];

  int   mTpcHitsBachelor;

  float mMomXi[3];              //! Not to be written out
  float mPtot2Bachelor;         //! Not to be written out
  float mPtot2Xi;               //! Not to be written out 
  float mPt2Xi;                 //! Not to be written out
  float mMomBachelorAlongXi;    //! Not to be written out
  float mMomV0AlongXi;          //! Not to be written out

  ClassDef(StXiMiniDst, 1)
};

inline int   StXiMiniDst::charge() const
             { return mCharge; }
inline float StXiMiniDst::decayLengthXi() const 
             { return mDecayLengthXi; } 
inline float *StXiMiniDst::decayVertexXi() const 
             { return mDecayVertexXi; } 

inline float StXiMiniDst::dcaXiDaughters() const 
             { return mDcaXiDaughters; }
inline float StXiMiniDst::dcaXiToPrimVertex() const 
             { return mDcaXiToPrimVertex; }
inline float StXiMiniDst::dcaBachelorToPrimVertex() const 
             { return mDcaBachelorToPrimVertex; }
inline float *StXiMiniDst::momBachelor() const 
             { return mMomBachelor; }
inline float *StXiMiniDst::momXi()
             { return mMomXi; }

inline int   StXiMiniDst::tpcHitsBachelor() const
             { return mTpcHitsBachelor; }
#endif

