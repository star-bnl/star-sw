/***********************************************************************
 *
 * $Id: StV0MiniDst.hh,v 1.7 1999/09/24 01:23:32 fisyak Exp $
 *
 * Author: Peter G. Jones, University of Birmingham, 04-Jun-1999
 *
 ***********************************************************************
 *
 * Description: V0 mini dst class
 *
 ***********************************************************************
 *
 * $Log: StV0MiniDst.hh,v $
 * Revision 1.7  1999/09/24 01:23:32  fisyak
 * Reduced Include Path
 *
 * Revision 1.6  1999/09/02 09:04:56  jones
 * Added StEvMiniDst class, New file handling, Partially implemented TTrees
 *
 * Revision 1.5  1999/08/13 12:38:16  jones
 * Major revision to merge StV0MiniDstMaker and StXiMiniDstMaker
 *
 * Revision 1.4  1999/08/03 02:31:45  genevb
 * Better implementation of StHFillObject
 *
 * Revision 1.3  1999/07/30 15:01:13  genevb
 * Switched from TObject to StHFillObject inheritance
 *
 * Revision 1.2  1999/07/26 19:17:25  jones
 * Added primary vertex position and v0 daughter DCA to the primary vertex
 *
 * Revision 1.1  1999/07/13 12:42:24  jones
 * *** empty log message ***
 *
 *
 ***********************************************************************/
#ifndef StV0MiniDst_hh
#define StV0MiniDst_hh
#include "StHFillObject.h"
#include "StEvMiniDst.hh"
#include "TClonesArray.h"

class StVertex;
class StV0Vertex;

class StV0MiniDst : public StHFillObject {
public:
  StV0MiniDst();
  ~StV0MiniDst();
  StV0MiniDst(StV0Vertex*,StEvMiniDst*);
  void UpdateV0();

  StEvMiniDst *event();         // Pointer to event information

  float decayLengthV0() const;       // 3-d decay distance
  float *decayVertexV0();            // Coordinates of decay vertex
  float dcaV0Daughters() const;      // DCA of v0 daughters at decay vertex
  float dcaV0ToPrimVertex() const;   // DCA of v0 to primary vertex
  float dcaPosToPrimVertex() const;  // DCA of pos v0 daughter to pri vertex
  float dcaNegToPrimVertex() const;  // DCA of neg v0 daughter to pri vertex
  float *momPos();                   // Momentum components of pos. daughter
  float *momNeg();                   // Momentum components of neg. daughter

  int   tpcHitsPos() const;          // Number of TPC hits on pos. daughter
  int   tpcHitsNeg() const;          // Number of TPC hits on neg. daughter

  float *momV0();               // Momentum components of V0
  float alphaV0();              // Armenteros-Podolanski variable
  float ptArmV0();              // Armenteros-Podolanski variable
  float eLambda();              // Energy assuming lambda hypothesis
  float eK0Short();             // Energy assuming k-short hypothesis
  float ePosProton();           // Energy of pos. daughter assuming proton
  float ePosPion();             // Energy of pos. daughter assuming pion
  float eNegProton();           // Energy of neg. daughter assuming antiproton
  float eNegPion();             // Energy of neg. daughter assuming pion
  float massLambda();           // Mass assuming lambda hypothesis
  float massAntiLambda();       // Mass assuming antilambda hypothesis
  float massK0Short();          // Mass assuming k-short hypothesis
  float rapLambda();            // Rapidity assuming (anti)lambda
  float rapK0Short();           // Rapidity assuming k-short
  float cTauLambda();           // Lifetime (ctau) assuming (anti)lambda
  float cTauK0Short();          // Lifetime (ctau) assuming k-short
  float ptV0();                 // Transverse momentum
  float ptotV0();               // Total momentum
  float ptPos();                // Transverse momentum of pos. daughter
  float ptotPos();              // Total momentum of pos. daughter
  float ptNeg();                // Transverse momentum of neg. daughter
  float ptotNeg();              // Total momentum of neg. daughter  

protected:
  StEvMiniDst *mEvent;          // These are written out

  float mDecayVertexV0[3];
  float mDcaV0Daughters;
  float mDcaV0ToPrimVertex;
  float mDcaPosToPrimVertex;
  float mDcaNegToPrimVertex;
  float mMomPos[3];
  float mMomNeg[3];

  int   mTpcHitsPos;
  int   mTpcHitsNeg;

  float mDecayLengthV0;         //! Not to be written out
  float mMomV0[3];              //! Not to be written out
  float mPtot2Pos;              //! Not to be written out
  float mPtot2Neg;              //! Not to be written out
  float mPtot2V0;               //! Not to be written out
  float mPt2V0;                 //! Not to be written out
  float mMomPosAlongV0;         //! Not to be written out
  float mMomNegAlongV0;         //! Not to be written out

  ClassDef(StV0MiniDst, 1)
};

class StV0MiniMiniDst: public TObject {
public:
protected:
  TClonesArray *array;
  ClassDef(StV0MiniMiniDst, 1)
};

inline StEvMiniDst *StV0MiniDst::event()
             { return mEvent; }

inline float StV0MiniDst::decayLengthV0() const 
             { return mDecayLengthV0; }
inline float *StV0MiniDst::decayVertexV0()
             { return mDecayVertexV0; } 

inline float StV0MiniDst::dcaV0Daughters() const 
             { return mDcaV0Daughters; }
inline float StV0MiniDst::dcaV0ToPrimVertex() const 
             { return mDcaV0ToPrimVertex; }
inline float StV0MiniDst::dcaPosToPrimVertex() const 
             { return mDcaPosToPrimVertex; }
inline float StV0MiniDst::dcaNegToPrimVertex() const 
             { return mDcaNegToPrimVertex; }
inline float *StV0MiniDst::momPos()
             { return mMomPos; }
inline float *StV0MiniDst::momNeg()
             { return mMomNeg; }
inline float *StV0MiniDst::momV0()
             { return mMomV0; }

inline int   StV0MiniDst::tpcHitsPos() const
             { return mTpcHitsPos; }
inline int   StV0MiniDst::tpcHitsNeg() const
             { return mTpcHitsNeg; }
#endif
