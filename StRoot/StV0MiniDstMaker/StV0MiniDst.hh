/***********************************************************************
 *
 * $Id: StV0MiniDst.hh,v 1.4 1999/08/03 02:31:45 genevb Exp $
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

class StVertex;
class StV0Vertex;

class StV0MiniDst : public StHFillObject {
public:
  StV0MiniDst();
  ~StV0MiniDst();
  StV0MiniDst(StV0Vertex*,StVertex*);
  void Update();

  int run() const;              // Run number
  int event() const;            // Event number
  float *primVertex();          // Primary Vertex Position

  float decayDistance() const;          // 3-d decay distance
  float *position();                    // Coordinates of decay vertex
  float dcaDaughters() const;           // DCA of daughters at decay vertex
  float dcaParentToPrimVertex() const;  // DCA of v0 to primary vertex
  float dcaPosToPrimVertex() const;     // DCA of pos v0 daughter to pri vertex
  float dcaNegToPrimVertex() const;     // DCA of neg v0 daughter to pri vertex
  float *momPosDaughter();              // Momentum components of pos. daughter
  float *momNegDaughter();              // Momentum components of neg. daughter

  float alpha();                // Armenteros-Podolanski variable
  float ptArm();                // Armenteros-Podolanski variable
  float eLambda();              // Energy assuming lambda hypothesis
  float eK0Short();             // Energy assuming k-short hypothesis
  float ePosDaughterProton();   // Energy of pos. daughter assuming proton
  float ePosDaughterPion();     // Energy of pos. daughter assuming pion
  float eNegDaughterProton();   // Energy of neg. daughter assuming antiproton
  float eNegDaughterPion();     // Energy of neg. daughter assuming pion
  float massLambda();           // Mass assuming lambda hypothesis
  float massAntiLambda();       // Mass assuming antilambda hypothesis
  float massK0Short();          // Mass assuming k-short hypothesis
  float rapLambda();            // Rapidity assuming (anti)lambda
  float rapK0Short();           // Rapidity assuming k-short
  float cTauLambda();           // Lifetime (ctau) assuming (anti)lambda
  float cTauK0Short();          // Lifetime (ctau) assuming k-short
  float pt();                   // Transverse momentum
  float ptot();                 // Total momentum
  float ptPosDaughter();        // Transverse momentum of pos. daughter
  float ptotPosDaughter();      // Total momentum of pos. daughter
  float ptNegDaughter();        // Transverse momentum of neg. daughter
  float ptotNegDaughter();      // Total momentum of neg. daughter  

protected:
  int   mRun;                   // These are written out
  int   mEvent;
  float mPrimVertex[3];

  float mPosition[3];
  float mDcaDaughters;
  float mDcaParentToPrimVertex;
  float mDcaPosToPrimVertex;
  float mDcaNegToPrimVertex;
  float mMomPosDaughter[3];
  float mMomNegDaughter[3];

  float mDecayDistance;         //! Not to be written out
  float mMomV0[3];              //! Not to be written out
  float mPtot2PosDaughter;      //! Not to be written out
  float mPtot2NegDaughter;      //! Not to be written out
  float mPtot2;                 //! Not to be written out
  float mPt2;                   //! Not to be written out
  float mPx;                    //! Not to be written out
  float mPy;                    //! Not to be written out
  float mPz;                    //! Not to be written out
  float mMomPosAlongV0;         //! Not to be written out
  float mMomNegAlongV0;         //! Not to be written out

  ClassDef(StV0MiniDst, 1)
};

inline int   StV0MiniDst::run() const
             { return mRun; }
inline int   StV0MiniDst::event() const
             { return mEvent; }
inline float *StV0MiniDst::primVertex()
             { return mPrimVertex; }

inline float StV0MiniDst::decayDistance() const 
             { return mDecayDistance; }
inline float *StV0MiniDst::position()
             { return mPosition; } 

inline float StV0MiniDst::dcaDaughters() const 
             { return mDcaDaughters; }
inline float StV0MiniDst::dcaParentToPrimVertex() const 
             { return mDcaParentToPrimVertex; }
inline float StV0MiniDst::dcaPosToPrimVertex() const 
             { return mDcaPosToPrimVertex; }
inline float StV0MiniDst::dcaNegToPrimVertex() const 
             { return mDcaNegToPrimVertex; }
inline float *StV0MiniDst::momPosDaughter()
             { return mMomPosDaughter; }
inline float *StV0MiniDst::momNegDaughter()
             { return mMomNegDaughter; }

#endif
