/***********************************************************************
 *
 * $Id: StXiMiniDst.hh,v 1.4 1999/09/02 09:04:57 jones Exp $
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
 * Revision 1.4  1999/09/02 09:04:57  jones
 * Added StEvMiniDst class, New file handling, Partially implemented TTrees
 *
 * Revision 1.3  1999/08/16 10:21:03  jones
 * Fixed compilation problem on HP related to use of const on pointer type
 *
 * Revision 1.2  1999/08/13 14:28:20  jones
 * Added comments to explain the usage of the public member functions
 *
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
  StXiMiniDst(StXiVertex*,StV0Vertex*,StEvMiniDst*);
  void UpdateXi();

  int   charge() const;              // Particle charge
  float decayLengthXi() const;       // 3-d decay distance
  float *decayVertexXi();            // Coordinate of decay vertex
  float dcaXiDaughters() const;      // DCA of xi daughters at decay vertex
  float dcaBachelorToPrimVertex() const; // DCA of bachelor to primary vertex
  float dcaXiToPrimVertex() const;   // DCA of xi to primary vertex
  float *momBachelor();              // Momentum components of bachelor
  int   tpcHitsBachelor() const;     // Number of TPC hits on bachelor

  float *momXi();                    // Momentum components of Xi/Omega
  float alphaXi();                   // Armenteros-Podolanski variable
  float ptArmXi();                   // Armenteros-Podolanski variable
  float eOmega();                    // Energy assuming Omega hypothesis
  float eXi();                       // Energy assuming Xi hypothesis
  float eBachelorPion();             // Energy of bachelor assuming pion
  float eBachelorKaon();             // Energy of bachelor assuming kaon
  float massOmega();                 // Mass assuming Omega hypothesis
  float massXi();                    // Mass assuming Xi hypothesis
  float rapOmega();                  // Rapidity assuming (anti)Omega
  float rapXi();                     // Rapidity assuming (anti)Xi
  float cTauOmega();                 // Lifetime (ctau) assuming (anti)Omega
  float cTauXi();                    // Lifetime (ctau) assuming (anti)Xi
  float ptBachelor();                // Transverse momentum of bachelor
  float ptotBachelor();              // Total momentum of bachelor
  float ptXi();                      // Transverse momentum of Xi/Omega
  float ptotXi();                    // Total momentum of Xi/Omega

protected:
  int   mCharge;                     // Written out
  float mDecayLengthXi;
  float mDecayVertexXi[3];

  float mDcaXiDaughters;
  float mDcaBachelorToPrimVertex;
  float mDcaXiToPrimVertex;
  float mMomBachelor[3];

  int   mTpcHitsBachelor;

  float mMomXi[3];                   //! Not to be written out
  float mPtot2Bachelor;              //! Not to be written out
  float mPtot2Xi;                    //! Not to be written out 
  float mPt2Xi;                      //! Not to be written out
  float mMomBachelorAlongXi;         //! Not to be written out
  float mMomV0AlongXi;               //! Not to be written out

  ClassDef(StXiMiniDst, 1)
};

inline int   StXiMiniDst::charge() const
             { return mCharge; }
inline float StXiMiniDst::decayLengthXi() const 
             { return mDecayLengthXi; } 
inline float *StXiMiniDst::decayVertexXi()
             { return mDecayVertexXi; } 

inline float StXiMiniDst::dcaXiDaughters() const 
             { return mDcaXiDaughters; }
inline float StXiMiniDst::dcaXiToPrimVertex() const 
             { return mDcaXiToPrimVertex; }
inline float StXiMiniDst::dcaBachelorToPrimVertex() const 
             { return mDcaBachelorToPrimVertex; }
inline float *StXiMiniDst::momBachelor() 
             { return mMomBachelor; }
inline float *StXiMiniDst::momXi()
             { return mMomXi; }

inline int   StXiMiniDst::tpcHitsBachelor() const
             { return mTpcHitsBachelor; }
#endif

