/***********************************************************************
 *
 * $Id: StV0MuDst.hh,v 2.0 2000/06/02 22:11:54 genevb Exp $
 *
 * Authors: Gene Van Buren, UCLA, 24-Mar-2000
 *          Peter G. Jones, University of Birmingham, 04-Jun-1999
 *
 ***********************************************************************
 *
 * Description: V0 micro dst class
 *
 ***********************************************************************
 *
 * $Log: StV0MuDst.hh,v $
 * Revision 2.0  2000/06/02 22:11:54  genevb
 * New version of Strangeness micro DST package
 *
 * Revision 1.4  2000/03/31 03:20:24  jones
 * Added topology map to V0/Xi; access funcs for each data member
 *
 * Revision 1.3  2000/03/29 20:52:13  genevb
 * Added StKinkMuDst, replaced arrays
 *
 * Revision 1.2  2000/03/29 14:42:40  genevb
 * Removed StV0MiniMiniDst
 *
 * Revision 1.1  2000/03/29 03:10:08  genevb
 * Introduction of Strangeness Micro DST package
 *
 *
 ***********************************************************************/
#ifndef StV0MuDst_hh
#define StV0MuDst_hh
#include "StStrangeMuDst.hh"

#ifndef StTrackTopologyMap_hh
#include "StEvent/StTrackTopologyMap.h"
#endif

class StVertex;
class StV0Vertex;
class StStrangeEvMuDst;

class StV0MuDst : public StStrangeMuDst {
public:
  StV0MuDst();
  ~StV0MuDst();
  StV0MuDst(StV0Vertex*,StStrangeEvMuDst*);
  void    Clear();
  void    Fill(StV0Vertex*,StStrangeEvMuDst*);
  void    SetEvent(StStrangeEvMuDst*);

  StStrangeEvMuDst *event();           // Pointer to event information

  virtual Float_t decayLengthV0() const;  // 3-d decay distance
  Float_t decayVertexV0X() const;      // Coordinates of decay vertex
  Float_t decayVertexV0Y() const;
  Float_t decayVertexV0Z() const;
  Float_t dcaV0Daughters() const;      // DCA of v0 daughters at decay vertex
  Float_t dcaV0ToPrimVertex()  const;  // DCA of v0 to primary vertex
  Float_t dcaPosToPrimVertex() const;  // DCA of pos v0 daughter to pri vertex
  Float_t dcaNegToPrimVertex() const;  // DCA of neg v0 daughter to pri vertex
  Float_t momPosX() const;             // Momentum components of pos. daughter
  Float_t momPosY() const;
  Float_t momPosZ() const;
  Float_t momNegX() const;             // Momentum components of neg. daughter
  Float_t momNegY() const;
  Float_t momNegZ() const;
  StTrackTopologyMap& topologyMapPos();
  StTrackTopologyMap& topologyMapNeg();
  UShort_t keyPos() const;             // Track id v0 daughters
  UShort_t keyNeg() const;             // Track id v0 daughters

  Float_t momV0X() const;         // Momentum components of V0
  Float_t momV0Y() const;
  Float_t momV0Z() const;
  Float_t alphaV0();              // Armenteros-Podolanski variable
  Float_t ptArmV0();              // Armenteros-Podolanski variable
  Float_t eLambda();              // Energy assuming lambda hypothesis
  Float_t eK0Short();             // Energy assuming k-short hypothesis
  Float_t ePosProton();           // Energy of pos. daughter assuming proton
  Float_t ePosPion();             // Energy of pos. daughter assuming pion
  Float_t eNegProton();           // Energy of neg. daughter assuming antiproton
  Float_t eNegPion();             // Energy of neg. daughter assuming pion
  Float_t massLambda();           // Mass assuming lambda hypothesis
  Float_t massAntiLambda();       // Mass assuming antilambda hypothesis
  Float_t massK0Short();          // Mass assuming k-short hypothesis
  Float_t rapLambda();            // Rapidity assuming (anti)lambda
  Float_t rapK0Short();           // Rapidity assuming k-short
  Float_t cTauLambda();           // Lifetime (ctau) assuming (anti)lambda
  Float_t cTauK0Short();          // Lifetime (ctau) assuming k-short
  Float_t ptV0();                 // Transverse momentum
  Float_t ptotV0();               // Total momentum
  Float_t ptPos();                // Transverse momentum of pos. daughter
  Float_t ptotPos();              // Total momentum of pos. daughter
  Float_t ptNeg();                // Transverse momentum of neg. daughter
  Float_t ptotNeg();              // Total momentum of neg. daughter  
  Float_t chi2V0()  const;        // Chi square of V0
  Float_t clV0()    const;        // Confidence level of V0
  Float_t chi2Pos() const;        // Chi square of pos. daughter
  Float_t clPos()   const;        // Confidence level of pos. daughter
  Float_t chi2Neg() const;        // Chi square of neg. daughter
  Float_t clNeg()   const;        // Confidence level of neg. daughter
  Long_t  detectorIdV0();         // Detector ID for V0 Vertex
  virtual Long_t detectorIdPars();// Detector ID for pars used in V0 finder

protected:
  StStrangeEvMuDst *mEvent;       //!

  Float_t mDecayVertexV0X;        // These are written out
  Float_t mDecayVertexV0Y;
  Float_t mDecayVertexV0Z;
  Float_t mDcaV0Daughters;
  Float_t mDcaV0ToPrimVertex;
  Float_t mDcaPosToPrimVertex;
  Float_t mDcaNegToPrimVertex;
  Float_t mMomPosX;
  Float_t mMomPosY;
  Float_t mMomPosZ;
  Float_t mMomNegX;
  Float_t mMomNegY;
  Float_t mMomNegZ;

  UShort_t mKeyPos;
  UShort_t mKeyNeg;

  StTrackTopologyMap mTopologyMapPos;
  StTrackTopologyMap mTopologyMapNeg;

  Float_t mChi2V0;
  Float_t mClV0;
  Float_t mChi2Pos;
  Float_t mClPos;
  Float_t mChi2Neg;
  Float_t mClNeg;

  Float_t Ptot2Pos();          
  Float_t Ptot2Neg();             
  Float_t Ptot2V0();            
  Float_t Pt2V0();  
  Float_t MomPosAlongV0();
  Float_t MomNegAlongV0();

  Long_t detectorIdTrack(StTrackTopologyMap&);

  ClassDef(StV0MuDst, 2)
};

inline StV0MuDst::StV0MuDst(StV0Vertex* v1,StStrangeEvMuDst* e1)
             { Fill(v1,e1); }
inline void StV0MuDst::SetEvent(StStrangeEvMuDst* ev)
             { mEvent = ev; }
inline StStrangeEvMuDst *StV0MuDst::event()
             { return mEvent; }
inline Float_t StV0MuDst::decayVertexV0X() const { return mDecayVertexV0X; }
inline Float_t StV0MuDst::decayVertexV0Y() const { return mDecayVertexV0Y; }
inline Float_t StV0MuDst::decayVertexV0Z() const { return mDecayVertexV0Z; }
inline Float_t StV0MuDst::dcaV0Daughters() const 
             { return mDcaV0Daughters; }
inline Float_t StV0MuDst::dcaV0ToPrimVertex() const 
             { return mDcaV0ToPrimVertex; }
inline Float_t StV0MuDst::dcaPosToPrimVertex() const 
             { return mDcaPosToPrimVertex; }
inline Float_t StV0MuDst::dcaNegToPrimVertex() const 
             { return mDcaNegToPrimVertex; }
inline Float_t StV0MuDst::momPosX() const { return mMomPosX; }
inline Float_t StV0MuDst::momPosY() const { return mMomPosY; }
inline Float_t StV0MuDst::momPosZ() const { return mMomPosZ; }
inline Float_t StV0MuDst::momNegX() const { return mMomNegX; }
inline Float_t StV0MuDst::momNegY() const { return mMomNegY; }
inline Float_t StV0MuDst::momNegZ() const { return mMomNegZ; }
inline StTrackTopologyMap& StV0MuDst::topologyMapPos()
             { return mTopologyMapPos; }
inline StTrackTopologyMap& StV0MuDst::topologyMapNeg()
             { return mTopologyMapNeg; }
inline UShort_t StV0MuDst::keyPos() const { return mKeyPos; } 
inline UShort_t StV0MuDst::keyNeg() const { return mKeyNeg; } 
inline Float_t StV0MuDst::momV0X()  const { return mMomPosX + mMomNegX; }
inline Float_t StV0MuDst::momV0Y()  const { return mMomPosY + mMomNegY; }
inline Float_t StV0MuDst::momV0Z()  const { return mMomPosZ + mMomNegZ; }
inline Float_t StV0MuDst::chi2V0()  const { return mChi2V0; }
inline Float_t StV0MuDst::clV0()    const { return mClV0; }
inline Float_t StV0MuDst::chi2Pos() const { return mChi2Pos; }
inline Float_t StV0MuDst::clPos()   const { return mClPos; }
inline Float_t StV0MuDst::chi2Neg() const { return mChi2Neg; }
inline Float_t StV0MuDst::clNeg()   const { return mClNeg; }
#endif
