/***********************************************************************
 *
 * $Id: StV0MuDst.hh,v 1.3 2000/03/29 20:52:13 genevb Exp $
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
#include "TObject.h"

class StVertex;
class StV0Vertex;
class StStrangeEvMuDst;

class StV0MuDst : public TObject {
public:
  StV0MuDst();
  ~StV0MuDst();
  StV0MuDst(StV0Vertex*,StStrangeEvMuDst*);
  void    Clear();
  void    Fill(StV0Vertex*,StStrangeEvMuDst*);
  void    SetEvent(StStrangeEvMuDst*);

  StStrangeEvMuDst *event();           // Pointer to event information

  Float_t decayLengthV0();             // 3-d decay distance
  Float_t decayVertexV0(Int_t n);      // Coordinates of decay vertex
  Float_t dcaV0Daughters() const;      // DCA of v0 daughters at decay vertex
  Float_t dcaV0ToPrimVertex() const;   // DCA of v0 to primary vertex
  Float_t dcaPosToPrimVertex() const;  // DCA of pos v0 daughter to pri vertex
  Float_t dcaNegToPrimVertex() const;  // DCA of neg v0 daughter to pri vertex
  Float_t momPos(Int_t n);             // Momentum components of pos. daughter
  Float_t momNeg(Int_t n);             // Momentum components of neg. daughter

  Int_t   tpcHitsPos() const;          // Number of TPC hits on pos. daughter
  Int_t   tpcHitsNeg() const;          // Number of TPC hits on neg. daughter

  Float_t momV0(Int_t n);         // Momentum components of V0
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

protected:
  StStrangeEvMuDst *mEvent;           //!

  Float_t mDecayVertexV0X;            // These are written out
  Float_t mDecayVertexV0Y;            // These are written out
  Float_t mDecayVertexV0Z;            // These are written out
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

  Int_t   mTpcHitsPos;
  Int_t   mTpcHitsNeg;

  Float_t Ptot2Pos();          
  Float_t Ptot2Neg();             
  Float_t Ptot2V0();            
  Float_t Pt2V0();  
  Float_t MomPosAlongV0();
  Float_t MomNegAlongV0();

  ClassDef(StV0MuDst, 1)
};

inline StV0MuDst::StV0MuDst(StV0Vertex* v1,StStrangeEvMuDst* e1)
             { Fill(v1,e1); }
inline void StV0MuDst::SetEvent(StStrangeEvMuDst* ev)
             { mEvent = ev; }
inline StStrangeEvMuDst *StV0MuDst::event()
             { return mEvent; }
inline Float_t StV0MuDst::dcaV0Daughters() const 
             { return mDcaV0Daughters; }
inline Float_t StV0MuDst::dcaV0ToPrimVertex() const 
             { return mDcaV0ToPrimVertex; }
inline Float_t StV0MuDst::dcaPosToPrimVertex() const 
             { return mDcaPosToPrimVertex; }
inline Float_t StV0MuDst::dcaNegToPrimVertex() const 
             { return mDcaNegToPrimVertex; }
inline Int_t   StV0MuDst::tpcHitsPos() const
             { return mTpcHitsPos; }
inline Int_t   StV0MuDst::tpcHitsNeg() const
             { return mTpcHitsNeg; }
#endif
