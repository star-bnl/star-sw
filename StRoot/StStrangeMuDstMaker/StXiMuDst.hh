/***********************************************************************
 *
 * $Id: StXiMuDst.hh,v 1.1 2000/03/29 03:10:08 genevb Exp $
 *
 * Authors: Gene Van Buren, UCLA, 24-Mar-2000
 *          Peter G. Jones, University of Birmingham, 30-Mar-1999
 *
 ***********************************************************************
 *
 * Description: Xi (cascade) micro dst class
 *
 ***********************************************************************
 *
 * $Log: StXiMuDst.hh,v $
 * Revision 1.1  2000/03/29 03:10:08  genevb
 * Introduction of Strangeness Micro DST package
 *
 *
 ***********************************************************************/
#ifndef StXiMuDst_hh
#define StXiMuDst_hh
#include "StV0MuDst.hh"

class StVertex;
class StV0Vertex;
class StXiVertex;

class StXiMuDst : public StV0MuDst {
public:
  StXiMuDst();
  ~StXiMuDst();
  StXiMuDst(StXiVertex* x1,StV0Vertex* v1,StStrangeEvMuDst* e1);
  void    Clear();
  void    Fill(StXiVertex*,StV0Vertex*,StStrangeEvMuDst*);

  Int_t   charge() const;              // Particle charge
  Float_t decayLengthXi();             // 3-d decay distance
  Float_t *decayVertexXi();            // Coordinate of decay vertex
  Float_t dcaXiDaughters() const;      // DCA of xi daughters at decay vertex
  Float_t dcaBachelorToPrimVertex() const; // DCA of bachelor to primary vertex
  Float_t dcaXiToPrimVertex() const;   // DCA of xi to primary vertex
  Float_t *momBachelor();              // Momentum components of bachelor
  Int_t   tpcHitsBachelor() const;     // Number of TPC hits on bachelor

  Float_t *momXi();                    // Momentum components of Xi/Omega
  Float_t alphaXi();                   // Armenteros-Podolanski variable
  Float_t ptArmXi();                   // Armenteros-Podolanski variable
  Float_t eOmega();                    // Energy assuming Omega hypothesis
  Float_t eXi();                       // Energy assuming Xi hypothesis
  Float_t eBachelorPion();             // Energy of bachelor assuming pion
  Float_t eBachelorKaon();             // Energy of bachelor assuming kaon
  Float_t massOmega();                 // Mass assuming Omega hypothesis
  Float_t massXi();                    // Mass assuming Xi hypothesis
  Float_t rapOmega();                  // Rapidity assuming (anti)Omega
  Float_t rapXi();                     // Rapidity assuming (anti)Xi
  Float_t cTauOmega();                 // Lifetime (ctau) assuming (anti)Omega
  Float_t cTauXi();                    // Lifetime (ctau) assuming (anti)Xi
  Float_t ptBachelor();                // Transverse momentum of bachelor
  Float_t ptotBachelor();              // Total momentum of bachelor
  Float_t ptXi();                      // Transverse momentum of Xi/Omega
  Float_t ptotXi();                    // Total momentum of Xi/Omega

protected:
  Int_t   mCharge;                     // Written out
  Float_t mDecayVertexXi[3];

  Float_t mDcaXiDaughters;
  Float_t mDcaBachelorToPrimVertex;
  Float_t mDcaXiToPrimVertex;
  Float_t mMomBachelor[3];

  Int_t   mTpcHitsBachelor;

  void    FillXi(StXiVertex*);
  Float_t MomXi(int n);
  Float_t Ptot2Bachelor();
  Float_t Ptot2Xi();
  Float_t Pt2Xi();
  Float_t MomBachelorAlongXi();
  Float_t MomV0AlongXi();

  ClassDef(StXiMuDst, 1)
};

inline StXiMuDst::StXiMuDst(StXiVertex* x1,StV0Vertex* v1,StStrangeEvMuDst* e1):
StV0MuDst(v1,e1)
             { FillXi(x1); }

inline Int_t   StXiMuDst::charge() const
             { return mCharge; }
inline Float_t *StXiMuDst::decayVertexXi()
             { return mDecayVertexXi; } 

inline Float_t StXiMuDst::dcaXiDaughters() const 
             { return mDcaXiDaughters; }
inline Float_t StXiMuDst::dcaXiToPrimVertex() const 
             { return mDcaXiToPrimVertex; }
inline Float_t StXiMuDst::dcaBachelorToPrimVertex() const 
             { return mDcaBachelorToPrimVertex; }
inline Float_t *StXiMuDst::momBachelor() 
             { return mMomBachelor; }
inline Float_t *StXiMuDst::momXi()
             { Float_t a[3] = {MomXi(0),MomXi(1),MomXi(2)}; return a; }

inline Int_t   StXiMuDst::tpcHitsBachelor() const
             { return mTpcHitsBachelor; }
#endif

