/***********************************************************************
 *
 * $Id: StXiMuDst.hh,v 2.0 2000/06/02 22:11:55 genevb Exp $
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
 * Revision 2.0  2000/06/02 22:11:55  genevb
 * New version of Strangeness micro DST package
 *
 * Revision 1.3  2000/03/31 03:20:25  jones
 * Added topology map to V0/Xi; access funcs for each data member
 *
 * Revision 1.2  2000/03/29 20:52:14  genevb
 * Added StKinkMuDst, replaced arrays
 *
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
  virtual Float_t decayLengthV0() const;  // 3-d decay distance
  virtual Float_t decayLengthXi() const;  // 3-d decay distance
  Float_t decayVertexXiX() const;      // Coordinate of decay vertex
  Float_t decayVertexXiY() const;
  Float_t decayVertexXiZ() const;
  Float_t dcaXiDaughters() const;      // DCA of xi daughters at decay vertex
  Float_t dcaBachelorToPrimVertex() const; // DCA of bachelor to primary vertex
  Float_t dcaXiToPrimVertex() const;   // DCA of xi to primary vertex
  Float_t momBachelorX() const;        // Momentum components of bachelor
  Float_t momBachelorY() const;
  Float_t momBachelorZ() const;
  UShort_t keyBachelor() const;
  StTrackTopologyMap& topologyMapBachelor();

  Float_t momXiX() const;              // Momentum components of Xi/Omega
  Float_t momXiY() const;
  Float_t momXiZ() const;
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
  Float_t chi2Xi() const;              // Chi square of Xi
  Float_t clXi()   const;              // Confidence level of Xi
  Float_t chi2Bachelor() const;        // Chi square of bachelor
  Float_t clBachelor()   const;        // Confidence level of bachelor
  Long_t  detectorIdXi();              // Detector ID for Xi vertex
  virtual Long_t detectorIdPars();     // Detector ID for pars used in Xi finder

protected:
  Int_t   mCharge;                     // Written out
  Float_t mDecayVertexXiX;
  Float_t mDecayVertexXiY;
  Float_t mDecayVertexXiZ;

  Float_t mDcaXiDaughters;
  Float_t mDcaBachelorToPrimVertex;
  Float_t mDcaXiToPrimVertex;
  Float_t mMomBachelorX;
  Float_t mMomBachelorY;
  Float_t mMomBachelorZ;

  UShort_t mKeyBachelor;

  StTrackTopologyMap mTopologyMapBachelor;

  Float_t mChi2Xi;
  Float_t mClXi;
  Float_t mChi2Bachelor;
  Float_t mClBachelor;

  void    FillXi(StXiVertex*);
  Float_t Ptot2Bachelor();
  Float_t Ptot2Xi();
  Float_t Pt2Xi();
  Float_t MomBachelorAlongXi();
  Float_t MomV0AlongXi();

  ClassDef(StXiMuDst, 2)
};

inline StXiMuDst::StXiMuDst(StXiVertex* x1,StV0Vertex* v1,StStrangeEvMuDst* e1):
StV0MuDst(v1,e1)
             { FillXi(x1); }

inline Int_t   StXiMuDst::charge() const
             { return mCharge; }
inline Float_t StXiMuDst::decayVertexXiX() const { return mDecayVertexXiX; }
inline Float_t StXiMuDst::decayVertexXiY() const { return mDecayVertexXiY; }
inline Float_t StXiMuDst::decayVertexXiZ() const { return mDecayVertexXiZ; }
inline Float_t StXiMuDst::dcaXiDaughters() const 
             { return mDcaXiDaughters; }
inline Float_t StXiMuDst::dcaXiToPrimVertex() const 
             { return mDcaXiToPrimVertex; }
inline Float_t StXiMuDst::dcaBachelorToPrimVertex() const 
             { return mDcaBachelorToPrimVertex; }
inline Float_t StXiMuDst::momBachelorX() const { return mMomBachelorX; }
inline Float_t StXiMuDst::momBachelorY() const { return mMomBachelorY; }
inline Float_t StXiMuDst::momBachelorZ() const { return mMomBachelorZ; }
inline Float_t StXiMuDst::momXiX() const { return mMomBachelorX + momV0X(); }
inline Float_t StXiMuDst::momXiY() const { return mMomBachelorY + momV0Y(); }
inline Float_t StXiMuDst::momXiZ() const { return mMomBachelorZ + momV0Z(); }
inline UShort_t StXiMuDst::keyBachelor() const { return mKeyBachelor; }
inline StTrackTopologyMap& StXiMuDst::topologyMapBachelor()
             { return mTopologyMapBachelor; }
inline Float_t StXiMuDst::chi2Xi() const { return mChi2Xi; }
inline Float_t StXiMuDst::clXi()   const { return mClXi; }
inline Float_t StXiMuDst::chi2Bachelor() const { return mChi2Bachelor; }
inline Float_t StXiMuDst::clBachelor()   const { return mClBachelor; }
#endif
