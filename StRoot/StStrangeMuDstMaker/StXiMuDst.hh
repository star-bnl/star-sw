/*!
 * \class StXiMuDst
 * \author Gene Van Buren, UCLA, 24-Mar-2000
 * \author Peter G. Jones, University of Birmingham, 30-Mar-1999
 *
 *               Xi (cascade) micro dst class
 *
 */

#ifndef StXiMuDst_hh
#define StXiMuDst_hh
#include "StV0MuDst.hh"
#include "StXiI.hh"

class StXiVertex;
#include "StPhysicalHelixD.hh"

class StXiMuDst : public StV0MuDst, public virtual StXiI {
  friend class StMuMomentumShiftMaker;

public:
  StXiMuDst();
  ~StXiMuDst();
  StXiMuDst(StXiVertex* x1,StV0Vertex* v1,StStrangeEvMuDst* e1);
  void    Fill(StXiVertex*,StV0Vertex*,StStrangeEvMuDst*);

  Int_t   charge() const;              // Particle charge
  Float_t decayVertexXiX() const;      // Coordinate of decay vertex
  Float_t decayVertexXiY() const;
  Float_t decayVertexXiZ() const;
  virtual Float_t decayLengthV0() const; // 3-d decay distance
  Float_t dcaXiDaughters() const;      // DCA of xi daughters at decay vertex
  Float_t dcaBachelorToPrimVertex() const; // DCA of bachelor to primary vertex
  Float_t dcaXiToPrimVertex() const;   // DCA of xi to primary vertex
  Float_t momBachelorX() const;        // Momentum components of bachelor
  Float_t momBachelorY() const;
  Float_t momBachelorZ() const;
  Int_t   keyBachelor() const;
  StTrackTopologyMap& topologyMapBachelor();

  TVector3 momXi();                    //  Momentum components of Xi/Omega at decay vertex
  Float_t momXiX();
  Float_t momXiY();                    /// Momentum components of Xi/Omega at decay vertex
  Float_t momXiZ();

  TVector3 momXiAtPrimVertex();        //  Momentum components of Xi/Omega at primary vertex
  Float_t momXiAtPrimVertexX();
  Float_t momXiAtPrimVertexY();        /// Momentum components of Xi/Omega at primary vertex
  Float_t momXiAtPrimVertexZ();

  Float_t chi2Xi() const;              // Chi square of Xi
  Float_t clXi()   const;              // Confidence level of Xi
  Float_t chi2Bachelor() const;        // Chi square of bachelor
  Float_t clBachelor()   const;        // Confidence level of bachelor
  void setBachelorBad();               // Set the bachelor as bad
  Long_t  detectorIdXi();              // Detector ID for Xi vertex
  virtual Long_t detectorIdPars();     // Detector ID for pars used in Xi finder
  Float_t dedxBachelor() const;        // dE/dX of bachelor
  Float_t errDedxBachelor() const;     // Error on mean of dE/dX of bachelor
  UShort_t numDedxBachelor() const;    // Number of dE/dX points for bachelor
  Float_t lenDedxBachelor() const;     // Length of dE/dX track for bachelor

  StPhysicalHelixD& helixXi();         // helix of the Xi track

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

  Int_t mKeyBachelor;

  StTrackTopologyMap mTopologyMapBachelor;

  Float_t mChi2Xi;
  Float_t mClXi;
  Float_t mChi2Bachelor;
  Float_t mClBachelor;

  void    FillXi(StXiVertex*);
  void    setXiHelix();

  Float_t mDedxBachelor;
  Float_t mErrDedxBachelor;
  UShort_t mNumDedxBachelor;

  ClassDef(StXiMuDst,8)
};

inline StXiMuDst::StXiMuDst(StXiVertex* x1,StV0Vertex* v1,StStrangeEvMuDst* e1):
             StXiI(), StV0MuDst(v1,e1)
             { FillXi(x1); }

inline Int_t   StXiMuDst::charge() const
             { return mCharge; }
inline Float_t StXiMuDst::decayVertexXiX() const { return mDecayVertexXiX; }
inline Float_t StXiMuDst::decayVertexXiY() const { return mDecayVertexXiY; }
inline Float_t StXiMuDst::decayVertexXiZ() const { return mDecayVertexXiZ; }
inline Float_t StXiMuDst::decayLengthV0() const { return StXiI::decayLengthV0(); }
inline Float_t StXiMuDst::dcaXiDaughters() const 
             { return mDcaXiDaughters; }
inline Float_t StXiMuDst::dcaXiToPrimVertex() const 
             { return mDcaXiToPrimVertex; }
inline Float_t StXiMuDst::dcaBachelorToPrimVertex() const 
             { return mDcaBachelorToPrimVertex; }
inline Float_t StXiMuDst::momBachelorX() const { return mMomBachelorX; }
inline Float_t StXiMuDst::momBachelorY() const { return mMomBachelorY; }
inline Float_t StXiMuDst::momBachelorZ() const { return mMomBachelorZ; }
inline TVector3 StXiMuDst::momXi()
             {return TVector3(momXiX(), momXiY(), momXiZ());}
inline Float_t StXiMuDst::momXiX() { return mMomBachelorX + momV0X(); }
inline Float_t StXiMuDst::momXiY() { return mMomBachelorY + momV0Y(); }
inline Float_t StXiMuDst::momXiZ() { return mMomBachelorZ + momV0Z(); }
inline Float_t StXiMuDst::momXiAtPrimVertexX() { return momXiAtPrimVertex().X(); }
inline Float_t StXiMuDst::momXiAtPrimVertexY() { return momXiAtPrimVertex().Y(); }
inline Float_t StXiMuDst::momXiAtPrimVertexZ() { return momXiAtPrimVertex().Z(); }
inline Int_t StXiMuDst::keyBachelor() const { return mKeyBachelor; }
inline StTrackTopologyMap& StXiMuDst::topologyMapBachelor()
             { return mTopologyMapBachelor; }
inline Float_t StXiMuDst::chi2Xi() const { return mChi2Xi; }
inline Float_t StXiMuDst::clXi()   const { return mClXi; }
inline Float_t StXiMuDst::chi2Bachelor() const { return mChi2Bachelor; }
inline Float_t StXiMuDst::clBachelor()   const { return mClBachelor; }
inline void StXiMuDst::setBachelorBad() { mChi2Bachelor = -TMath::Abs(mChi2Bachelor); }
inline Float_t StXiMuDst::dedxBachelor() const { return mDedxBachelor; }
inline Float_t StXiMuDst::errDedxBachelor() const { return mErrDedxBachelor; }
inline UShort_t StXiMuDst::numDedxBachelor() const
             { return (mNumDedxBachelor%100); }
inline Float_t StXiMuDst::lenDedxBachelor() const
             { return (mNumDedxBachelor/100); }

#endif


/***********************************************************************
 * $Id: StXiMuDst.hh,v 3.13 2011/05/27 18:25:32 genevb Exp $
 * $Log: StXiMuDst.hh,v $
 * Revision 3.13  2011/05/27 18:25:32  genevb
 * Propagate StTrack::key => Int_t to other codes
 *
 * Revision 3.12  2008/07/11 16:23:10  genevb
 * bad() won't work unless chi2 allows to return negative values
 *
 * Revision 3.11  2008/07/10 16:16:56  genevb
 * Allow for marking of bad tracks -> bad secondary vertices
 *
 * Revision 3.10  2005/07/06 22:32:07  fisyak
 * Use templated StPhysicalHelixD
 *
 * Revision 3.9  2005/07/03 19:04:34  perev
 * Change class version to avoid ROOT bug in old version 4.00.04
 *
 * Revision 3.8  2005/03/17 05:02:20  genevb
 * Add StMuMomentumShiftMaker friend
 *
 * Revision 3.7  2003/10/20 17:20:19  perev
 * Change the order of inheritance and increased version numbers
 *
 * Revision 3.6  2003/08/26 22:36:28  genevb
 * Calculate Xi momenta at/near primary vertex
 *
 * Revision 3.5  2003/05/30 21:20:20  genevb
 * doxygen savvy, encoding of FTPC mults, change virtual funcs
 *
 * Revision 3.4  2001/11/05 23:41:07  genevb
 * Add more dEdx, B field info, careful of changes to TTree unrolling
 *
 * Revision 3.3  2001/05/04 20:15:15  genevb
 * Common interfaces and reorganization of components, add MC event info
 *
 * Revision 3.2  2000/08/10 01:16:25  genevb
 * Added number of dedx points
 *
 * Revision 3.1  2000/07/14 14:09:11  genevb
 * Fixed small typo
 *
 * Revision 3.0  2000/07/14 12:56:51  genevb
 * Revision 3 has event multiplicities and dedx information for vertex tracks
 *
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
