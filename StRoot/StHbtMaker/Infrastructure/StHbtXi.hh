/***********************************************************************
 *
 * $Id: StHbtXi.hh,v 1.0 1999/09/07
 *
 * Authors: Frank Laue
 *
 ***********************************************************************
 *
 * Description: Xi class with members copied from StXihbtXi.hh
 *
 ***********************************************************************/
#ifndef StHbtXi_hh
#define StHbtXi_hh

#include "Stiostream.h"
#include "StHbtMaker/Infrastructure/StHbtVector.hh" //same as in StHbtTrack.hh
#include "StHbtMaker/Infrastructure/StHbtV0.hh"

#ifdef __ROOT__
#include "StStrangeMuDstMaker/StXiMuDst.hh"
#endif

class StHbtTTreeEvent;
class StHbtTTreeXi;

class StHbtXi : public StHbtV0 {
public:
  StHbtXi(){/* no-op */}
#ifdef __ROOT__
  StHbtXi(StXiMuDst&); // from strangeness Xi micro dst structure
  StHbtXi(const StHbtTTreeEvent*, const StHbtTTreeXi*);
#endif
  ~StHbtXi(){/* no-op */}

  void UpdateXi();
  float decayLengthXi() const;            // 3-d decay distance
  StHbtThreeVector decayVertexXi() const; // Coordinates of decay vertex
  float decayVertexXiX() const;           // Coordinates of decay vertex
  float decayVertexXiY() const;           // Coordinates of decay vertex
  float decayVertexXiZ() const;           // Coordinates of decay vertex
  float dcaXiDaughters() const;           // DCA of xi daughters at decay vertex
  float dcaXiToPrimVertex() const;        // DCA of xi to primary vertex
  float dcaBacToPrimVertex() const;       // DCA of bachelor  xi daughter to pri vertex
  StHbtThreeVector momBac() const;        // Momentum components of bac. daughter
  float momBacX() const;                  // Momentum components of bac. daughter
  float momBacY() const;                  // Momentum components of bac. daughter
  float momBacZ() const;                  // Momentum components of bac. daughter

  int   tpcHitsBac() const;               // Number of TPC hits on bac. daughter
  unsigned long trackTopologyMapBac(unsigned int) const;

  StHbtThreeVector momXi() const ;        // Momentum components of Xi
  float momXiX() const ;                  // Momentum components of Xi
  float momXiY() const ;                  // Momentum components of Xi
  float momXiZ() const ;                  // Momentum components of Xi
  float alphaXi() const ;                 // Armenteros-Podolanski variable
  float ptArmXi() const ;                 // Armenteros-Podolanski variable
  float eXi() const ;                     // Energy assuming xi hypothesis
  float eOmega() const ;                  // Energy assuming omega hypothesis
  float eBacKaon() const ;                // Energy of bac. daughter assuming kaon
  float eBacPion() const ;                // Energy of bac. daughter assuming pion
  float massXi() const ;                  // Mass assuming Xi hypothesis
  float massOmega() const ;               // Mass assuming Omega hypothesis
  float rapXi() const ;                   // Rapidity assuming (anti) xi
  float rapOmega() const ;                // Rapidity assuming (anti) omega
  float cTauXi() const ;                  // Lifetime (ctau) const assuming (anti) xi
  float cTauOmega() const ;               // Lifetime (ctau) const assuming (anti) omega
  float ptXi() const ;                    // Transverse momentum
  float ptotXi() const ;                  // Total momentum
  float ptBac() const ;                   // Transverse momentum of bac. daughter
  float ptotBac() const ;                 // Total momentum of bac. daughter
  float dedxBac() const;                  // dedx of Bac track
  unsigned short   idBac() const;         // Id of bac. track
  unsigned short   keyBac() const;        // Id of bac. track

  void SetdecayLengthXi(const float);  
  void SetdecayVertexXi(const StHbtThreeVector);  
  void SetdecayVertexXiX(const float);
  void SetdecayVertexXiY(const float);
  void SetdecayVertexXiZ(const float);
  void SetdcaXiDaughters(const float); 
  void SetdcaXiToPrimVertex(const float);  
  void SetdcaBacToPrimVertex(const float); 
  void SetmomBac(const StHbtThreeVector);  
  void SetmomBacX(const float);  
  void SetmomBacY(const float);  
  void SetmomBacZ(const float);  

  void SettpcHitsBac(const int&);      

  void SetTrackTopologyMapBac(unsigned int, const unsigned long&);

  void SetmomXi( StHbtThreeVector);
  void SetmomXiX( float);
  void SetmomXiY( float);
  void SetmomXiZ( float);
  void SetalphaXi( float);       
  void SetptArmXi( float);       
  void SeteXi( float);     
  void SeteOmega( float);    
  void SeteBacPion( float);  
  void SeteBacKaon( float);    
  void SetmassXi( float);  
  void SetmassOmega( float);
  void SetrapXi( float);    
  void SetrapOmega( float);   
  void SetcTauXi( float);   
  void SetcTauOmega( float);  
  void SetptXi( float);         
  void SetptotXi( float);       
  void SetptBac( float);        
  void SetptotBac( float);      
  void SetidBac(const unsigned short&);
  void SetdedxBac(float);
  void SetkeyBac(const unsigned short&);

//   friend ostream& operator<<(ostream& out, StHbtXi& v0);
//   friend istream& operator>>(istream& in,  StHbtXi& v0);

   friend class StHbtIOBinary;
   friend class StHbtTTreeXi;

protected:
  int   mCharge;                
  float mDecayLengthXi;
  StHbtThreeVector mDecayVertexXi;
  float mDcaXiDaughters; 
  float mDcaXiToPrimVertex;
  float mDcaBachelorToPrimVertex;
  StHbtThreeVector mMomBachelor;

  unsigned int   mTopologyMapBachelor[2];
  unsigned short mKeyBachelor;

  int   mTpcHitsBac;

  float mChi2Xi;
  float mClXi;
  float mChi2Bachelor;
  float mClBachelor;

  float mDedxBachelor;
  unsigned short mNumDedxBachelor;

  // the following variables are not in the persistent version and can be calculated via UpdateXi();
  StHbtThreeVector mMomXi;
  float mAlphaXi;
  float mPtArmXi;

  float mEXi;
  float mEOmega;
  float mEBacPion;
  float mEBacKaon;
  float mMassXi;
  float mMassOmega;
  float mRapXi;
  float mRapOmega;
  float mCTauXi;
  float mCTauOmega;
  float mPtXi;
  float mPtotXi;
  float mPtBac;
  float mPtotBac;

  unsigned short   mKeyBac;
};

inline float StHbtXi::decayLengthXi() const { return mDecayLengthXi; }
inline StHbtThreeVector StHbtXi::decayVertexXi() const { return mDecayVertexXi; } 
inline float StHbtXi::decayVertexXiX() const { return mDecayVertexXi.x(); } 
inline float StHbtXi::decayVertexXiY() const { return mDecayVertexXi.y(); } 
inline float StHbtXi::decayVertexXiZ() const { return mDecayVertexXi.z(); } 
inline float StHbtXi::dcaXiDaughters() const { return mDcaXiDaughters; }
inline float StHbtXi::dcaXiToPrimVertex() const { return mDcaXiToPrimVertex; }
inline float StHbtXi::dcaBacToPrimVertex() const { return mDcaBachelorToPrimVertex; }
inline StHbtThreeVector StHbtXi::momBac() const { return mMomBachelor; }
inline float StHbtXi::momBacX() const { return mMomBachelor.x(); }
inline float StHbtXi::momBacY() const { return mMomBachelor.y(); }
inline float StHbtXi::momBacZ() const { return mMomBachelor.z(); }
inline StHbtThreeVector StHbtXi::momXi() const { return mMomXi; }
inline float StHbtXi::momXiX() const { return mMomXi.x(); }
inline float StHbtXi::momXiY() const { return mMomXi.y(); }
inline float StHbtXi::momXiZ() const { return mMomXi.z(); }
inline float StHbtXi::alphaXi() const { return mAlphaXi; }
inline float StHbtXi::ptArmXi() const {return mPtArmXi;}
inline float StHbtXi::eXi() const {return mEXi;}
inline float StHbtXi::eOmega() const {return mEOmega;}
inline float StHbtXi::eBacPion() const {return mEBacPion;}
inline float StHbtXi::eBacKaon() const {return mEBacKaon;}
inline float StHbtXi::massXi() const {return mMassXi;}
inline float StHbtXi::massOmega() const {return mMassOmega;}
inline float StHbtXi::rapXi() const {return mRapXi;}
inline float StHbtXi::rapOmega() const {return mRapOmega;}
inline float StHbtXi::cTauXi() const {return mCTauXi;}
inline float StHbtXi::cTauOmega() const {return mCTauOmega;}
inline float StHbtXi::ptXi() const {return mPtXi;}
inline float StHbtXi::ptotXi() const {return mPtotXi;}
inline float StHbtXi::ptBac() const {return mPtBac;}
inline float StHbtXi::ptotBac() const {return mPtotBac;}
inline int   StHbtXi::tpcHitsBac() const
             { return mTpcHitsBac; }
inline float StHbtXi::dedxBac() const {return mDedxBachelor;}

inline unsigned long   StHbtXi::trackTopologyMapBac(unsigned int word) const { return mTopologyMapBachelor[word]; }
inline unsigned short   StHbtXi::idBac() const { return mKeyBac; }; 
inline unsigned short   StHbtXi::keyBac() const { return mKeyBac; }

inline void StHbtXi::SetdecayLengthXi(const float x){ mDecayLengthXi= x;}   
inline void StHbtXi::SetdecayVertexXiX(const float x){ mDecayVertexXi.setX(x);}
inline void StHbtXi::SetdecayVertexXiY(const float x){ mDecayVertexXi.setY(x);}
inline void StHbtXi::SetdecayVertexXiZ(const float x){ mDecayVertexXi.setZ(x);}
inline void StHbtXi::SetdecayVertexXi(const StHbtThreeVector v){ mDecayVertexXi = v; }
inline void StHbtXi::SetdcaXiDaughters(const float x){mDcaXiDaughters= x;} 
inline void StHbtXi::SetdcaXiToPrimVertex(const float x){mDcaXiToPrimVertex= x;}   
inline void StHbtXi::SetdcaBacToPrimVertex(const float x){ mDcaBachelorToPrimVertex = x;} 
inline void StHbtXi::SetmomBac(const StHbtThreeVector v){mMomBachelor = v; }
inline void StHbtXi::SetmomBacX(const float x){mMomBachelor.setX(x);}
inline void StHbtXi::SetmomBacY(const float x){mMomBachelor.setY(x);}
inline void StHbtXi::SetmomBacZ(const float x){mMomBachelor.setZ(x);}
inline void StHbtXi::SetTrackTopologyMapBac(unsigned int word, const unsigned long& m){mTopologyMapBachelor[word]=m;} 
inline void StHbtXi::SetmomXi(StHbtThreeVector v){mMomXi= v; }
inline void StHbtXi::SetmomXiX(const float x){mMomXi.setX(x);}
inline void StHbtXi::SetmomXiY(const float x){mMomXi.setY(x);}
inline void StHbtXi::SetmomXiZ(const float x){mMomXi.setZ(x);}

inline void StHbtXi::SetalphaXi( float x){mAlphaXi= x;}
inline void StHbtXi::SetptArmXi( float x){mPtArmXi = x;}
inline void StHbtXi::SeteXi( float x){mEXi= x;}       
inline void StHbtXi::SeteOmega( float x){mEOmega= x;}
inline void StHbtXi::SeteBacPion( float x){mEBacPion= x;}
inline void StHbtXi::SeteBacKaon( float x){mEBacKaon= x;}
inline void StHbtXi::SetmassXi( float x){mMassXi = x;} 
inline void StHbtXi::SetmassOmega( float x){mMassOmega= x;}  
inline void StHbtXi::SetrapXi( float x){mRapXi= x;}
inline void StHbtXi::SetrapOmega( float x){mRapOmega = x;}   
inline void StHbtXi::SetcTauXi( float x){mCTauXi = x;}   
inline void StHbtXi::SetcTauOmega( float x){mCTauOmega = x;}   
inline void StHbtXi::SetptXi( float x){mPtXi = x;}          
inline void StHbtXi::SetptotXi( float x){mPtotXi = x;}
inline void StHbtXi::SetptBac( float x){mPtBac = x;}
inline void StHbtXi::SetptotBac( float x){mPtotBac = x;}    
inline void StHbtXi::SetidBac(const unsigned short& s){ mKeyBac= s;}
inline void StHbtXi::SetkeyBac(const unsigned short& s){ mKeyBac= s;}
inline void StHbtXi::SettpcHitsBac(const int& i){mTpcHitsBac=i;} 
inline void StHbtXi::SetdedxBac(float x){mDedxBachelor=x;}

#endif


/***********************************************************************
 *
 * $Log: StHbtXi.hh,v $
 * Revision 1.3  2003/09/02 17:58:33  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.2  2001/12/05 15:10:33  laue
 * Boris' updates (mainly access functions)
 *
 *
 ***********************************************************************/
















