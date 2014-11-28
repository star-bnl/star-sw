/***********************************************************************
 *
 * $Id: StHbtV0.hh,v 1.0 1999/09/07
 *
 * Authors: Helen Caines, Tom Humanic 07-Sep-1999
 *
 ***********************************************************************
 *
 * Description: V0 class with members copied from StV0hbtV0.hh
 *
 ***********************************************************************
 *
 * $Log: StHbtV0.hh,v $
 * Revision 1.13  2003/09/02 17:58:32  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.12  2003/01/14 09:45:14  renault
 * enable this class to be used for theorical calculations.
 *
 * Revision 1.11  2002/11/19 23:37:56  renault
 * Get V0 daughters helix
 *
 * Revision 1.10  2002/02/09 19:25:36  laue
 * updates (dedx length)
 *
 * Revision 1.9  2001/09/05 20:41:43  laue
 * Updates of the hbtMuDstTree microDSTs
 *
 * Revision 1.8  2000/10/09 21:54:23  laue
 * Helens changes to the V0s
 *
 * Revision 1.7  2000/08/09 14:50:22  laue
 * 'const' removed to compile on solaris
 *
 * Revision 1.6  2000/07/16 21:38:23  laue
 * StHbtCoulomb.cxx StHbtSectoredAnalysis.cxx : updated for standalone version
 * StHbtV0.cc StHbtV0.hh : some cast to prevent compiling warnings
 * StHbtParticle.cc StHbtParticle.hh : pointers mTrack,mV0 initialized to 0
 * StHbtIOBinary.cc : some printouts in #ifdef STHBTDEBUG
 * StHbtEvent.cc : B-Field set to 0.25Tesla, we have to think about a better
 *                 solution
 *
 * Revision 1.5  2000/05/03 17:44:43  laue
 * StHbtEvent, StHbtTrack & StHbtV0 declared friend to StHbtIOBinary
 * StHbtParticle updated for V0 pos,neg track Id
 *
 * Revision 1.4  2000/02/18 21:32:24  laue
 * franksTrackCut changed. If mCharge is set to '0' there will be no cut
 * on charge. This is important for front-loaded cuts.
 *
 * copy constructor implemented for StHbtEvent, StHbtTrack and StHbtV0.
 *
 * franks1HistoD.cxx franks1HistoD.h franks2HistoD.cxx franks2HistoD.h
 * removed. We can now (CC5 on Solaris) use the versions (no D)
 *
 * Revision 1.3  2000/02/01 00:33:32  laue
 * namespaces changed to run on the new Solaris Compiler CC5
 * since we can use member templates in franks1Histo.hh we are doing it
 *
 * Revision 1.2  1999/10/15 02:32:37  lisa
 * Helen has added method StHbtV0::UpdateV0() to fill derived information - this leads to smaller microDSTs
 *
 * Revision 1.1  1999/09/16 18:47:59  lisa
 * replace placeholder HbtV0Track stuff with Helens StHbtV0 classes
 *
 * 
 *
 ***********************************************************************/
#ifndef StHbtV0_hh
#define StHbtV0_hh

#include "Stiostream.h"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh" //same as in StHbtTrack.hh
#include "StarClassLibrary/StPhysicalHelixD.hh" // Gael 12 Sept 02
#ifdef __ROOT__
#include "StStrangeMuDstMaker/StV0MuDst.hh"
#endif
/* Th stuff */
#include "StHbtMaker/Base/StHbtHiddenInfo.hh"
/***/
class StHbtTTreeEvent;
class StHbtTTreeV0;

class StHbtV0 {
public:
  StHbtV0(){/* no-op */}
  StHbtV0( const StHbtV0&); // copy constructor
#ifdef __ROOT__
  StHbtV0( StV0MuDst&); // from strangeness V0 micro dst structure
  StHbtV0(const StHbtTTreeEvent*, const StHbtTTreeV0*);
#endif
  ~StHbtV0(){if(mHiddenInfo) delete mHiddenInfo;}


  float decayLengthV0() const;       // 3-d decay distance
  StHbtThreeVector decayVertexV0() const; // Coordinates of decay vertex
  StHbtThreeVector primaryVertex() const; // Coordinates of primary vertex
  float decayVertexV0X() const; // Coordinates of decay vertex
  float decayVertexV0Y() const; // Coordinates of decay vertex
  float decayVertexV0Z() const; // Coordinates of decay vertex
  float dcaV0Daughters() const;      // DCA of v0 daughters at decay vertex
  float dcaV0ToPrimVertex() const;   // DCA of v0 to primary vertex
  float dcaPosToPrimVertex() const;  // DCA of pos v0 daughter to pri vertex
  float dcaNegToPrimVertex() const;  // DCA of neg v0 daughter to pri vertex
  StHbtThreeVector momPos() const;   // Momentum components of pos. daughter
  float momPosX() const;   // Momentum components of pos. daughter
  float momPosY() const;   // Momentum components of pos. daughter
  float momPosZ() const;   // Momentum components of pos. daughter
  StHbtThreeVector momNeg() const;   // Momentum components of neg. daughter
  float momNegX() const;   // Momentum components of neg. daughter
  float momNegY() const;   // Momentum components of neg. daughter
  float momNegZ() const;   // Momentum components of neg. daughter

  int   tpcHitsPos() const;          // Number of TPC hits on pos. daughter
  int   tpcHitsNeg() const;          // Number of TPC hits on neg. daughter
  unsigned long trackTopologyMapPos(unsigned int) const;
  unsigned long trackTopologyMapNeg(unsigned int) const;

  StHbtThreeVector momV0() const ;    // Momentum components of V0
  float momV0X() const ;    // Momentum components of V0
  float momV0Y() const ;    // Momentum components of V0
  float momV0Z() const ;    // Momentum components of V0
  float alphaV0() const ;             // Armenteros-Podolanski variable
  float ptArmV0() const ;            // Armenteros-Podolanski variable
  float eLambda() const ;             // Energy assuming lambda hypothesis
  float eK0Short() const ;            // Energy assuming k-short hypothesis
  float ePosProton() const ;          // Energy of pos. daughter assuming proton
  float ePosPion() const ;            // Energy of pos. daughter assuming pion
  float eNegProton() const ;          // Energy of neg. daughter assuming antiproton
  float eNegPion() const ;            // Energy of neg. daughter assuming pion
  float massLambda() const ;          // Mass assuming lambda hypothesis
  float massAntiLambda() const ;      // Mass assuming antilambda hypothesis
  float massK0Short() const ;         // Mass assuming k-short hypothesis
  float rapLambda() const ;           // Rapidity assuming (anti) constlambda
  float rapK0Short() const ;          // Rapidity assuming k-short
  float cTauLambda() const ;          // Lifetime (ctau) const assuming (anti) constlambda
  float cTauK0Short() const ;         // Lifetime (ctau) const assuming k-short
  float ptV0() const ;                // Transverse momentum
  float ptotV0() const ;              // Total momentum
  float ptPos() const ;               // Transverse momentum of pos. daughter
  float ptotPos() const ;             // Total momentum of pos. daughter
  float dedxPos() const;              // dedx of Positive track
  float numdedxPos() const;
// number of hits in dE/dX track of pos. daughter--Gael04Fev 2002
  float errdedxPos() const;              
// error on dedx of Positive track--Gael04Fev 2002
  float lendedxPos() const;              
// Length of dE/dX track of pos. daughter--Gael04Fev 2002
  float pseudoRapPos() const;              
// Length of dE/dX track of neg. daughter--Gael04Fev 2002

  float ptNeg() const ;               // Transverse momentum of neg. daughter
  float ptotNeg() const ;             // Total momentum of neg. daughter
  float dedxNeg() const;              // dedx of Negative track
  float numdedxNeg() const;
// number of hits in dE/dX track of neg. daughter--Gael04Fev 2002
  float errdedxNeg() const;              
// error on dedx of Negative track--Gael04Fev 2002
  float lendedxNeg() const;              
// Length of dE/dX track of neg. daughter--Gael04Fev 2002
  float pseudoRapNeg() const;              
// Length of dE/dX track of neg. daughter--Gael04Fev 2002

  unsigned short     idNeg() const;               // Id of negative track
  unsigned short   idPos() const;               // Id of positive track
  unsigned short   keyNeg() const;               // Id of negative track
  unsigned short   keyPos() const;               // Id of positive track

  const StPhysicalHelixD& HelixPos() const; // Gael 12 Sept 02
  const StPhysicalHelixD& HelixNeg() const; // Gael 12 Sept 02

  void UpdateV0(); // Fills derived info
  void SetdecayLengthV0(const float);  
  void SetdecayVertexV0(const StHbtThreeVector);  
  void SetdecayVertexV0X(const float);
  void SetdecayVertexV0Y(const float);
  void SetdecayVertexV0Z(const float);
  void SetdcaV0Daughters(const float); 
  void SetdcaV0ToPrimVertex(const float);  
  void SetdcaPosToPrimVertex(const float); 
  void SetdcaNegToPrimVertex(const float); 
  void SetmomPos(const StHbtThreeVector);  
  void SetmomPosX(const float);  
  void SetmomPosY(const float);  
  void SetmomPosZ(const float);  
  void SetmomNeg(const StHbtThreeVector);  
  void SetmomNegX(const float);  
  void SetmomNegY(const float);  
  void SetmomNegZ(const float);  

  void SettpcHitsPos(const int&);      
  void SettpcHitsNeg(const int&);      

  void SetTrackTopologyMapPos(unsigned int, const unsigned long&);
  void SetTrackTopologyMapNeg(unsigned int, const unsigned long&);      

  void SetmomV0( StHbtThreeVector);
  void SetmomV0X( float);
  void SetmomV0Y( float);
  void SetmomV0Z( float);
  void SetalphaV0( float);       
  void SetptArmV0( float);       
  void SeteLambda( float);     
  void SeteK0Short( float);    
  void SetePosProton( float);  
  void SetePosPion( float);    
  void SeteNegProton( float);  
  void SeteNegPion( float);    
  void SetmassLambda( float);  
  void SetmassAntiLambda( float);
  void SetmassK0Short( float);  
  void SetrapLambda( float);    
  void SetrapK0Short( float);   
  void SetcTauLambda( float);   
  void SetcTauK0Short( float);  
  void SetptV0( float);         
  void SetptotV0( float);       
  void SetptPos( float);        
  void SetptotPos( float);      
  void SetptNeg( float);        
  void SetptotNeg( float);
  void SetidNeg(const unsigned short&);
  void SetidPos(const unsigned short&);
  void SetdedxNeg(float);
  void SeterrdedxNeg(float x);//Gael 04Fev2002
  void SetlendedxNeg(float x);//Gael 04Fev2002
  void SetpseudoRapNeg(float x);//Gael 04Fev2002
  void SetdedxPos(float);
  void SeterrdedxPos(float x);//Gael 04Fev2002
  void SetlendedxPos(float x);//Gael 04Fev2002
  void SetpseudoRapPos(float x);//Gael 04Fev2002
  void SetkeyNeg(const unsigned short&);
  void SetkeyPos(const unsigned short&);
     
  void SetHelixPos(const StPhysicalHelixD&); // Gael 12 Sept 02
  void SetHelixNeg(const StPhysicalHelixD&); // Gael 12 Sept 02

  void SetprimaryVertex(const StHbtThreeVector v);//Gael 24 Sept 02
  /* Th stuff */
  void SetHiddenInfo(StHbtHiddenInfo* aHiddenInfo);
  bool ValidHiddenInfo() const;
  // Fab private : (official : const StHbtHiddenInfo* HiddenInfo() const;
  StHbtHiddenInfo* getHiddenInfo() const;
  /***/
  friend ostream& operator<<(ostream& out, StHbtV0& v0);
  friend istream& operator>>(istream& in,  StHbtV0& v0);

  friend class StHbtIOBinary;
  friend class StHbtTTreeV0;
  friend class StHbtTTreeXi;

protected:
 
  float mDecayLengthV0;
  StHbtThreeVector mDecayVertexV0;
  StHbtThreeVector mPrimaryVertex;
  float mDcaV0Daughters;
  float mDcaV0ToPrimVertex;
  float mDcaPosToPrimVertex;
  float mDcaNegToPrimVertex;
  StHbtThreeVector mMomPos;
  StHbtThreeVector mMomNeg;

  unsigned long  mTrackTopologyMapPos[2];
  unsigned long  mTrackTopologyMapNeg[2];
       
  int   mTpcHitsPos;
  int   mTpcHitsNeg;

  float mChi2V0;
  float mClV0;
  float mChi2Pos;
  float mClPos;
  float mChi2Neg;
  float mClNeg;

  float mDedxPos;
  float mErrDedxPos;
  float mLenDedxPos;
  
  float mDedxNeg;
  float mErrDedxNeg;
  float mLenDedxNeg;

  unsigned short mNumDedxPos;
  unsigned short mNumDedxNeg;

  StPhysicalHelixD mHelixPos; // Gael 12 Sept 02
  StPhysicalHelixD mHelixNeg; // Gael 12 Sept 02

  // the following variables are not in the persistent version and can be calculated via UpdateV0();
  StHbtThreeVector mMomV0;
  float mAlphaV0;
  float mPtArmV0;
  float mELambda;
  float mEK0Short;
  float mEPosProton;
  float mEPosPion;
  float mENegProton;
  float mENegPion;
  float mMassLambda;
  float mMassAntiLambda;
  float mMassK0Short;
  float mRapLambda;
  float mRapK0Short;
  float mCTauLambda;
  float mCTauK0Short;
  float mPtV0;
  float mPtotV0;
  float mPtPos;
  float mPtotPos;
  float mPtNeg;
  float mPtotNeg;

  unsigned short   mKeyNeg;
  unsigned short   mKeyPos;
  /* Th stuff */
  // Fab private : add mutable
  mutable StHbtHiddenInfo* mHiddenInfo; //!
  /***/


};



inline float StHbtV0::decayLengthV0() const { return mDecayLengthV0; }
inline StHbtThreeVector StHbtV0::decayVertexV0() const { return mDecayVertexV0; } 
inline StHbtThreeVector StHbtV0::primaryVertex() const { return mPrimaryVertex; }
inline float StHbtV0::decayVertexV0X() const { return mDecayVertexV0.x(); } 
inline float StHbtV0::decayVertexV0Y() const { return mDecayVertexV0.y(); } 
inline float StHbtV0::decayVertexV0Z() const { return mDecayVertexV0.z(); } 
inline float StHbtV0::dcaV0Daughters() const { return mDcaV0Daughters; }
inline float StHbtV0::dcaV0ToPrimVertex() const { return mDcaV0ToPrimVertex; }
inline float StHbtV0::dcaPosToPrimVertex() const { return mDcaPosToPrimVertex; }
inline float StHbtV0::dcaNegToPrimVertex() const { return mDcaNegToPrimVertex; }
inline StHbtThreeVector StHbtV0::momPos() const { return mMomPos; }
inline float StHbtV0::momPosX() const { return mMomPos.x(); }
inline float StHbtV0::momPosY() const { return mMomPos.y(); }
inline float StHbtV0::momPosZ() const { return mMomPos.z(); }
inline StHbtThreeVector StHbtV0::momNeg() const { return mMomNeg; }
inline float StHbtV0::momNegX() const { return mMomNeg.x(); }
inline float StHbtV0::momNegY() const { return mMomNeg.y(); }
inline float StHbtV0::momNegZ() const { return mMomNeg.z(); }
inline StHbtThreeVector StHbtV0::momV0() const { return mMomV0; }
inline float StHbtV0::momV0X() const { return mMomV0.x(); }
inline float StHbtV0::momV0Y() const { return mMomV0.y(); }
inline float StHbtV0::momV0Z() const { return mMomV0.z(); }
inline float StHbtV0::alphaV0() const { return mAlphaV0; }
inline float StHbtV0::ptArmV0() const {return mPtArmV0;}
inline float StHbtV0::eLambda() const {return mELambda;}
inline float StHbtV0::eK0Short() const {return mEK0Short;}
inline float StHbtV0::ePosProton() const {return mEPosProton;}
inline float StHbtV0::ePosPion() const {return mEPosPion;}
inline float StHbtV0::eNegProton() const {return mENegProton;}
inline float StHbtV0::eNegPion() const {return mENegPion;}
inline float StHbtV0::massLambda() const {return mMassLambda;}
inline float StHbtV0::massAntiLambda() const {return mMassAntiLambda;}
inline float StHbtV0::massK0Short() const {return mMassK0Short;}
inline float StHbtV0::rapLambda() const {return mRapLambda;}
inline float StHbtV0::rapK0Short() const {return mRapK0Short;}
inline float StHbtV0::cTauLambda() const {return mCTauLambda;}
inline float StHbtV0::cTauK0Short() const {return mCTauK0Short;}
inline float StHbtV0::ptV0() const {return mPtV0;}
inline float StHbtV0::ptotV0() const {return mPtotV0;}
inline float StHbtV0::ptPos() const {return mPtPos;}
inline float StHbtV0::ptotPos() const {return mPtotPos;}
inline float StHbtV0::ptNeg() const {return mPtNeg;}
inline float StHbtV0::ptotNeg() const {return mPtotNeg;}
inline int   StHbtV0::tpcHitsPos() const { return mTpcHitsPos; }
inline int   StHbtV0::tpcHitsNeg() const { return mTpcHitsNeg; }
inline float StHbtV0::dedxNeg() const {return mDedxNeg;}
inline float StHbtV0::numdedxNeg() const {return mNumDedxNeg;} //Gael 04Fev2002
inline float StHbtV0::errdedxNeg() const {return mErrDedxNeg;} //Gael 04Fev2002
inline float StHbtV0::lendedxNeg() const {return mLenDedxNeg;} //Gael 04Fev2002
inline float StHbtV0::pseudoRapNeg() const {return mMomNeg.pseudoRapidity();} //Gael 04Fev2002
inline float StHbtV0::dedxPos() const {return mDedxPos;}
inline float StHbtV0::numdedxPos() const {return mNumDedxPos;} //Gael 04Fev2002
inline float StHbtV0::errdedxPos() const {return mErrDedxPos;} //Gael 04Fev2002
inline float StHbtV0::lendedxPos() const {return mLenDedxPos;} //Gael 04Fev2002
inline float StHbtV0::pseudoRapPos() const {return mMomPos.pseudoRapidity();} //Gael 04Fev2002


inline unsigned long   StHbtV0::trackTopologyMapPos(unsigned int word) const { return mTrackTopologyMapPos[word]; }
inline unsigned long   StHbtV0::trackTopologyMapNeg(unsigned int word) const { return mTrackTopologyMapNeg[word]; }
inline unsigned short   StHbtV0::idNeg() const { return mKeyNeg; } 
inline unsigned short   StHbtV0::keyNeg() const { return mKeyNeg; }
inline unsigned short   StHbtV0::idPos() const { return mKeyPos; } 
inline unsigned short   StHbtV0::keyPos() const { return mKeyPos; }

inline void StHbtV0::SetdecayLengthV0(const float x){ mDecayLengthV0= x;}   
inline void StHbtV0::SetdecayVertexV0X(const float x){ mDecayVertexV0.setX(x);}
inline void StHbtV0::SetdecayVertexV0Y(const float x){ mDecayVertexV0.setY(x);}
inline void StHbtV0::SetdecayVertexV0Z(const float x){ mDecayVertexV0.setZ(x);}
inline void StHbtV0::SetdecayVertexV0(const StHbtThreeVector v){ mDecayVertexV0 = v; }
inline void StHbtV0::SetdcaV0Daughters(const float x){mDcaV0Daughters= x;} 
inline void StHbtV0::SetdcaV0ToPrimVertex(const float x){mDcaV0ToPrimVertex= x;}   
inline void StHbtV0::SetdcaPosToPrimVertex(const float x){mDcaPosToPrimVertex = x;} 
inline void StHbtV0::SetdcaNegToPrimVertex(const float x){mDcaNegToPrimVertex = x;} 
inline void StHbtV0::SetmomPos(const StHbtThreeVector v){mMomPos = v; }
inline void StHbtV0::SetmomPosX(const float x){mMomPos.setX(x);}
inline void StHbtV0::SetmomPosY(const float x){mMomPos.setY(x);}
inline void StHbtV0::SetmomPosZ(const float x){mMomPos.setZ(x);}
inline void StHbtV0::SetmomNeg(const StHbtThreeVector v){mMomNeg = v; }
inline void StHbtV0::SetmomNegX(const float x){mMomNeg.setX(x);}
inline void StHbtV0::SetmomNegY(const float x){mMomNeg.setY(x);}
inline void StHbtV0::SetmomNegZ(const float x){mMomNeg.setZ(x);}
inline void StHbtV0::SetTrackTopologyMapPos(unsigned int word, const unsigned long& m){mTrackTopologyMapPos[word]=m;} 
inline void StHbtV0::SetTrackTopologyMapNeg(unsigned int word, const unsigned long& m){mTrackTopologyMapNeg[word]=m;} 
inline void StHbtV0::SetmomV0(StHbtThreeVector v){mMomV0= v; }
inline void StHbtV0::SetmomV0X(const float x){mMomV0.setX(x);}
inline void StHbtV0::SetmomV0Y(const float x){mMomV0.setY(x);}
inline void StHbtV0::SetmomV0Z(const float x){mMomV0.setZ(x);}

inline void StHbtV0::SetalphaV0( float x){mAlphaV0= x;}
inline void StHbtV0::SetptArmV0( float x){mPtArmV0 = x;}
inline void StHbtV0::SeteLambda( float x){mELambda= x;}       
inline void StHbtV0::SeteK0Short( float x){mEK0Short= x;}
inline void StHbtV0::SetePosProton( float x){mEPosProton= x;}      
inline void StHbtV0::SetePosPion( float x){mEPosPion= x;}      
inline void StHbtV0::SeteNegProton( float x){mENegProton= x;} 
inline void StHbtV0::SeteNegPion( float x){mENegPion= x;}       
inline void StHbtV0::SetmassLambda( float x){mMassLambda = x;} 
inline void StHbtV0::SetmassAntiLambda( float x){mMassAntiLambda= x;} 
inline void StHbtV0::SetmassK0Short( float x){mMassK0Short= x;}  
inline void StHbtV0::SetrapLambda( float x){mRapLambda= x;}
inline void StHbtV0::SetrapK0Short( float x){mRapK0Short = x;}   
inline void StHbtV0::SetcTauLambda( float x){mCTauLambda = x;}   
inline void StHbtV0::SetcTauK0Short( float x){mCTauK0Short = x;}   
inline void StHbtV0::SetptV0( float x){mPtV0 = x;}          
inline void StHbtV0::SetptotV0( float x){mPtotV0 = x;}
inline void StHbtV0::SetptPos( float x){mPtPos = x;}
inline void StHbtV0::SetptotPos( float x){mPtotPos = x;}    
inline void StHbtV0::SetptNeg( float x){ mPtNeg= x;}    
inline void StHbtV0::SetptotNeg( float x){ mPtotNeg= x;}
inline void StHbtV0::SetidNeg(const unsigned short& s){ mKeyNeg= s;}
inline void StHbtV0::SetidPos(const unsigned short& s){ mKeyPos= s;}
inline void StHbtV0::SetkeyNeg(const unsigned short& s){ mKeyNeg= s;}
inline void StHbtV0::SetkeyPos(const unsigned short& s){ mKeyPos= s;}
inline void StHbtV0::SettpcHitsPos(const int& i){mTpcHitsPos=i;} 
inline void StHbtV0::SettpcHitsNeg(const int& i){mTpcHitsNeg=i;}
inline void StHbtV0::SetdedxNeg(float x){mDedxNeg=x;}
inline void StHbtV0::SeterrdedxNeg(float x){mErrDedxNeg=x;}//Gael 04Fev2002
inline void StHbtV0::SetlendedxNeg(float x){mLenDedxNeg=x;}//Gael 04Fev2002
inline void StHbtV0::SetdedxPos(float x){mDedxPos=x;}
inline void StHbtV0::SeterrdedxPos(float x){mErrDedxPos=x;}//Gael 04Fev2002
inline void StHbtV0::SetlendedxPos(float x){mLenDedxPos=x;}//Gael 04Fev2002
inline void StHbtV0::SetprimaryVertex(const StHbtThreeVector v) { mPrimaryVertex = v; }//Gael 24 Sept 02
#endif


















