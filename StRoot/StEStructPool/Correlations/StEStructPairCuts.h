/**********************************************************************
 *
 * $Id: StEStructPairCuts.h,v 1.21 2012/11/16 21:22:27 prindle Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Cut class for track-pair level quantities
 *
 *
 ***********************************************************************/
#ifndef __STEBYEPAIRCUTS_H
#define __STEBYEPAIRCUTS_H

#include "StEStructPairLUT.h"
#include "StEStructPool/AnalysisMaker/StEStructCuts.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"

#include "Stiostream.h"


class StEStructPairCuts : public StEStructCuts {

protected:


  CutName mdphiName;
  CutName mdetaName;
  CutName mgooddzdxyName;
  CutName mdmtName;
  CutName mqInvName;
  CutName mEntSepName;
  CutName mExitSepName;
  CutName mQualityName;
  CutName mMidTpcSepLSName;
  CutName mMidTpcSepUSName;
  CutName mHBTName;
  CutName mCoulombName;
  CutName mMergingName;
  CutName mMergingName2;
  CutName mCrossingName;
  CutName mCrossingName2;
  CutName mLUTName;
  CutName mpionMomentumName;
  CutName mKaonMomentumName;
  CutName mprotonMomentumName;

  CutName mpionOtherMassName;
  CutName mpionpionMassName;
  CutName mpionKaonMassName;
  CutName mpionprotonMassName;
  CutName mKaonOtherMassName;
  CutName mKaonKaonMassName;
  CutName mKaonprotonMassName;
  CutName mprotonOtherMassName;
  CutName mprotonprotonMassName;
  CutName mOtherOtherMassName;

  float mdphi[2];
  float mdeta[2];
  float mgooddzdxy[2];
  float mdmt[2];
  float mqInv[2];
  float mEntSep[2];
  float mExitSep[2];
  float mQuality[2];
  float mMidTpcSepLS[2];
  float mMidTpcSepUS[2];
  float mHBT[4];
  float mCoulomb[4];
  float mMerging[2];
  float mMerging2[2];
  float mCrossing[2];
  float mCrossing2[2];
  float mLUTParams[2];
  float mdEdxMomentumCut[4][2];
  float mToFMomentumCut[4][2];
  float mpionOtherMass[2];
  float mpionpionMass[2];
  float mpionKaonMass[2];
  float mpionprotonMass[2];
  float mKaonOtherMass[2];
  float mKaonKaonMass[2];
  float mKaonprotonMass[2];
  float mprotonOtherMass[2];
  float mprotonprotonMass[2];
  float mOtherOtherMass[2];

  bool  mdeltaPhiCut,    mdeltaEtaCut,    mGooddeltaZdeltaXYCut, mdeltaMtCut;
  bool  mqInvCut,        mEntSepCut,      mExitSepCut,   mQualityCut;
  bool  mMidTpcSepLSCut, mMidTpcSepUSCut;
  bool  mHBTCut,         mCoulombCut,     mMergingCut,   mCrossingCut,     mMergingCut2,   mCrossingCut2,   mLUTCut;
  bool  mpionMomentumCut, mKaonMomentumCut, mprotonMomentumCut;
  bool  mpionOtherMassCut, mpionpionMassCut, mpionKaonMassCut, mpionprotonMassCut;
  bool  mKaonOtherMassCut, mKaonKaonMassCut, mKaonprotonMassCut, mprotonOtherMassCut;
  bool  mprotonprotonMassCut, mOtherOtherMassCut;

  int  mdphiCounter[4], mdetaCounter[4], mgooddzdxyCounter[4], mdmtCounter[4];
  int  mqInvCounter[4],   mEntSepCounter[4],  mExitSepCounter[4],  mQualityCounter[4];
  int  msplitLSCounter[4],  msplitUSCounter[4];
  int  mHBTCounter[4],  mCoulombCounter[4], mMergingCounter[4], mCrossingCounter[4], mMergingCounter2[4], mCrossingCounter2[4], mLUTCounter[4];
  int  mpionMomentumCounter[4], mKaonMomentumCounter[4], mprotonMomentumCounter[4];
  int  mpionOtherMassCounter[4], mpionpionMassCounter[4], mpionKaonMassCounter[4], mpionprotonMassCounter[4];
  int  mKaonOtherMassCounter[4], mKaonKaonMassCounter[4], mKaonprotonMassCounter[4], mprotonOtherMassCounter[4];
  int  mprotonprotonMassCounter[4], mOtherOtherMassCounter[4];

  // if data stored for subsequent use.... e.g. next cut, histogramming
  float  mdeltaPhi, mdeltaEta, mdeltaMt;
  float  mqInvariant,  mEntranceSeparation, mExitSeparation, mQualityVal;
  float  mMidTpcSeparationLS, mMidTpcSeparationUS;
  float  mBField;                   // magnetic field (kilogauss as MuDst)

  int  mType;
  unsigned long mapMask0;
  unsigned long mapMask1;
  unsigned long bitI[32];

  int mretVal; //! just a dummy holder used a lot

  int  mcutMode;  // cut space definitions

  float mZoffset; // Z offset in primary vertex; correction for mixed event pair separations 

  void init();
  void initCuts();
  void initNames();

public:

  StEStructTrack* mTrack1;
  StEStructTrack* mTrack2;

  StEStructPairCuts();
  StEStructPairCuts(const char* cutFileName);
  virtual ~StEStructPairCuts();

  // Look Up Table object.
  StEStructPairLUT *mLUT;

  virtual bool loadBaseCuts(const char* name, const char** vals, int nvals);
  virtual void loadUserCuts(const char* name, const char** vals, int nvals);
  virtual void printCutStats(ostream& ofs);
  virtual void printCutCounts(ostream& ofs, const char* cutType,int c1, int c2);

  float BField() const { return mBField; };
  void SetBField(const float bfield){ mBField=bfield; };

  // StEStructPairCuts stuff

  const  StEStructTrack*      Track1() const;
  const  StEStructTrack*      Track2() const;
       void                 SetTrack1(const StEStructTrack* trkPtr);
       void                 SetTrack2(const StEStructTrack* trkPtr);


//-----For Auto Correlationn Study
       float                 DeltaMt()   const;
       float                 DeltaXt()   const;
       float                 DeltaYt()   const;
       float                 DeltaYt(float mass1,float mass2)   const;
       float                 DeltaEta()   const;
       float                 DeltaEta(float mass1,float mass2)   const;
       float                 DeltaPhi()   const;
       float                 DeltaPt()   const;
       float                 SigmaMt()   const;
       float                 SigmaXt()   const;
       float                 SigmaYt()   const;
       float                 SigmaYt(float mass1,float mass2)   const;
       float                 SigmaEta()   const;
       float                 SigmaEta(float mass1,float mass2)   const;
       float                 SigmaPhi()   const;
       float                 SigmaPt()    const;
//--- For Cut Bin Definitions (found these in more than 1 place ...)
       bool                  awaySide();
       bool                  sameSide();


//-----For HBT Study
       StLorentzVectorF  fourMomentumDiff() const;
       StLorentzVectorF  fourMomentumSum()  const;
       float                 qInv()   const;
       float                 kT()     const;
       float                 mInv()   const;

//-----For Pair Cut
       double                  quality()                        const;
       double                  OpeningAngle()                   const;
       double                  NominalTpcEntranceSeparation()   const;
       double                  NominalTpcXYEntranceSeparation() const;
       double                  NominalTpcZEntranceSeparation()  const;
       double                  MidTpcXYSeparation()             const;
       double                  MidTpcZSeparation()              const;
       double                  MidTpcSeparation()               const;
       double                  OuterMidTpcXYSeparation()        const;
       double                  OuterMidTpcZSeparation()         const;
       double                  OuterMidTpcSeparation()          const;
       double                  NominalTpcExitSeparation()       const;
       double                  NominalTpcXYExitSeparation()     const;
       double                  NominalTpcZExitSeparation()      const;    
       double                  TpcZEndcapExitSeparation()       const;    
       double                  TpcRadialEndCapSeparation() const;
       double                  NominalTpcAvgXYSeparation()      const;
       double                  NominalTpcAvgZSeparation()       const;
       bool                    TracksCrossInPhi()               const;

       void                    SetZoffset(float Zoffset) { mZoffset = Zoffset; };
       float                   GetZoffset() { return mZoffset; };

       void setPairType(int type);
       int  cutPair();
       int  cutPair(bool doHistograms);
       int  cutPairHistograms();

       // explicit cut coding... 
       // for Pair cuts we have to break the model of
       // non-duplicate codes simply because even checking a 
       // bool for each cut is expensive when done N1*N2*Ncut times
       
       /* because of the expense 1st check wether we need to do further */

       int  goodDeltaXY();
       int  goodDeltaZ();
       int  goodDeltaMt();
       
       int  cutDeltaPhi();
       int  cutDeltaEta();
       int  cutDeltaMt();
       int  cutqInvORNominalEntranceSep();
       int  cutqInv();
       int  cutEntranceSep();
       int  cutMidTpcSepUS();
       int  cutMidTpcSepLS();
       int  cutExitSep();
       int  cutQuality();
       int  cutHBT();
       int  cutCoulomb();
       int  cutMerging();
       int  cutCrossing();
       int  cutMerging2();
       int  cutCrossing2();
       int  cutLUT();
       int  cutMass();
       
       // calls above set but fills histogramming variables
       int  cutDeltaPhiH();
       int  cutDeltaEtaH();
       int  cutDeltaMtH();
       int  cutqInvH();
       int  cutEntranceSepH();
       int  cutMidTpcSepUSH();
       int  cutMidTpcSepLSH();
       int  cutExitSepH();
       int  cutQualityH();

       // for new GoodPair study
       
       int correlationDepth();



  ClassDef(StEStructPairCuts,1)
};

inline void StEStructPairCuts::loadUserCuts(const char* name, const char** vals, int nvals){ }

inline void StEStructPairCuts::setPairType(int type) { mType=type; }
inline int  StEStructPairCuts::cutPair(bool doHistograms){
  if(!doHistograms) return cutPair();
  return cutPairHistograms();
}


inline void StEStructPairCuts::SetTrack1(const StEStructTrack* trkPtr){
  mTrack1=(StEStructTrack*)trkPtr;
}

inline void StEStructPairCuts::SetTrack2(const StEStructTrack* trkPtr){
  mTrack2=(StEStructTrack*)trkPtr;
}

inline const StEStructTrack* StEStructPairCuts::Track1() const {return mTrack1;}
inline const StEStructTrack* StEStructPairCuts::Track2() const {return mTrack2;}

inline float StEStructPairCuts::SigmaMt() const {
  return mTrack1->FourMomentum().mt()+mTrack2->FourMomentum().mt();
}
inline float StEStructPairCuts::SigmaXt() const {
  return mTrack1->Xt()+mTrack2->Xt();
}
inline float StEStructPairCuts::SigmaYt() const {
  return mTrack1->Yt()+mTrack2->Yt();
}
inline float StEStructPairCuts::SigmaYt(float mass1, float mass2) const {
  return mTrack1->Yt(mass1)+mTrack2->Yt(mass2);
}
inline float StEStructPairCuts::SigmaEta() const {
  return mTrack1->Eta()+mTrack2->Eta();
}
inline float StEStructPairCuts::SigmaEta(float mass1, float mass2) const {
  return mTrack1->Eta(mass1)+mTrack2->Eta(mass2);
}
inline float StEStructPairCuts::SigmaPhi() const {
  return mTrack1->Phi()+mTrack2->Phi();
}

inline float StEStructPairCuts::SigmaPt() const {
  return mTrack1->Pt()+mTrack2->Pt();
}

inline float StEStructPairCuts::DeltaMt() const {
  return mTrack1->FourMomentum().mt()-mTrack2->FourMomentum().mt();
}
inline float StEStructPairCuts::DeltaXt() const {
  return mTrack1->Xt()-mTrack2->Xt();
}
inline float StEStructPairCuts::DeltaYt() const {
  return mTrack1->Yt()-mTrack2->Yt();
}
inline float StEStructPairCuts::DeltaYt(float mass1, float mass2) const {
  return mTrack1->Yt(mass1)-mTrack2->Yt(mass2);
}
inline float StEStructPairCuts::DeltaEta() const {
  return mTrack1->Eta()-mTrack2->Eta();
}
inline float StEStructPairCuts::DeltaEta(float mass1, float mass2) const {
  return mTrack1->Eta(mass1)-mTrack2->Eta(mass2);
}
inline float StEStructPairCuts::DeltaPhi() const {
  return mTrack1->Phi()-mTrack2->Phi();
}

inline float StEStructPairCuts::DeltaPt() const {
  return mTrack1->Pt()-mTrack2->Pt();
}
inline float StEStructPairCuts::mInv() const {
  return abs(mTrack1->FourMomentum()+mTrack2->FourMomentum());
}

inline float StEStructPairCuts::kT() const {
  return 0.5*(mTrack1->FourMomentum()+mTrack2->FourMomentum()).perp();
}

inline float StEStructPairCuts::qInv() const {
  return -1.0*(mTrack1->FourMomentum()-mTrack2->FourMomentum()).m();
}

inline bool StEStructPairCuts::awaySide() {
    mdeltaPhi=fabs(DeltaPhi());   
    if((mdeltaPhi>M_PI/2.0) && (mdeltaPhi<1.5*M_PI)) return true;
    return false;
}

inline bool StEStructPairCuts::sameSide() {
    mdeltaPhi=fabs(DeltaPhi());   
    if((mdeltaPhi < M_PI/2.0) || (mdeltaPhi> (1.5*M_PI))) return true;
    return false;
}

// Both goodDeltaPhi and goodDeltaEta were looking for their deltas to
// be smaller than the cut value. To speed up the pairCuts we want
// to quickly identify pairs that will not be cut. Also, cuts are actually done
// on difference in Z and XY directions. If either of these are too big we don't
// want to bother trying rest of pair cuts that we know won't reject pair.
inline int StEStructPairCuts::goodDeltaXY() {
    if ( mGooddeltaZdeltaXYCut &&
        MidTpcXYSeparation() > mgooddzdxy[1] &&
        OuterMidTpcXYSeparation() > mgooddzdxy[1] ) {
        return 1;
    }
    return 0;
}

// For OuterMidTPC we have the complication of tracks exiting via endcap.
// OuterMidTpcZSeparation() will return a negative number in this case and
// the pair will not be flagged to skip the pair cuts.
// I expect a small fraction of pairs are like this so we keep this code
// simple and hopefully fast, passing a few more pairs on to detailed
// pair cuts than are really necessary. 
//
// I find that the quality cuts can be unexpectedly high (therefore rejecting
// the pair) when one track exits via the endcap while the other does not.
// I guess we need to do this cut properly after all.
inline int StEStructPairCuts::goodDeltaZ() {
    if ( mGooddeltaZdeltaXYCut &&
         MidTpcZSeparation() > mgooddzdxy[0] &&
         OuterMidTpcZSeparation() > mgooddzdxy[0] ) {
        return 1;
    }
    double outerDelZ;
    if ( mGooddeltaZdeltaXYCut ) {
        if ((outerDelZ = OuterMidTpcZSeparation()) < 0) {
            return 1;
        } else if ((MidTpcZSeparation() > mgooddzdxy[0]) &&
                   (outerDelZ > mgooddzdxy[0] )) {
            return 1;
        }
    }
    return 0;
}

inline int StEStructPairCuts::goodDeltaMt(){
  if(mdeltaMtCut &&
     ( (mdeltaMt=fabs(DeltaMt())) <mdmt[1] )) return 0;
  return 1;
}


inline int StEStructPairCuts::cutDeltaPhi(){
  if ( mdeltaPhiCut &&
       ( (mdeltaPhi=DeltaPhi()) <mdphi[0] || mdeltaPhi>mdphi[1])  
      ) return ++(mdphiCounter[mType]);
  return 0;
}

inline int StEStructPairCuts::cutDeltaEta(){
  if(mdeltaEtaCut &&
     ( (mdeltaEta=DeltaEta()) <mdeta[0] || mdeltaEta>mdeta[1]) 
     ) return ++(mdetaCounter[mType]);
  return 0;
}


inline int StEStructPairCuts::cutDeltaMt(){
  if(mdeltaMtCut &&
     ( (mdeltaMt=DeltaMt()) <mdmt[0] || mdeltaMt>mdmt[1]) 
     ) return ++(mdmtCounter[mType]);
  return 0;
}

inline int StEStructPairCuts::cutqInvORNominalEntranceSep(){
  /* small qInv and entrance cut */

  if( mqInvCut && mEntSepCut &&
      (  (mqInvariant=qInv()) <mqInv[0]  &&
	 ((mEntranceSeparation=NominalTpcEntranceSeparation())<mEntSep[0]))
      ) return ++(mqInvCounter[mType]);
  return 0;
}

inline int StEStructPairCuts::cutqInv(){
  if( mqInvCut && 
      (  (mqInvariant=qInv())<mqInv[0] 
	 || mqInvariant>mqInv[1] 
	 )                  
      ) return ++(mqInvCounter[mType]);
  return 0;
}

inline int StEStructPairCuts::cutEntranceSep(){
  if( mEntSepCut && 
      ( (mEntranceSeparation=NominalTpcEntranceSeparation())<mEntSep[0] 
	|| mEntranceSeparation>mEntSep[1]  
	)                  
      ) return ++(mEntSepCounter[mType]);
  return 0;
}


inline int StEStructPairCuts::cutMidTpcSepUS(){
  if( mMidTpcSepUSCut ){
    double x1=mMidTpcSeparationUS=MidTpcSeparation();
    double x2=NominalTpcEntranceSeparation();
    double x3=NominalTpcExitSeparation();
    if(((x1+x2+x3)/3.)>mMidTpcSepUS[1])return 0; // ok, average is large
    if( x1<x2 && x1<x3 && x1<mMidTpcSepUS[0]) return ++(msplitUSCounter[mType]);
  }
  return 0;
}

inline int StEStructPairCuts::cutMidTpcSepLS(){
  if( mMidTpcSepLSCut ){
    double x1=mMidTpcSeparationLS=MidTpcSeparation();
    double x2=NominalTpcEntranceSeparation();
    double x3=NominalTpcExitSeparation();
    if(((x1+x2+x3)/3.)>mMidTpcSepLS[1])return 0; //ok, average is large
    if( x1<x2 && x1<x3 && x1<mMidTpcSepLS[0]) return ++(msplitLSCounter[mType]);  }
  return 0;
}


inline int StEStructPairCuts::cutExitSep(){
   if( mExitSepCut && 
       ( (mExitSeparation=NominalTpcExitSeparation()) <mExitSep[0])
       // || mExitSeparation>mExitSep[1] )
       ) return ++(mExitSepCounter[mType]);
   return 0;
}

inline int StEStructPairCuts::cutQuality() {
    if( mQualityCut && 
      ((mQualityVal=quality()) <mQuality[0] || mQualityVal>mQuality[1] )) {
        return ++(mQualityCounter[mType]);
    }
    return 0;
}

inline int StEStructPairCuts::cutHBT(){
  if(!mHBTCut || mType==1 || mType==3) return 0;  // HBT applies only to LS pairs
  float dpt = fabs(DeltaPt());  // DeltaPt is signed,
  float deta = fabs(DeltaEta());  // now DeltaEta and DeltaPhi are signed...
  float dphi = fabs(DeltaPhi());  // 
  if ( deta<mHBT[0] && dphi<mHBT[1] && dpt<mHBT[2] 
       && mTrack1->Pt()<mHBT[3] && mTrack2->Pt()<mHBT[3] )
    return ++(mHBTCounter[mType]);
  return 0;
}

inline int StEStructPairCuts::cutCoulomb(){
  if(!mCoulombCut) return 0;
  float dpt = fabs(DeltaPt());  // DeltaPt is signed,
  float deta = fabs(DeltaEta());  // now DeltaEta and DeltaPhi are signed...
  float dphi = fabs(DeltaPhi());  //
  if ( deta<mCoulomb[0] && dphi<mCoulomb[1] && dpt<mCoulomb[2]
       && mTrack1->Pt()<mCoulomb[3] && mTrack2->Pt()<mCoulomb[3] )
    return ++(mCoulombCounter[mType]);
  return 0;
}

inline int StEStructPairCuts::cutMerging(){
  if(!mMergingCut) return 0;
  if ( NominalTpcAvgXYSeparation()<mMerging[0] && 
       NominalTpcAvgZSeparation() <mMerging[1])
    return ++(mMergingCounter[mType]);
  return 0;
}

inline int StEStructPairCuts::cutCrossing(){
  // Crossing geometries are a little complicated...
  // Pair   Cut if:   
  // + +    dPt & dPhi opp. sign
  // - -    dPt & dPhi same sign
  // + -    dPhi>0
  // - +    dPhi<0  
  // Above table is for positive field, for reversed field switch charge signs
  // I'm going to explicity list each case rather then try to be clever here.

  if(!mCrossingCut) return 0;  // make the common case fast
  mretVal = 0;
  if ( MidTpcXYSeparation()<mCrossing[0] && MidTpcZSeparation()<mCrossing[1])  {
    float dphi = mTrack1->Phi()-mTrack2->Phi(); // signed DeltaPhi
    float dpt =  mTrack1->Pt()- mTrack2->Pt();  // signed DeltaPt
    if (mType==1 || mType==3) {   // US pair
      if(mBField>=0) { // pos field
  if (mTrack1->Charge()>0 && dphi>0)  mretVal = 1;  // + -
  if (mTrack1->Charge()<0 && dphi<0)  mretVal = 1;  // - + 
      } else {              // rev field
  if (mTrack1->Charge()>0 && dphi<0)  mretVal = 1;  // rev +- : same as -+ above
        if (mTrack1->Charge()<0 && dphi>0)  mretVal = 1;  // rev -+ : same as +- above
      } 
    } else {                      // LS pair
      if(mBField>=0) { // pos field
  if (mTrack1->Charge()>0 && dphi*dpt<0)  mretVal = 1;  // + +
  if (mTrack1->Charge()<0 && dphi*dpt>0)  mretVal = 1;  // - -
      } else {              // rev field
  if (mTrack1->Charge()>0 && dphi*dpt>0)  mretVal = 1;  // rev ++ : same as --
        if (mTrack1->Charge()<0 && dphi*dpt<0)  mretVal = 1;  // rev -- : same as ++
      }
    }
  }

    // This should do what that does. Might be faster? 
    // Problem is that although signbit is not zero if the bit is set it is not actually
    // guaranteed to be 1.
//    mretVal = 0;
//    if ( MidTpcXYSeparation()<mCrossing[0] && MidTpcZSeparation()<mCrossing[1])  {
//        int dphi = 1-2*signbit(mTrack1->Phi()-mTrack2->Phi()); // sign of DeltaPhi
//        int chrg = 1-2*signbit(mBField*mTrack1->Charge());
//        if (mType==1 || mType==3) {   // US pair
//            if (chrg == dphi) mretVal = 1;
//        } else {
//            int dpt = 1-2*signbit(mTrack1->Pt()- mTrack2->Pt());  // sign of DeltaPt
//            if (chrg != dpt*dphi) mretVal = 1;
//        }
//    }
    if (mretVal==0) return 0;
    return ++(mCrossingCounter[mType]);
}

inline int StEStructPairCuts::cutMerging2() {
    // If tracks ever get close we cut them. Ignore TPC entrance.
    //  Doesn't seem to take care of crossing tracks.
    // Turns out when one of the tracks leaves via the endcap the nominal
    // Z separation is -1. We use only the XY separation in this case.
    //
    // Ooops. Looks like that final case of only using the XY separation when
    // the nominal Z separation is negative allows cutting pairs where one track leaves
    // the TPC endcap and the other has arbitrary eta. Not what I wanted.
    // Use difference in radius instead of Z when track leaves via endcap.
    // Also, if track does not make it to a given radius ignore the cut there.
    if (!mMergingCut2) {
        return 0;
    }
    // For normal eta and Z-Vertex cuts all tracks will exceed the MidTPC radius before
    // passing the Z position of the endcap.
    if ( (MidTpcZSeparation()<mMerging2[0]) && (MidTpcXYSeparation()<mMerging2[1]) ) {
        return ++(mMergingCounter2[mType]);
    }
    // Possible for track to pass through endcap before getting to radius of OuterMidTPC.
    // If tracks go through different endcaps do not cut.
    // If only one track goes through an endcap or they go through same endcap then
    // replace z difference cut with radial difference cut.
    // Not sure XY separation cut is optimal in endcap case.
    if ( OuterMidTpcZSeparation()>0 ) {
        if ( OuterMidTpcZSeparation()<mMerging2[0] && OuterMidTpcXYSeparation()<mMerging2[1] ) {
            return ++(mMergingCounter2[mType]);
        }
    } else if ( OuterMidTpcZSeparation()>-3 ) {
        if ( (TpcRadialEndCapSeparation()<mMerging2[0]) && (OuterMidTpcXYSeparation()<mMerging2[1]) ) {
            return ++(mMergingCounter2[mType]);
        }
    }
    if (NominalTpcZExitSeparation() > 0) {
        if ( (NominalTpcZExitSeparation()<mMerging2[0]) && (NominalTpcXYExitSeparation()<mMerging2[1]) ) {
            return ++(mMergingCounter2[mType]);
        }
    } else if ( NominalTpcZExitSeparation() > -3) {
        if ( (TpcRadialEndCapSeparation()<mMerging2[0]) && (NominalTpcXYExitSeparation()<mMerging2[1]) ) {
            return ++(mMergingCounter2[mType]);
        }
    }
    return 0;
}
inline int StEStructPairCuts::cutCrossing2() {
    // For tracks that cross.
    // This removes a slot narrow in Z separation but perhaps wide in XY separation.
    // I find that low-y_t, high-y_t pairs can be far apart at the middle of the
    // tpc but close together near the outer radius, so require large z separation
    // at both radii.

    if (!mCrossingCut2) {
        return 0;
    }
    // If tracks are far enough apart in Z don't bother to look for crossing.
    // If one (or both) tracks pass through endcap before OuterMid point
    // we ignore that check.
    if ( OuterMidTpcZSeparation()>0 ) {
        if ( MidTpcZSeparation()>mCrossing2[0] && OuterMidTpcZSeparation()>mCrossing2[1] ) {
            return 0;
        }
    } else if ( OuterMidTpcZSeparation()==-3 ) {
        return 0;
    } else {
        if ( MidTpcZSeparation()>mCrossing2[0] ) {
            return 0;
        }
    }
// Following is the old cut. Problem for low pt tracks (which probably makes almost
//  no difference overall.
//    if ( (MidTpcZSeparation()>mCrossing2[0]) && (OuterMidTpcZSeparation()>mCrossing2[1]) ) {
//        return 0;
//    }
    if (!TracksCrossInPhi()) {
        return 0;
    }
    return ++(mCrossingCounter2[mType]);
}
inline int StEStructPairCuts::cutLUT() {
    // Use a Look Up Table to determine which pairs get close enough
    // to affect each other.

    if (!mLUTCut) {
        return 0;
    }
    double delPhi = mTrack1->Phi() - mTrack2->Phi();
    double delEta = mTrack1->Eta() - mTrack2->Eta();
    if (mLUT->cut(mTrack1->Curvature(),mTrack2->Curvature(),delPhi,delEta)) {
        return ++(mLUTCounter[mType]);
    }
    return 0;
}

inline int StEStructPairCuts::cutMass() {
    if (!(mpionOtherMassCut    | mpionpionMassCut | mpionKaonMassCut   | mpionprotonMassCut  |
          mKaonOtherMassCut    | mKaonKaonMassCut | mKaonprotonMassCut | mprotonOtherMassCut |
          mprotonprotonMassCut | mOtherOtherMassCut)) {
        return 0;
    }

    int mode[4][4] = {{9, 0, 4, 7}, {0, 1, 2, 3}, {4, 2, 5, 6}, {7, 3, 6, 8}};
    int it1 = Track1()->PID();
    int it2 = Track2()->PID();
    if (it1 < 0 || 3 < it1) {
        return 0;
    }
    if (it2 < 0 || 3 < it2) {
        return 0;
    }
    int iBin = mode[it1][it2];

    double e, e1, e2, p1, p2, p[3], m, m1, m2;
    p1   = Track1()->Ptot();
    p2   = Track2()->Ptot();
    p[0] = Track1()->Px() + Track2()->Px();
    p[1] = Track1()->Py() + Track2()->Py();
    p[2] = Track1()->Pz() + Track2()->Pz();
    float Mass[]  = {0.1396,  0.1396,  0.497, 0.9383};
    float Mass2[] = {0.01949, 0.01949, 0.247, 0.880};
    if (9 == iBin) {
        // For o-o try using m1 = m2 = 0.
        m1 = 0;
        m2 = 0;
        e1 = p1;
        e2 = p2;
    } else {
        m1 = Mass[it1];
        m2 = Mass[it2];
        e1 = sqrt(p1*p1 + Mass2[it1]);
        e2 = sqrt(p2*p2 + Mass2[it2]);
    }
    e = e1 + e2;
    m = sqrt(e*e - p[0]*p[0] - p[1]*p[1] - p[2]*p[2]);
    // Cut on invariant mass to keep or exclude resonances.
    switch(iBin) {
        case 0: {  // pion-Other
            if (mpionOtherMassCut) {
                if (mpionOtherMass[0] < mpionOtherMass[1]) { // Exclude mass within window
                    if ((mpionOtherMass[0] < m) && (m < mpionOtherMass[1])) {
                        return ++(mpionOtherMassCounter[mType]);
                    }
                } else {  // Keep only pairs within mass windo
                    if ((m < mpionOtherMass[1]) || (mpionOtherMass[0] < m)) {
                        return ++(mpionOtherMassCounter[mType]);
                    }
                }
            }
            break;
        }
        case 1: {  // pion-pion
            if (mpionpionMassCut) {
                if (mpionpionMass[0] < mpionpionMass[1]) { // Exclude mass within window
                    if ((mpionpionMass[0] < m) && (m < mpionpionMass[1])) {
                        return ++(mpionpionMassCounter[mType]);
                    }
                } else {  // Keep only pairs within mass windo
                    if ((m < mpionpionMass[1]) || (mpionpionMass[0] < m)) {
                        return ++(mpionpionMassCounter[mType]);
                    }
                }
            }
            break;
        }
        case 2: {  // pion-Kaon
            if (mpionKaonMassCut) {
                if (mpionKaonMass[0] < mpionKaonMass[1]) { // Exclude mass within window
                    if ((mpionKaonMass[0] < m) && (m < mpionKaonMass[1])) {
                        return ++(mpionKaonMassCounter[mType]);
                    }
                } else {  // Keep only pairs within mass windo
                    if ((m < mpionKaonMass[1]) || (mpionKaonMass[0] < m)) {
                        return ++(mpionKaonMassCounter[mType]);
                    }
                }
            }
            break;
        }
        case 3: {  // pion-proton
            if (mpionprotonMassCut) {
                if (mpionprotonMass[0] < mpionprotonMass[1]) { // Exclude mass within window
                    if ((mpionprotonMass[0] < m) && (m < mpionprotonMass[1])) {
                        return ++(mpionprotonMassCounter[mType]);
                    }
                } else {  // Keep only pairs within mass windo
                    if ((m < mpionprotonMass[1]) || (mpionprotonMass[0] < m)) {
                        return ++(mpionprotonMassCounter[mType]);
                    }
                }
            }
            break;
        }
        case 4: {  // Kaon-Other
            if (mKaonOtherMassCut) {
                if (mKaonOtherMass[0] < mKaonOtherMass[1]) { // Exclude mass within window
                    if ((mKaonOtherMass[0] < m) && (m < mKaonOtherMass[1])) {
                        return ++(mKaonOtherMassCounter[mType]);
                    }
                } else {  // Keep only pairs within mass windo
                    if ((m < mKaonOtherMass[1]) || (mKaonOtherMass[0] < m)) {
                        return ++(mKaonOtherMassCounter[mType]);
                    }
                }
            }
            break;
        }
        case 5: {  // Kaon-Kaon
            if (mKaonKaonMassCut) {
                if (mKaonKaonMass[0] < mKaonKaonMass[1]) { // Exclude mass within window
                    if ((mKaonKaonMass[0] < m) && (m < mKaonKaonMass[1])) {
                        return ++(mKaonKaonMassCounter[mType]);
                    }
                } else {  // Keep only pairs within mass windo
                    if ((m < mKaonKaonMass[1]) || (mKaonKaonMass[0] < m)) {
                        return ++(mKaonKaonMassCounter[mType]);
                    }
                }
            }
            break;
        }
        case 6: {  // Kaon-proton
            if (mKaonprotonMassCut) {
                if (mKaonprotonMass[0] < mKaonprotonMass[1]) { // Exclude mass within window
                    if ((mKaonprotonMass[0] < m) && (m < mKaonprotonMass[1])) {
                        return ++(mKaonprotonMassCounter[mType]);
                    }
                } else {  // Keep only pairs within mass windo
                    if ((m < mKaonprotonMass[1]) || (mKaonprotonMass[0] < m)) {
                        return ++(mKaonprotonMassCounter[mType]);
                    }
                }
            }
            break;
        }
        case 7: {  // proton-Other
            if (mprotonOtherMassCut) {
                if (mprotonOtherMass[0] < mprotonOtherMass[1]) { // Exclude mass within window
                    if ((mprotonOtherMass[0] < m) && (m < mprotonOtherMass[1])) {
                        return ++(mprotonOtherMassCounter[mType]);
                    }
                } else {  // Keep only pairs within mass windo
                    if ((m < mprotonOtherMass[1]) || (mprotonOtherMass[0] < m)) {
                        return ++(mprotonOtherMassCounter[mType]);
                    }
                }
            }
            break;
        }
        case 8: {  // proton-proton
            if (mprotonprotonMassCut) {
                if (mprotonprotonMass[0] < mprotonprotonMass[1]) { // Exclude mass within window
                    if ((mprotonprotonMass[0] < m) && (m < mprotonprotonMass[1])) {
                        return ++(mprotonprotonMassCounter[mType]);
                    }
                } else {  // Keep only pairs within mass windo
                    if ((m < mprotonprotonMass[1]) || (mprotonprotonMass[0] < m)) {
                        return ++(mprotonprotonMassCounter[mType]);
                    }
                }
            }
            break;
        }
        case 9: {  // Other-Other
            if (mOtherOtherMassCut) {
                if (mOtherOtherMass[0] < mOtherOtherMass[1]) { // Exclude mass within window
                    if ((mOtherOtherMass[0] < m) && (m < mOtherOtherMass[1])) {
                        return ++(mOtherOtherMassCounter[mType]);
                    }
                } else {  // Keep only pairs within mass windo
                    if ((m < mOtherOtherMass[1]) || (mOtherOtherMass[0] < m)) {
                        return ++(mOtherOtherMassCounter[mType]);
                    }
                }
            }
            break;
        }
    }
    return 0;
}

inline int StEStructPairCuts::cutDeltaPhiH(){
  if(!mdeltaPhiCut) return 0;
  mretVal=cutDeltaPhi();
  mvalues[mdphiName.idx]=mdeltaPhi;
  return mretVal;
}

inline int StEStructPairCuts::cutDeltaEtaH(){
  if(!mdeltaEtaCut){
    mdeltaEta=DeltaEta(); // still needed for other cuts
    return 0;
  }
  mretVal=cutDeltaEta();
  mvalues[mdetaName.idx]=mdeltaEta;
  return mretVal;
}

inline int StEStructPairCuts::cutDeltaMtH(){
  if(!mdeltaMtCut) return 0;
  mretVal=cutDeltaMt();
  mvalues[mdmtName.idx]=mdeltaMt;
  return mretVal;
}

inline int StEStructPairCuts::cutqInvH(){
  if(!mqInvCut) return 0;
  mretVal=cutqInv();
  mvalues[mqInvName.idx]=mqInvariant;
  return mretVal;
}

inline int StEStructPairCuts::cutEntranceSepH(){
  if(!mEntSepCut) return 0;
  mretVal=cutEntranceSep();
  mvalues[mEntSepName.idx]=mEntranceSeparation;
  return mretVal;
}

inline int StEStructPairCuts::cutMidTpcSepLSH(){
  if(!mMidTpcSepLSCut) return 0;
  mretVal=cutMidTpcSepLS();
  mvalues[mMidTpcSepLSName.idx]=mMidTpcSeparationLS;
  return mretVal;
}

inline int StEStructPairCuts::cutMidTpcSepUSH(){
  if(!mMidTpcSepUSCut) return 0;
  mretVal=cutMidTpcSepUS();
  mvalues[mMidTpcSepUSName.idx]=mMidTpcSeparationUS;
  return mretVal;
}


inline int StEStructPairCuts::cutExitSepH(){
  if(!mExitSepCut) return 0;
  mretVal=cutExitSep();
  mvalues[mExitSepName.idx]=mExitSeparation;
  return mretVal;
}

inline int StEStructPairCuts::cutQualityH(){
  if(!mQualityCut) return 0;
  mretVal=cutQuality();
  mvalues[mQualityName.idx]=mQualityVal;
  return mretVal;
}


inline int StEStructPairCuts::correlationDepth(){

  unsigned int am1=mTrack1->TopologyMapData(0)&65535;
  unsigned int am2=mTrack2->TopologyMapData(0)&65535;
  if(am1==0 || am2==0) return 0;
  if(am1==am2) return 1;

  unsigned int msk=(65535<<16);

  unsigned int bm1=mTrack1->TopologyMapData(0)&msk;
  unsigned int bm2=mTrack2->TopologyMapData(0)&msk;
  bm1=(16>>bm1);
  bm2=(16>>bm2);

  if(am1==bm2 || bm1==am2) return 2;
  if(bm1==bm2){
    if(bm1==0) return 0;
    return 3;
  }

  unsigned int cm1=mTrack1->TopologyMapData(1)&65535;
  unsigned int cm2=mTrack2->TopologyMapData(1)&65535;
  if(cm1==cm2 && cm1==0) return 0;
 
  if(am1==cm2 || am2==cm1) return 4;
  if( ((bm1==cm2) && bm1!=0) || ((bm2==cm1)&&(bm2!=0)) ) return 4;
  if( cm1==cm2) return 5;

  unsigned int dm1=mTrack1->TopologyMapData(1)&msk;
  unsigned int dm2=mTrack2->TopologyMapData(1)&msk;
  dm1=(16>>dm1);
  dm2=(16>>dm2);
  if(dm1==dm2 && dm1==0) return 0;

  if(am1==dm2 || am2==dm1) return 6;
  if( ((bm1==dm2) && bm1!=0) || ((bm2==dm1)&&(bm2!=0)) ) return 6;
  if( ((cm1==dm2) && cm1!=0) || ((cm2==dm1)&&(cm2!=0)) ) return 6;
  if( dm1==dm2 && dm1!=0) return 7;

  return 0;
}



#endif

/***********************************************************************
 *
 * $Log: StEStructPairCuts.h,v $
 * Revision 1.21  2012/11/16 21:22:27  prindle
 * 2ptCorrelations: SS, AS histograms.  Get eta limits from cuts. Fit PtAll histogram. Add histograms to keep track of eta, phi limits. A few more histograms
 * Binning: Add quality cut.
 * CutBin: modify mode9
 * PairCuts: modify goodDeltaZ for case of one track leaving via endcap.
 *
 * Revision 1.20  2010/09/02 21:24:08  prindle
 *   2ptCorrelations: Fill histograms for event mixing information
 *                    Option for common mixing buffer
 *                    Switch to selectively fill QInv histograms (which take a long time)
 *   CutBin: Moved PID code to Track class from Pair class. Needed to update this code.
 *   PairCuts: Moved PID code from here to Track class.
 *             Removed unnecessary creation of StThreeVector which seem to take a long time
 *             Add ToF momentum cuts, modify dEdx momentum cuts. (Now allow dEdx to be
 *             be identified up to 15GeV/c, ToF up to 10GeV/c.)
 *
 * Revision 1.19  2010/03/02 21:45:28  prindle
 *   Had a problem with pair cuts when one track exited via endplate
 *   Calculate maxDEta properly
 *   Warning if you try turning histograms for pair cuts on
 *
 * Revision 1.18  2009/11/09 21:32:41  prindle
 * Fix warnings about casting char * to a const char * by redeclaring as const char *.
 *
 * Revision 1.17  2009/05/08 00:09:55  prindle
 * In 2ptCorrelations we added switches to select blocks of histograms to fill.
 * (See constructor in StEStruct2ptCorrelations.cxx)
 * Use a brute force method for checking crossing cuts. I had too many corner
 * cases with my clever check.
 * In Binning, change Yt limit and add methods for accessing number of histogram bins
 * to use (used in Support)
 *
 * Revision 1.16  2008/12/02 23:45:07  prindle
 * Changed switchYt to switchXX (etc.) to better reflect function.
 * Change minYt to 1.0 in Binning so YtYt histogram doesn't have empty lower bin (pt = 0.164 for yt = 1.0)
 * In CutBin: remove initPtBin
 *            add mode 8
 *            add notSymmetrized (used in Support)
 * Added LUT (Look Up Table) for pair cuts. Experimental for now.
 * Modified cutMerging2 (to look at track separation at a few radii)
 * and cutCrossing2 so it doesn't accidentally reject almost back to back tracks.
 *
 * Revision 1.15  2008/05/01 23:39:14  prindle
 *   I was using a triangular region for the merging cut. Decided a rectangular
 * region was safer.
 *
 * Revision 1.14  2008/03/19 22:06:01  prindle
 * Added doInvariantMass flag.
 * Added some plots in pairDensityHistograms.
 * SetZOffset used to only be done when doPairDensity was true.
 * Moved creating/copying pairDensity histograms to same place as other histograms.
 * Added cutBinHistMode
 * mode3 neck was defined as yt1<2.2 && yt2<2.2 (and not soft)
 *            now is        1.8<yt1<2.2  && 1.8<yt2<2.2
 * Added gooddzdxy, Merging2 and Crossing2 to pair cuts.
 *
 * Revision 1.13  2007/11/26 19:55:25  prindle
 * In 2ptCorrelations: Support for keeping all z-bins of selected centralities
 *                     Change way \hat{p_t} is calculated for parent distributions in pid case.
 *    Binning          Added parent binning (for \hat{p_t}
 *    CutBin:          Mode 5 extensively modified.
 *                     Added invariant mass cuts (probably a bad idea in general.)
 *
 * Revision 1.12  2007/01/26 17:17:11  msd
 * Implemented new binning scheme: dEta stored in array with bin centered at zero, dPhi array has bins centered at zero and pi.  Final DEtaDPhi has 25x25 bins with dPhi bin width of pi/12 so all major angles are centered in bins.
 *
 * Revision 1.11  2006/10/02 22:21:04  prindle
 * Store only quadrant of eta_Delta - phi_Delta array/histogram.
 * Store half of eta_Sigma - phi_Delta array/histogram.
 * This required modifications in Binning.
 * I had a bug in the pair loop (which left +- not fully symmetrized)
 * and had to make changes in cut bins for mode 5 (and 3 I think)
 * when I fixed this.
 * Also change crossing cut to use only two parameters, the sign of
 * the magnetic field being taken from the MuDst.
 *
 * Revision 1.10  2006/04/10 23:42:32  porter
 * Added sameSide() & awaySide() methods to PairCut (so only defined in 1 place)
 * and added the eta_delta weighting as a binned correctin defined by the eta-limits in
 * the StEStructBinning object
 *
 * Revision 1.9  2006/04/06 01:01:22  prindle
 *
 *   New mode in CutBin, 5, to do pid correlations. There is still an issue
 * of how to set the pt ranges allowed for the different particle types.
 * For data we probably want to restrict p to below 1GeV for pi and K, but
 * for Hijing and Pythia we can have perfect pid. Currently cuts are type
 * into the code (so you have to re-compile to change them.)
 *
 *   In the Correlations code I split -+ from +- and am keeping track of
 * pt for each cut bin. These required changes in the Support code.
 *
 * Revision 1.8  2006/04/04 22:10:13  porter
 * a handful of changes (specific to correlations)
 *  - added StEStructQAHists so that if NOT input frm Maker, each analysis has its own
 *  - used ability to get any max,min val from the cut class - or z-vertex binning
 *  - put z-vertex binning into 1 place
 *  - switched back 1st line of pair cut method to keep pair if good, not to reject if bad.
 *  - Pair cut object is now pointer in correlations
 *  - some diagnostic printouts available from macro
 *  - Duncan's delta-phi binning change
 *
 * Revision 1.7  2006/02/22 22:05:19  prindle
 * Removed all references to multRef (?)
 * Added cut mode 5 for particle identified correlations.
 * Other cut modes should be same as before
 *
 * Revision 1.6  2005/09/14 17:14:25  msd
 * Large update, added new pair-cut system, added pair density plots for new analysis mode (4), added event mixing cuts (rewrote buffer for this)
 *
 * Revision 1.5  2005/03/03 01:30:44  porter
 * updated StEStruct2ptCorrelations to include pt-correlations and removed
 * old version of pt-correlations from chunhuih (StEStruct2ptPtNbar)
 *
 * Revision 1.4  2004/06/25 03:11:50  porter
 * New cut-binning implementation and modified pair-cuts for chunhui to review
 *
 * Revision 1.3  2004/06/16 20:00:43  chunhuih
 *
 * changed one more post-increment operator to pre-increment operator.
 *
 * Revision 1.2  2004/03/19 19:07:43  chunhuih
 *
 * Use pre-increment instead of post-increment operators for the return values
 * of a set of cut methods. This returns the correct cut value for the first
 * time the counting array is incremented.
 *
 * Revision 1.1  2003/10/15 18:20:46  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/







