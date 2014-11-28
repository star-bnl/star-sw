/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : Buffer between the StHbtPair and the correlation function
 * this base Class is responsible for accessing data concerning afterburner
 * correlator i.e :
 * user must plug in one StHbtFsiWeight
 * then the StHbtThPair is initialised with Set(StHbtPair)
 * then user have access to RealMomentum, Measured Momentum, Emission Point,
 * Pid Weight of Numerator and Denominator.
 * for each instantiation of that class, programmer choose which data 
 * he want to be returned
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#ifndef ST_HBT_TH_PAIR_HH
#define ST_HBT_TH_PAIR_HH


#include "StHbtMaker/Infrastructure/StHbtPair.hh"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"

class StHbtFsiWeight;

class StHbtThPair {
public:

  StHbtThPair();
  virtual ~StHbtThPair(){/* no-op */};

  virtual void Set(const StHbtPair*);
  virtual void SetWeight(StHbtFsiWeight*);

  virtual const StHbtLorentzVector* GetRealMomentum1() const;
  virtual const StHbtLorentzVector* GetRealMomentum2() const;
  virtual const StHbtLorentzVector* GetMeasMomentum1() const;
  virtual const StHbtLorentzVector* GetMeasMomentum2() const;
  virtual const StHbtLorentzVector* GetEmPoint1() const;
  virtual const StHbtLorentzVector* GetEmPoint2() const;
  virtual int GetPid1() const;
  virtual int GetPid2() const;
  virtual const StHbtPair* GetMeasPair() const;
  virtual double GetWeightNum() ;
  virtual double GetWeightDen() ;
  virtual StHbtString Report();  
  virtual double RealQInv() const;
  double RealqSideCMS() const;
  double RealqOutCMS() const;
  double RealqLongCMS() const;
  double RealqSidePf() const;
  double RealqOutPf() const;
  double RealqLongPf() const;

  // Fabrice private
  virtual void set(const StHbtPair*);
  void setMomRes1(int pid);
  void setMomRes2(int pid);
  void setPairPurity(double aPairPurity);
  virtual StHbtLorentzVector const* getMomentum1() const;
  virtual StHbtLorentzVector const* getMomentum2() const;
  virtual StHbtLorentzVector const* getEmPoint1() const;
  virtual StHbtLorentzVector const* getEmPoint2() const;
  virtual int getPid1() const;
  virtual int getPid2() const;
  virtual double QInv() const;

  double KStarSide() const;
  double KStarOut() const;
  double KStarLong() const;
  double KStar() const;
  double CVK() const;
  double KStarFlipped() const;
  double CVKFlipped() const;  
  double RStar() const;
  double RTrans() const;
  double ROut() const;
  double RLong() const;
  double RSide() const;
  double DTime() const;
  double DTimePairLCMS() const;
  double ROutPairCMS() const;
  double RLongPairCMS() const;
  double RSidePairCMS() const;
  double DTimePairCMS() const;

  double Betat() const;
  double Ut() const;
  double Pt() const;
  
protected:
  virtual void setVariables(const StHbtPair*);

  const StHbtLorentzVector* mMomentum1;//!
  const StHbtLorentzVector* mMomentum2;//!
  StHbtLorentzVector* mEmPoint1;//!
  StHbtLorentzVector* mEmPoint2;//!  
  int mPid1;//!
  int mPid2;//!

  const StHbtPair* mMeasPair;//!

  StHbtFsiWeight* mWeight;//!
  double mWeightNum;
  double mWeightDen;
  bool mWeightOk;
  void UpdateWeight();
  
  mutable short mMomParCalculated;
  void calcMomParameters() const;
  mutable double mKStarSide;
  mutable double mKStarOut;
  mutable double mKStarLong;
  mutable double mKStar;
  mutable double mCVK;
  mutable double mKStarFlipped;
  mutable double mCVKFlipped;  
  mutable double mBetat;
  mutable double mUt;
  mutable double mPt;

  mutable short mPosParCalculated;
  void calcPosParameters() const;
  mutable double mRStar;
  mutable double mRTrans;
  mutable double mROut;
  mutable double mRSide;
  mutable double mRLong;
  mutable double mDTime;
  mutable double mDTimePairLCMS;
  mutable double mROutPairCMS;
  mutable double mRSidePairCMS;
  mutable double mRLongPairCMS; // same as LCMS?
  mutable double mDTimePairCMS;

  short mMomRes1;
  short mMomRes2;
  // param : Y = [0] + [1] * X^[2]
  double mPLoss1[3]; // Momentum loss parametrization
  double mPtRes1[3]; // pt resolution
  double mPhiRes1[3]; // Phi res
  double mThetaRes1[3]; // Theta res
  double mPLoss2[3]; // Momentum loss parametrization
  double mPtRes2[3]; // pt resolution
  double mPhiRes2[3]; //
  double mThetaRes2[3]; //
  //void applyMomRes1();
  //void applyMomRes2();

  double mPairPurity;

#ifdef __ROOT__
  ClassDef(StHbtThPair,1)
#endif
};


inline const StHbtLorentzVector* StHbtThPair::GetRealMomentum1() const
{return mMomentum1;};
inline const StHbtLorentzVector* StHbtThPair::GetRealMomentum2() const
{return mMomentum2;};
inline const StHbtLorentzVector* StHbtThPair::GetMeasMomentum1() const
{return (StHbtLorentzVector*)(&(mMeasPair->track1()->FourMomentum()));};
inline const StHbtLorentzVector* StHbtThPair::GetMeasMomentum2() const
{return (StHbtLorentzVector*)(&(mMeasPair->track2()->FourMomentum()));};
inline const StHbtLorentzVector* StHbtThPair::GetEmPoint1() const
{return mEmPoint1;};
inline const StHbtLorentzVector* StHbtThPair::GetEmPoint2() const
{return mEmPoint2;};  
inline int StHbtThPair::GetPid1() const {return mPid1;};
inline int StHbtThPair::GetPid2() const {return mPid2;};
inline const StHbtPair* StHbtThPair::GetMeasPair() const
{return mMeasPair;}; 
inline double StHbtThPair::GetWeightNum()  {
  if (!(mWeightOk)) UpdateWeight();
  return mWeightNum;} 
inline double StHbtThPair::GetWeightDen()  {
  if (!(mWeightOk)) UpdateWeight();
  return mWeightDen;}; 

inline void StHbtThPair::SetWeight(StHbtFsiWeight* aWeight) 
{mWeight=aWeight;};   
inline double StHbtThPair::RealQInv() const{
  return abs(*mMomentum1-*mMomentum2);
}

inline void StHbtThPair::setPairPurity(double aPairPurity){
  mPairPurity=aPairPurity;
}
inline void StHbtThPair::Set(const StHbtPair* aPair){
  mMomParCalculated=0;
  mPosParCalculated=0;
  mWeightOk=false;
  mMeasPair=aPair;
  setVariables(aPair);

  //if(mMomRes1) applyMomRes1();
  //if(mMomRes2) applyMomRes2();
}
// Fabrice private
inline void StHbtThPair::set(const StHbtPair* aPair){
  static StHbtParticle* tPrevPart1;
  static StHbtParticle* tPrevPart2;
  if(tPrevPart1 != aPair->track1() || tPrevPart2 != aPair->track2()){
    mWeightOk=false;
    mMeasPair=aPair;
    setVariables(aPair);
    calcMomParameters();
    calcPosParameters();
  }
  tPrevPart1 = aPair->track1();
  tPrevPart2 = aPair->track2();
  //if(mMomRes1) applyMomRes1();
  //if(mMomRes2) applyMomRes2();
}
inline StHbtLorentzVector const* StHbtThPair::getMomentum1() const 
{return mMomentum1;};
inline StHbtLorentzVector const* StHbtThPair::getMomentum2() const 
{return mMomentum2;};
inline StHbtLorentzVector const* StHbtThPair::getEmPoint1() const 
{return mEmPoint1;};
inline StHbtLorentzVector const* StHbtThPair::getEmPoint2() const 
{return mEmPoint2;};  
inline int StHbtThPair::getPid1() const {return mPid1;};
inline int StHbtThPair::getPid2() const {return mPid2;};

inline double StHbtThPair::QInv() const{
  return abs((*mMomentum1)-(*mMomentum2));
}

inline double StHbtThPair::KStarSide() const{
  if(!mMomParCalculated) calcMomParameters();
  return mKStarSide;
}
inline double StHbtThPair::KStarOut() const{
  if(!mMomParCalculated) calcMomParameters();
  return mKStarOut;
}
inline double StHbtThPair::KStarLong() const{
  if(!mMomParCalculated) calcMomParameters();
  return mKStarLong;
}
inline double StHbtThPair::KStar() const{
  if(!mMomParCalculated) calcMomParameters();
  return mKStar;
}
inline double StHbtThPair::CVK() const{
  if(!mMomParCalculated) calcMomParameters();
  return mCVK;
}
inline double StHbtThPair::KStarFlipped() const{
  if(!mMomParCalculated) calcMomParameters();
  return mKStarFlipped;
}
inline double StHbtThPair::CVKFlipped() const{
  if(!mMomParCalculated) calcMomParameters();
  return mCVKFlipped;
}
inline double StHbtThPair::Betat() const{
  if(!mMomParCalculated) calcMomParameters();
  return mBetat;
}
inline double StHbtThPair::Ut() const{
  if(!mMomParCalculated) calcMomParameters();
  return mUt;
}
inline double StHbtThPair::Pt() const{
  if(!mMomParCalculated) calcMomParameters();
  return mPt;
}

inline double StHbtThPair::RStar() const{
  if(!mPosParCalculated){ calcPosParameters(); }
  return mRStar;
}
inline double StHbtThPair::RTrans() const{
  if(!mPosParCalculated){ calcPosParameters(); }
  return mRTrans;
}
inline double StHbtThPair::ROut() const{
  if(!mPosParCalculated){ calcPosParameters(); }
  return mROut;
}
inline double StHbtThPair::RLong() const{
  if(!mPosParCalculated){ calcPosParameters(); }
  return mRLong;
}
inline double StHbtThPair::RSide() const{
  if(!mPosParCalculated){ calcPosParameters(); }
  return mRSide;
}
inline double StHbtThPair::DTime() const{
  if(!mPosParCalculated){ calcPosParameters(); }
  return mDTime;
}
inline double StHbtThPair::DTimePairLCMS() const{
  if(!mPosParCalculated){ calcPosParameters(); }
  return mDTimePairLCMS;
}
inline double StHbtThPair::ROutPairCMS() const{
  if(!mPosParCalculated){ calcPosParameters(); }
  return mROutPairCMS;
}
inline double StHbtThPair::RLongPairCMS() const{
  if(!mPosParCalculated){ calcPosParameters(); }
  return mRLongPairCMS;
}
inline double StHbtThPair::RSidePairCMS() const{
  if(!mPosParCalculated){ calcPosParameters(); }
  return mRSidePairCMS;
}
inline double StHbtThPair::DTimePairCMS() const{
  if(!mPosParCalculated){ calcPosParameters(); }
  return mDTimePairCMS;
}

#endif
