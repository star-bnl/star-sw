/***************************************************************************
 *
 *  
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
 *  
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

  virtual void Set(const StHbtPair*)=0;

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

  // Bertsch-Pratt momentum components in Pair Frame - written by Bekele/Humanic
  double RealqSideCMS() const;
  double RealqOutCMS() const;
  double RealqLongCMS() const;
  
  double RealqSidePf() const;
  double RealqOutPf() const;
  double RealqLongPf() const;

protected:
  const StHbtLorentzVector* mMomentum1;//!
  const StHbtLorentzVector* mMomentum2;//!

  const StHbtLorentzVector* mEmPoint1;//!
  const StHbtLorentzVector* mEmPoint2;//!
  
  int mPid1;//!
  int mPid2;//!

  const StHbtPair* mMeasPair;//!

  StHbtFsiWeight* mWeight;//!

  double mWeightNum;
  double mWeightDen;

  bool mWeightOk;

  void UpdateWeight();

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


#endif
