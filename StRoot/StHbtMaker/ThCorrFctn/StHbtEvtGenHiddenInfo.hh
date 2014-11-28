/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : Carrier of the information needed for the FSI calculation
 *   This hidden info carry enough information to do the calculation :
 *   Freeze out momenta and emission space-time point. In addition the pid
 *   is kept. I could be used to account for missidentification effects.
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#ifndef StHbtEvtGenHiddenInfo_hh
#define StHbtEvtGenHiddenInfo_hh

#include "StHbtMaker/Base/StHbtHiddenInfo.hh"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"

class StHbtEvtGenHiddenInfo : public StHbtHiddenInfo{

public:
// --- Constructors
  StHbtEvtGenHiddenInfo() {};
  StHbtEvtGenHiddenInfo(const StHbtLorentzVector& aFreezeOutMomEn, 
			const int& aStatus, const int& aPdgPid)
    :
    mEmPoint(0,0,0,0),
    mFreezeOutMomEn(aFreezeOutMomEn),
    mStatus(aStatus),mPdgPid(aPdgPid),mPosHaveNotBeenModified(1)
    {};
  StHbtEvtGenHiddenInfo(const StHbtLorentzVector& aEmPoint, 
			const StHbtLorentzVector& aFreezeOutMomEn, 
			const int& aStatus, const int& aPdgPid)
    :
    mEmPoint(aEmPoint),
    mFreezeOutMomEn(aFreezeOutMomEn),
     mStatus(aStatus),mPdgPid(aPdgPid),mPosHaveNotBeenModified(1)
    {};
  StHbtEvtGenHiddenInfo(const StHbtEvtGenHiddenInfo& aHiddenInfo)
    :
    mEmPoint(aHiddenInfo.mEmPoint),
    mFreezeOutMomEn(aHiddenInfo.mFreezeOutMomEn),
    mStatus(aHiddenInfo.mStatus),
    mPdgPid(aHiddenInfo.mPdgPid),mPosHaveNotBeenModified(1)
    {};
// --- Destructor
  virtual ~StHbtEvtGenHiddenInfo(){/* no-op */};

// --- Setting (avoid construction)
  void setEmPoint(const StHbtLorentzVector&);
  void setFreezeOutMomEn(const StHbtLorentzVector&); 
  void setStatus(int);
  void setPdgPid(int);
  void setPid(int);

// --- Return hidden info content
  StHbtLorentzVector* getEmPoint();
  StHbtLorentzVector* getFreezeOutMomEn();
  const StHbtLorentzVector* getEmPoint() const;
  const StHbtLorentzVector* getFreezeOutMomEn() const;
  int getStatus() const;
  int getPdgPid() const;
  int getPid() const;
  int posHaveNotBeenModified() const;
  void setPosHaveBeenModified();

// !!! MANDATORY !!!
// --- Copy the hidden info from StHbtTrack to StHbtParticle
  virtual StHbtHiddenInfo* getParticleHiddenInfo() const;

private:
  StHbtLorentzVector mEmPoint;
  StHbtLorentzVector mFreezeOutMomEn;
  int mStatus;
  int mPdgPid;
  int mPosHaveNotBeenModified;
};

inline StHbtLorentzVector* StHbtEvtGenHiddenInfo::getEmPoint()
{return  &mEmPoint;}
inline StHbtLorentzVector* StHbtEvtGenHiddenInfo::getFreezeOutMomEn() 
{return  &mFreezeOutMomEn;}
inline const StHbtLorentzVector* StHbtEvtGenHiddenInfo::getEmPoint()
const {return  &mEmPoint;}
inline const StHbtLorentzVector* StHbtEvtGenHiddenInfo::getFreezeOutMomEn() 
const {return  &mFreezeOutMomEn;}
inline int StHbtEvtGenHiddenInfo::getStatus() const {return  mStatus;}
inline int StHbtEvtGenHiddenInfo::getPdgPid() const {return  mPdgPid;}
inline int StHbtEvtGenHiddenInfo::getPid() const {return  mPdgPid;}
inline void StHbtEvtGenHiddenInfo::setEmPoint(const StHbtLorentzVector& aEmPoint){
mEmPoint = aEmPoint;
}
inline void StHbtEvtGenHiddenInfo::setFreezeOutMomEn(const StHbtLorentzVector& 
						     aFreezeOutMomEn){
  mFreezeOutMomEn=aFreezeOutMomEn;
}
inline void StHbtEvtGenHiddenInfo::setStatus(int aStatus){
  mStatus=aStatus;
}
inline void StHbtEvtGenHiddenInfo::setPdgPid(int aPdgPid){
  mPdgPid=aPdgPid;
}
inline void StHbtEvtGenHiddenInfo::setPid(int aPdgPid){
  mPdgPid=aPdgPid;
}
inline StHbtHiddenInfo* StHbtEvtGenHiddenInfo::getParticleHiddenInfo() const
{return new StHbtEvtGenHiddenInfo(mEmPoint,mFreezeOutMomEn,mStatus,mPdgPid);}
inline int StHbtEvtGenHiddenInfo::posHaveNotBeenModified() const{
  return mPosHaveNotBeenModified;
}
inline void StHbtEvtGenHiddenInfo::setPosHaveBeenModified(){
  mPosHaveNotBeenModified=0;
}
#endif
