/***************************************************************************
 *
 *  
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : Carrier of the information needed for the FSI calculation
 *
 ***************************************************************************
 *
 *  
 *
 ***************************************************************************/

#ifndef StHbtEvtGenHiddenInfo_hh
#define StHbtEvtGenHiddenInfo_hh

#include "StHbtMaker/Base/StHbtHiddenInfo.hh"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"

class StHbtEvtGenHiddenInfo : public StHbtHiddenInfo{

public:
// --- Constructors
  StHbtEvtGenHiddenInfo() {/* no-op */};
  StHbtEvtGenHiddenInfo(const StHbtLorentzVector& aEmPoint, 
		     const StHbtLorentzVector& aFreezeOutMomEn, 
		     const int& aPid)
    :
    mEmPoint(aEmPoint),
    mFreezeOutMomEn(aFreezeOutMomEn),
    mPid(aPid)
    {};
  StHbtEvtGenHiddenInfo(const StHbtEvtGenHiddenInfo& aHiddenInfo)
    :
    mEmPoint(aHiddenInfo.getEmPoint()),
    mFreezeOutMomEn(aHiddenInfo.getFreezeOutMomEn()),
    mPid(aHiddenInfo.getPid()) 
    {};
// --- Destructor
  virtual ~StHbtEvtGenHiddenInfo(){/* no-op */};


// --- Return hidden info content
  virtual const StHbtLorentzVector& getEmPoint() const;
  const StHbtLorentzVector& getFreezeOutMomEn() const;
  int getPid() const;

// ---Change hidden info content
  void setEmPoint( const StHbtLorentzVector*);
  void setFreezeOutMomEn(const StHbtLorentzVector*);
  void setPid(int) ;




// !!! MANDATORY !!!
// --- Copy the hidden info from StHbtTrack to StHbtParticle
  virtual StHbtHiddenInfo* getParticleHiddenInfo() const;

private:
  StHbtLorentzVector mEmPoint;
  StHbtLorentzVector mFreezeOutMomEn;
  int mPid;
};

inline const StHbtLorentzVector& StHbtEvtGenHiddenInfo::getEmPoint() 
const {return  mEmPoint;}
inline const StHbtLorentzVector& StHbtEvtGenHiddenInfo::getFreezeOutMomEn()  
const {return  mFreezeOutMomEn;}
inline int StHbtEvtGenHiddenInfo::getPid() const
 {return  mPid;}
inline  void  StHbtEvtGenHiddenInfo::setEmPoint( const StHbtLorentzVector* aX) 
{ mEmPoint=*aX;}
inline  void  StHbtEvtGenHiddenInfo::setFreezeOutMomEn(const StHbtLorentzVector* aP)
{ mFreezeOutMomEn=*aP;}
inline  void StHbtEvtGenHiddenInfo:: setPid(int aPid)
{mPid=aPid;}
inline StHbtHiddenInfo* StHbtEvtGenHiddenInfo::getParticleHiddenInfo()
 const
{return new StHbtEvtGenHiddenInfo(mEmPoint,mFreezeOutMomEn,mPid);}

#endif
