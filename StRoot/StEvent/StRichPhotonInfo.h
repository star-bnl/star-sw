/***************************************************************************
 *
 * $Id: StRichPhotonInfo.h,v 2.1 2000/11/25 11:51:01 lasiuk Exp $
 *
 * Author: Brian Lasiuk, Nov 2000
 ***************************************************************************
 *
 * Description: Definition of the persistent Photon Info object 
 *
 ***************************************************************************
 *
 * $Log: StRichPhotonInfo.h,v $
 * Revision 2.1  2000/11/25 11:51:01  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#ifndef StRichPhotonInfo_hh
#define StRichPhotonInfo_hh

#include <iostream.h>

#include "StObject.h"

#include "StEnumerations.h"

class StRichPhotonInfo : public StObject {
public:
    StRichPhotonInfo();
    StRichPhotonInfo(double, double, double);
    
    virtual ~StRichPhotonInfo();
    //StRichPhotonInfo(const StRichPhotonInfo&){ /* nopt */ }
    //StRichPhotonInfo& operator=(const StRichPhotonInfo&){/* nopt */}

    void    setD(Float_t);
    void    setSigma(Float_t);
    void    setAzimuth(Float_t);
    
    Float_t d()       const; 
    Float_t sigma()   const;
    Float_t azimuth() const;
    
protected:
    StObject* clone();

    Float_t    mD;
    Float_t    mSigma;
    Float_t    mAzimuth;
    
    ClassDef(StRichPhotonInfo,1) 
};

inline void StRichPhotonInfo::setD(Float_t d) {mD = d;}
inline void StRichPhotonInfo::setSigma(Float_t s) {mSigma = s;}
inline void StRichPhotonInfo::setAzimuth(Float_t psi) {mAzimuth = psi;}
    
inline Float_t StRichPhotonInfo::d() const {return mD;} 
inline Float_t StRichPhotonInfo::sigma() const {return mSigma;} 
inline Float_t StRichPhotonInfo::azimuth() const {return mAzimuth;} 

//non-members
ostream& operator<<(ostream& os, const StRichPhotonInfo& hit);

#endif
