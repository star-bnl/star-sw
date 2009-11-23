/***************************************************************************
 *
 * $Id: StRpsRomanPot.h,v 2.1 2009/11/23 22:18:25 ullrich Exp $
 *
 * Author: Thomas Ullrich, Nov 2009
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRpsRomanPot.h,v $
 * Revision 2.1  2009/11/23 22:18:25  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StRpsRomanPot_hh
#define StRpsRomanPot_hh

#include "StObject.h"
#include "StContainers.h"
#include "StRpsPlane.h"

class StRpsCollection;

class StRpsRomanPot : public StObject {
public:
    StRpsRomanPot();
    ~StRpsRomanPot();

    unsigned int romanPotId() const;
    unsigned int numberOfPlanes() const;
    unsigned int numberOfPlanesWithClusters() const;
    unsigned int adc(unsigned int) const;
    unsigned int tac(unsigned int) const;
    unsigned char status() const;
    
    const StRpsPlane* plane(unsigned int) const;
    StRpsPlane* plane(unsigned int);
    
    void setStatus(unsigned char);
    void setAdc(unsigned int, unsigned int);
    void setTac(unsigned int, unsigned int);
    
protected:
    void setRomanPotId(unsigned char);
    friend class StRpsCollection;
    
protected:
    enum {mNumberOfPlanes = 4};
    StRpsPlane mPlanes[mNumberOfPlanes];
    UChar_t  mRomanPotId; // 0-7
    UChar_t  mStatus;
    UInt_t   mAdc[2];
    UInt_t   mTac[2];

    ClassDef(StRpsRomanPot,1)
};

#endif
