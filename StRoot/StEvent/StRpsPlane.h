/***************************************************************************
 *
 * $Id: StRpsPlane.h,v 2.1 2009/11/23 22:18:25 ullrich Exp $
 *
 * Author: Thomas Ullrich, Nov 2009
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRpsPlane.h,v $
 * Revision 2.1  2009/11/23 22:18:25  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StRpsPlane_hh
#define StRpsPlane_hh

#include "StObject.h"
#include "StContainers.h"


class StRpsCluster;
class StRpsRomanPot;

class StRpsPlane : public StObject {
public:
    StRpsPlane();
    ~StRpsPlane();

    double offset() const;
    double z() const;
    double angle() const;
    short  orientation() const;
    unsigned char status() const;
    unsigned int  numberOfClusters() const;
    unsigned int  planeId() const;
    
    unsigned int  romanPotId() const;

    const StRpsCluster* cluster(unsigned int) const;
    StRpsCluster*       cluster(unsigned int);
        
    const StSPtrVecRpsCluster& clusters() const;
    StSPtrVecRpsCluster&       clusters();

    void addCluster(StRpsCluster*);
    void setOffset(double);
    void setZ(double);
    void setAngle(double);
    void setOrientation(short);
    void setStatus(unsigned char);
  
protected:    
    friend class StRpsCollection;
    void setPlaneId(unsigned char);
    void setRomanPotId(unsigned char);
    
protected:
    UChar_t  mPlaneId; // 0-3
    Double_t mOffset;
    Double_t mZ;
    Double_t mAngle;
    Short_t  mOrientation;
    UChar_t  mStatus;
    UChar_t  mRomanPotId; // 0-7
    StSPtrVecRpsCluster mClusters;

    ClassDef(StRpsPlane,1)
};

#endif
