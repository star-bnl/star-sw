/***************************************************************************
 *
 * $Id: StRpsCluster.h,v 2.2 2015/10/02 19:50:09 ullrich Exp $
 *
 * Author: Thomas Ullrich, Nov 2009
 ***************************************************************************
 *
 * Description:  Reconstructed cluster in the Roman Pot Silicon 
 *               detectors.         
 *
 ***************************************************************************
 *
 * $Log: StRpsCluster.h,v $
 * Revision 2.2  2015/10/02 19:50:09  ullrich
 * Added mPositionRMS and accessors.
 *
 * Revision 2.1  2009/11/23 22:18:25  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StRpsCluster_hh
#define StRpsCluster_hh

#include <Stiostream.h>
#include "StObject.h"

class StRpsPlane;

class StRpsCluster : public StObject {
public:
    StRpsCluster();
    StRpsCluster(double pos, double posRMS, short len,
                 double e, double xy, unsigned char qual);
    ~StRpsCluster();

    double position() const;
    double positionRMS() const;
    short  length() const;
    double energy() const;
    double xy() const;
    unsigned char quality() const;

    unsigned int romanPotId() const;
    unsigned int planeId() const;
    
    void setPosition(double);
    void setPositionRMS(double);
    void setLength(short);
    void setEnergy(double);
    void setXY(double);
    void setQuality(unsigned char);

protected:
    friend class StRpsPlane;
    void setPlaneId(unsigned char);
    void setRomanPotId(unsigned char);

 protected:
    Double_t       mPosition;
    Double_t       mPositionRMS;
    Short_t        mLength;    
    Double_t       mEnergy;
    Double_t       mXY;
    UChar_t        mQuality;
    UChar_t        mPlaneId;
    UChar_t        mRomanPotId;

    ClassDef(StRpsCluster,2)
};

ostream& operator<<(ostream&, const StRpsCluster&);

#endif
