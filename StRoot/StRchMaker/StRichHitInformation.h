/***************************************************************************
 *
 * $Id: StRichHitInformation.h,v 2.2 2000/09/29 19:05:25 lasiuk Exp $
 *
 * Author: bl
 ***************************************************************************
 *
 * Description: Definition of the Hit information
 *
 ***************************************************************************
 *
 * $Log: StRichHitInformation.h,v $
 * Revision 2.2  2000/09/29 19:05:25  lasiuk
 * number of pads added as well as ostream operator
 *
 * Revision 2.1  2000/09/13 21:01:58  lasiuk
 * Initial Revision
 *
 **************************************************************************/

#ifndef ST_RICH_HIT_INFORMATION_H
#define ST_RICH_HIT_INFORMATION_H

#include <iostream.h>

#include "StThreeVector.hh"

class StRichHitInformation {
public:
    StRichHitInformation();
    StRichHitInformation(StThreeVector<double> x,
			 StThreeVector<double> dx,
			 StThreeVector<double> x2,
			 float q, float maxAdc, int cno);
    ~StRichHitInformation();

    //StRichHitInformation(const StRichHitInformation&){/* use default*/}
    //StRichHitInformation operator=(const StRichHitInformation&){/* use default*/}

    void setPosition(StThreeVector<double>&);
    void setPositionError(StThreeVector<double>&);
    void setPosition2(StThreeVector<double>&);
    void setPosition(double,double,double);
    void setPositionError(double,double,double);
    void setPosition2(double,double,double);
    void setCharge(float);
    void setMaxAdc(float);
    void setClusterNumber(int);
    void setNumberOfPads(unsigned short);
    
    const StThreeVector<double>& position() const;
    StThreeVector<double>& position();
    const StThreeVector<double>& positionError() const;
    StThreeVector<double>& positionError();
    const StThreeVector<double>& position2() const;
    StThreeVector<double>& position2();
    float charge() const;
    float maxAdc() const;
    int clusterNumber() const;
    unsigned short numberOfPads() const;
    
private:
    StThreeVector<double> mPosition;
    StThreeVector<double> mPositionError; // sigma in pads
    StThreeVector<double> mPosition2;
    float                 mCharge;
    float                 mMaxAdc;
    int                   mClusterNumber;
    unsigned short        mNumberOfPads;
};
inline void StRichHitInformation::setPosition(StThreeVector<double>& x) { mPosition = x; }
inline void StRichHitInformation::setPositionError(StThreeVector<double>& dx) { mPositionError = dx; }
inline void StRichHitInformation::setPosition2(StThreeVector<double>& x2) { mPosition2 = x2; }
inline void StRichHitInformation::setCharge(float q) { mCharge = q; }
inline void StRichHitInformation::setMaxAdc(float q) { mMaxAdc = q; }
inline void StRichHitInformation::setClusterNumber(int n) { mClusterNumber = n; }
inline void StRichHitInformation::setNumberOfPads(unsigned short n) {mNumberOfPads = n;}

inline const StThreeVector<double>& StRichHitInformation::position() const { return mPosition; }
inline StThreeVector<double>& StRichHitInformation::position() { return mPosition; }
inline const StThreeVector<double>& StRichHitInformation::positionError() const { return mPositionError; }
inline StThreeVector<double>& StRichHitInformation::positionError() { return mPositionError; }
inline const StThreeVector<double>& StRichHitInformation::position2() const { return mPosition2; }
inline StThreeVector<double>& StRichHitInformation::position2() { return mPosition2; }
inline float StRichHitInformation::charge() const { return mCharge; }
inline float StRichHitInformation::maxAdc() const { return mMaxAdc; }
inline int   StRichHitInformation::clusterNumber() const { return mClusterNumber; }
inline unsigned short StRichHitInformation::numberOfPads() const { return mNumberOfPads;}

ostream& operator<<(ostream&, StRichHitInformation&);
#endif
