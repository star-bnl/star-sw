/***************************************************************************
 *
 * $Id: StRichCerenkovPhoton.h,v 1.2 2003/09/02 17:58:55 perev Exp $
 *
 * Author:  bl Mar 27, 2001
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              for use in Cerenkov Ray Tracing Algorithm
 *              to be added in the StRichCerenkovHistogram
 ***************************************************************************
 *
 * $Log: StRichCerenkovPhoton.h,v $
 * Revision 1.2  2003/09/02 17:58:55  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  2001/08/21 17:58:33  lasiuk
 * for 2000 analysis
 *
 **************************************************************************/
#ifndef StRichCerenkovPhoton_h
#define StRichCerenkovPhoton_h

#include <Stiostream.h>
#include "StRichHit.h"

class StRichCerenkovPhoton {
    
public:
    StRichCerenkovPhoton();
    StRichCerenkovPhoton(double theta, double phi, StRichHit* hitPointer = 0);

    ~StRichCerenkovPhoton();
    
    double     theta() const;
    double     phi()   const;
    StRichHit* hit()  const;

    void setTheta(double);
    void setPhi(double);
    void setHit(StRichHit*);	
    
private:
    double     mTheta;
    double     mPhi;
    StRichHit* mHit;
};

inline double StRichCerenkovPhoton::theta() const { return mTheta;}
inline double StRichCerenkovPhoton::phi() const { return mPhi;}
inline StRichHit* StRichCerenkovPhoton::hit() const { return mHit;}

inline void StRichCerenkovPhoton::setTheta(double theta) { mTheta = theta;}
inline void StRichCerenkovPhoton::setPhi(double phi) { mPhi = phi;}
inline void StRichCerenkovPhoton::setHit(StRichHit* hit) { mHit = hit;}

// Non-member functions
ostream& operator<<(ostream& os, const StRichCerenkovPhoton& photon);
#endif
