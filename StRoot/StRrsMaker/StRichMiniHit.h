/***************************************************************
 * $Id: StRichMiniHit.h,v 1.1 2000/03/17 14:54:50 lasiuk Exp $
 *
 * Description:
 *   StRichMiniHit is a data type containing information
 *   processed in RRS.
 *   
 *   StRichMiniHit is a structure/class with:
 *     - a 3-vector position (doubles)
 *     - a 3-vector momentum (doubles)
 *     - a track reference
 *     - a hit id
 *     - a mass
 *     - how it was generated (where it came from)
 *       
 ***************************************************************
 * $Log: StRichMiniHit.h,v $
 * Revision 1.1  2000/03/17 14:54:50  lasiuk
 * Large scale revisions after ROOT dependent memory leak
 *
 * Revision 1.1  2000/03/17 14:54:50  lasiuk
 * Large scale revisions after ROOT dependent memory leak
 *
 ***************************************************************/
#ifndef ST_RICH_MINIHIT_H
#define ST_RICH_MINIHIT_H

#include <iostream.h>

#include "StThreeVector.hh"

#include "StRichEnumeratedTypes.h"

class StRichMiniHit {
public:
    StRichMiniHit();
    StRichMiniHit(StThreeVector<double> x,
		  StThreeVector<double> p,
		  int trackP, short pID, double mass, StRichSignalType type);
    ~StRichMiniHit();

    //StRichMiniHit(const StRichMiniHit&); { /* use default */}
    //StRichMiniHit& operator=(const StRichMiniHit&) {/* use default */}

    // Access
    const StThreeVector<double>& position()   const;
    StThreeVector<double>& position();
    const StThreeVector<double>& momentum()  const;
    StThreeVector<double>& momentum();

    int              trackp()   const;
    short            id()       const;
    double           mass()     const;
    StRichSignalType process()  const;
    StRichSignalType type()     const;

private:
    StThreeVector<double>    mX;
    StThreeVector<double>    mP;
    int                      mTrackp;
    short                    mId;
    double                   mMass;
    StRichSignalType         mType;
};    
ostream& operator<<(ostream&, const StRichMiniHit&);

inline const StThreeVector<double>& StRichMiniHit::position() const {return mX;}
inline StThreeVector<double>& StRichMiniHit::position() {return mX;}
inline const StThreeVector<double>& StRichMiniHit::momentum()  const {return mP;}
inline StThreeVector<double>& StRichMiniHit::momentum() {return mP;}
inline int     StRichMiniHit::trackp() const {return mTrackp;}
inline short   StRichMiniHit::id()       const {return mId;}
inline double  StRichMiniHit::mass() const {return mMass;}
inline StRichSignalType StRichMiniHit::process() const {return mType;}
inline StRichSignalType StRichMiniHit::type() const {return mType;}

#endif // StRichMiniHit_H
