/*****************************************************************
 *
 * $Id: StTrsAnalogSignal.hh,v 1.4 2000/01/10 23:11:30 lasiuk Exp $
 *
 * Author: brian Nov 1, 1998
 *
 *****************************************************************
 * Description:  
 *
 *****************************************************************
 *
 * $Log: StTrsAnalogSignal.hh,v $
 * Revision 1.4  2000/01/10 23:11:30  lasiuk
 * Include MACROS for compatibility with SUN CC5.0
 *
 * Revision 1.3  1999/01/15 11:03:14  lasiuk
 * modify << operator for STL use
 *
 * Revision 1.2  1998/11/13 21:29:46  lasiuk
 * << operator
 *
 * Revision 1.1  1998/11/10 17:12:08  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.1  1998/11/01 13:46:43  lasiuk
 * Initial Revision
 *
 ******************************************************************/
#ifndef ST_TRS_ANALOGSIGNAL_HH
#define ST_TRS_ANALOGSIGNAL_HH

#include <iostream.h>
#include <utility>

#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
using std::pair;
#endif

class StTrsAnalogSignal {
public:
    StTrsAnalogSignal();
    StTrsAnalogSignal(float, float);
    ~StTrsAnalogSignal();
    //StTrsAnalogSignal(const StTrsAnalogSignal&);            // use default
    //StTrsAnalogSignal& operator=(const StTrsAnalogSignal&); // use default
    
    // access functions
    float time()         const;
    float amplitude()    const;
    
    void   setTime(float);
    void   setAmplitude(float);
    void   scaleAmplitude(float);
    
protected:
    pair<float, float>    mAnalogSignal;
};

inline float StTrsAnalogSignal::time() const {return mAnalogSignal.first;}
inline float StTrsAnalogSignal::amplitude() const {return mAnalogSignal.second;}
inline void StTrsAnalogSignal::setTime(float t) { mAnalogSignal.first = t;}
inline void StTrsAnalogSignal::setAmplitude(float a) { mAnalogSignal.second = a;}
inline void StTrsAnalogSignal::scaleAmplitude(float fac) {mAnalogSignal.second *= fac;}

// Non-member function
ostream& operator<<(ostream&, const StTrsAnalogSignal&);

class StTrsAnalogSignalComparator {
public:
    bool operator()(StTrsAnalogSignal x, StTrsAnalogSignal y)
	{
	    return (x.time() < y.time());
	}
};

#endif
