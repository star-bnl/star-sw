/*****************************************************************
 *
 * $Id: StMixerAnalogSignal.hh,v 1.1 2000/02/16 21:02:07 pfachini Exp $
 *
 * Author: Patricia Fachini - a copy from StTrsAnalogSignal.hh
 *
 *****************************************************************
 * Description:  
 *
 *****************************************************************
 *
 ******************************************************************/
#ifndef ST_MIXER_ANALOGSIGNAL_HH
#define ST_MIXER_ANALOGSIGNAL_HH

#include <iostream.h>
#include <utility>

class StMixerAnalogSignal {
public:
    StMixerAnalogSignal();
    StMixerAnalogSignal(float, float);
    ~StMixerAnalogSignal();
    
    // access functions
    float time()         const;
    float amplitude()    const;
    
    void   setTime(float);
    void   setAmplitude(float);
    void   scaleAmplitude(float);
    
protected:
    pair<float, float>    mAnalogSignal;
};

inline float StMixerAnalogSignal::time() const {return mAnalogSignal.first;}
inline float StMixerAnalogSignal::amplitude() const {return mAnalogSignal.second;}
inline void StMixerAnalogSignal::setTime(float t) { mAnalogSignal.first = t;}
inline void StMixerAnalogSignal::setAmplitude(float a) { mAnalogSignal.second = a;}
inline void StMixerAnalogSignal::scaleAmplitude(float fac) {mAnalogSignal.second *= fac;}

// Non-member function
ostream& operator<<(ostream&, const StMixerAnalogSignal&);

class StMixerAnalogSignalComparator {
public:
    bool operator()(StMixerAnalogSignal x, StMixerAnalogSignal y)
	{
	    return (x.time() < y.time());
	}
};

#endif
