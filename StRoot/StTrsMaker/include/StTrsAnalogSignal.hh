/*****************************************************************
 *
 * $Id: StTrsAnalogSignal.hh,v 1.1 1998/11/10 17:12:08 fisyak Exp $
 *
 * Author: brian Nov 1, 1998
 *
 *****************************************************************
 * Description:  
 *
 *****************************************************************
 *
 * $Log: StTrsAnalogSignal.hh,v $
 * Revision 1.1  1998/11/10 17:12:08  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.1  1998/11/10 17:12:08  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.1  1998/11/01 13:46:43  lasiuk
 * Initial Revision
 *
 ******************************************************************/
#define ST_TRS_ANALOGSIGNAL_HH

#include <iostream.h>
#include <utility>

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

// Non-member function
ostream& operator<<(ostream&, StTrsAnalogSignal&);

class StTrsAnalogSignalComparator {
public:
    bool operator()(StTrsAnalogSignal x, StTrsAnalogSignal y)
	{
	    return (x.time() < y.time());
	}
};

#endif
