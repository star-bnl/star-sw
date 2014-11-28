/*****************************************************************
 *
 * $Id: StTrsAnalogSignal.hh,v 1.7 2005/09/09 22:12:48 perev Exp $
 *
 * Author: brian Nov 1, 1998
 *
 *****************************************************************
 * Description:  
 *
 *****************************************************************
 *
 * $Log: StTrsAnalogSignal.hh,v $
 * Revision 1.7  2005/09/09 22:12:48  perev
 * Bug fix + IdTruth added
 *
 * Revision 1.6  2003/12/24 13:44:51  fisyak
 * Add (GEANT) track Id information in Trs; propagate it via St_tpcdaq_Maker; account interface change in StTrsZeroSuppressedReaded in StMixerMaker
 *
 * Revision 1.5  2003/09/02 17:59:16  perev
 * gcc 3.2 updates + WarnOff
 *
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
#include <assert.h>

#include <Stiostream.h>
#include <utility>

#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
using std::pair;
#endif

class StTrsAnalogSignal {
public:
    StTrsAnalogSignal();
    StTrsAnalogSignal(float, float, int id=0);
    ~StTrsAnalogSignal();
    //StTrsAnalogSignal(const StTrsAnalogSignal&);            // use default
    //StTrsAnalogSignal& operator=(const StTrsAnalogSignal&); // use default
    StTrsAnalogSignal& operator+=(const StTrsAnalogSignal&); 
    
    // access functions
    float time()         const;
    float amplitude()    const;
    int   id()           const   {return mId;}

    void   setTime(float);
    void   setAmplitude(float);
    void   scaleAmplitude(float);
    void   setId(int id)         {mId = id;}
    
protected:
    int                   mId; // geant track no.
    float                 mTime;
    float                 mAmp;
};

inline float StTrsAnalogSignal::time() const 		 {return mTime;}
inline float StTrsAnalogSignal::amplitude() const 	 {return mAmp; }
inline void  StTrsAnalogSignal::setTime(float t) 	 { mTime = t;  }
inline void  StTrsAnalogSignal::setAmplitude(float a) 	 { mAmp  = a;  }
inline void  StTrsAnalogSignal::scaleAmplitude(float fac){ mAmp *= fac;}
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
