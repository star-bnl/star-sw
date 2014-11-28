/*!
 * \class StRichCluster 
 * \author bl
 */
/***************************************************************************
 *
 * $Id: StRichCluster.h,v 2.5 2002/02/22 22:56:49 jeromel Exp $
 *
 * Author: bl
 ***************************************************************************
 *
 * Description: Definition of the Cluster  object (Persistent)
 *
 ***************************************************************************
 *
 * $Log: StRichCluster.h,v $
 * Revision 2.5  2002/02/22 22:56:49  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.4  2001/04/05 04:00:39  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.3  2000/08/09 14:11:51  perev
 * ClassDef must be without ;, (???)
 *
 * Revision 2.2  2000/08/08 14:42:14  ullrich
 * Added missing ClassDef and ClassImp macros.
 *
 * Revision 2.1  2000/05/22 21:44:56  ullrich
 * Initial Revision
 *
 ***************************************************************************/
#ifndef StRichCluster_hh
#define StRichCluster_hh
#include "StObject.h"

class StRichCluster : public StObject {
public:
    StRichCluster();
    ~StRichCluster();
    StRichCluster(int nPads, int nLocMax, int fPad, float ampSum, float amp2Sum, float rms2);
    //StRichCluster(const StRichCluster&) {}
    //StRichCluster& operator=(const StRichCluster&) {}
    
    int    operator==(const StRichCluster&) const;
    
    int    numberOfPads()        const;
    int    numberOfLocalMax()    const;
    int    firstPad()            const;
    float  minimumAmplitudeOfLocalMax() const;
    float  amplitudeSum()        const;
    float  amplitude2Sum()       const;
    float  rms2()                const;
    float  rms2Calc();
    
    void   increaseNumberOfLocalMax();
    void   increaseNumberOfPads();
    void   setFirstPad(int index);
    void   setNumberOfPads(int newNPads);
    void   updateAmplitude(float newamp);
    void   setMinimumAmplitudeOfLocalMax(float newLocMax);
    
private:
    Int_t    mNumberOfPads;     // number of associated pads
    Int_t    mNumberOfLocalMax; // number of local maxima
    Int_t    mFirstPad;         // index of first cluster pad
    Float_t  mMinimumAmplitudeOfLocalMax;   // lowest amplitude of all associated local maxima
    Float_t  mAmplitudeSum;     // sum of all pad amplitudes
    Float_t  mAmplitude2Sum;    // sum of squares of all pad amplitudes
    Float_t  mRms2;             // square rms of pad amplitudes
    ClassDef(StRichCluster,1)
};

inline int StRichCluster::numberOfPads() const { return mNumberOfPads; }
inline int StRichCluster::numberOfLocalMax() const { return mNumberOfLocalMax; }
inline int StRichCluster::firstPad() const { return mFirstPad; }
inline float StRichCluster::minimumAmplitudeOfLocalMax() const { return mMinimumAmplitudeOfLocalMax; }
inline float StRichCluster::amplitudeSum() const { return mAmplitudeSum; }
inline float StRichCluster::amplitude2Sum() const { return mAmplitude2Sum; }
inline float StRichCluster::rms2() const { return mRms2; }
inline void StRichCluster::increaseNumberOfLocalMax() { mNumberOfLocalMax++; }
inline void StRichCluster::increaseNumberOfPads() { mNumberOfPads++; }
inline void StRichCluster::setFirstPad(int index) { mFirstPad=index; }
inline void StRichCluster::setNumberOfPads(int newNPads)  { mNumberOfPads=newNPads; }
inline void StRichCluster::updateAmplitude(float newamp)
{
    mAmplitudeSum += newamp;
    mAmplitude2Sum += newamp*newamp;
}
inline float StRichCluster::rms2Calc()
{ return (mRms2 = mAmplitude2Sum/mNumberOfPads - (mAmplitudeSum*mAmplitudeSum/mNumberOfPads/mNumberOfPads)); }
inline void StRichCluster::setMinimumAmplitudeOfLocalMax(float newLocMax)
{ mMinimumAmplitudeOfLocalMax = newLocMax; }
#endif

