/***************************************************************************
 *
 * $Id: StRichCluster.h,v 2.1 2000/05/22 21:44:56 ullrich Exp $
 *
 * Author: bl
 ***************************************************************************
 *
 * Description: Definition of the Cluster  object (Persistent)
 *
 ***************************************************************************
 *
 * $Log: StRichCluster.h,v $
 * Revision 2.1  2000/05/22 21:44:56  ullrich
 * Initial Revision
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
    StRichCluster(Int_t nPads, Int_t nLocMax, Int_t fPad, Float_t ampSum, Float_t amp2Sum, Float_t rms2);
    //StRichCluster(const StRichCluster&) {}
    //StRichCluster& operator=(const StRichCluster&) {}
    
    Int_t    operator==(const StRichCluster&) const;
    
    Int_t    numberOfPads()        const;
    Int_t    numberOfLocalMax()    const;
    Int_t    firstPad()            const;
    Float_t  minimumAmplitudeOfLocalMax() const;
    Float_t  amplitudeSum()       const;
    Float_t  amplitude2Sum()      const;
    Float_t  rms2()               const;
    Float_t  rms2Calc();
    
    void   increaseNumberOfLocalMax();
    void   increaseNumberOfPads();
    void   setFirstPad(Int_t index);
    void   setNumberOfPads(Int_t newNPads);
    void   updateAmplitude(Float_t newamp);
    void   setMinimumAmplitudeOfLocalMax(Float_t newLocMax);
    
private:
   Int_t    mNumberOfPads;     // number of associated pads
   Int_t    mNumberOfLocalMax; // number of local maxima
   Int_t    mFirstPad;         // index of first cluster pad
   Float_t  mMinimumAmplitudeOfLocalMax;   // lowest amplitude of all associated local maxima 
   Float_t  mAmplitudeSum;     // sum of all pad amplitudes
   Float_t  mAmplitude2Sum;    // sum of squares of all pad amplitudes
   Float_t  mRms2;             // square rms of pad amplitudes
};

inline Int_t StRichCluster::numberOfPads() const { return mNumberOfPads; } 
inline Int_t StRichCluster::numberOfLocalMax() const { return mNumberOfLocalMax; } 
inline Int_t StRichCluster::firstPad() const { return mFirstPad; } 
inline Float_t StRichCluster::minimumAmplitudeOfLocalMax() const { return mMinimumAmplitudeOfLocalMax; } 
inline Float_t StRichCluster::amplitudeSum() const { return mAmplitudeSum; } 
inline Float_t StRichCluster::amplitude2Sum() const { return mAmplitude2Sum; } 
inline Float_t StRichCluster::rms2() const { return mRms2; } 
inline void StRichCluster::increaseNumberOfLocalMax() { mNumberOfLocalMax++; } 
inline void StRichCluster::increaseNumberOfPads() { mNumberOfPads++; } 
inline void StRichCluster::setFirstPad(Int_t index) { mFirstPad=index; } 
inline void StRichCluster::setNumberOfPads(Int_t newNPads)  { mNumberOfPads=newNPads; } 
inline void StRichCluster::updateAmplitude(Float_t newamp)
{
    mAmplitudeSum += newamp;
    mAmplitude2Sum += newamp*newamp;
}
inline Float_t StRichCluster::rms2Calc()
{ return (mRms2 = mAmplitude2Sum/mNumberOfPads - (mAmplitudeSum*mAmplitudeSum/mNumberOfPads/mNumberOfPads)); }
inline void StRichCluster::setMinimumAmplitudeOfLocalMax(Float_t newLocMax)
{ mMinimumAmplitudeOfLocalMax = newLocMax; } 
#endif

