/***************************************************************************
 *
 * $Id: StRichCluster.h,v 1.1 2000/04/05 16:39:23 lasiuk Exp $
 *
 * Author: bl
 ***************************************************************************
 *
 * Description: Definition of the Cluster  object (Internal
 *  The work is done in:
 *
 ***************************************************************************
 *
 * $Log: StRichCluster.h,v $
 * Revision 1.1  2000/04/05 16:39:23  lasiuk
 * Initial Revision
 *
 * Revision 1.1  2000/04/05 16:39:23  lasiuk
 * Initial Revision
 *
 ***************************************************************************/
#ifndef ST_RICH_CLUSTER
#define ST_RICH_CLUSTER

#include <iostream.h>
#include <vector>

#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

#include "StGlobals.hh"
#include "StThreeVector.hh"

class StRichCluster {
public:
    StRichCluster();
    ~StRichCluster();

    //StRichCluster(const StRichCluster&) {}
    //StRichCluster& operator=(const StRichCluster&) {}
    
    bool operator==(const StRichCluster&) const;
    
    int    numberOfPads()        const;
    int    numberOfLocalMax()    const;
    int    firstPad()            const;
    double minimumAmplitudeOfLocalMax() const;
    double amplitudeSum()       const;
    double amplitude2Sum()      const;
    double rms2()               const;

    void   increaseNumberOfLocalMax();
    void   increaseNumberOfPads();
    void   setFirstPad(int index);
    void   setNumberOfPads(int newNPads);
    void   updateAmplitude(double newamp);
    double rms2Calc();
    void   setMinimumAmplitudeOfLocalMax(double newLocMax);
    //int    split(vector<StRichCluster>&, StRichSinglePixelCollection&, int );
    //void   dump(const StRichSinglePixelCollection&, ostream& = cout);
    
private:
   int    mNumberOfPads;     // number of associated pads
   int    mNumberOfLocalMax; // number of local maxima
   int    mFirstPad;         // index of first cluster pad
   double mMinimumAmplitudeOfLocalMax;   // lowest amplitude of all associated local maxima 
   double mAmplitudeSum;     // sum of all pad amplitudes
   double mAmplitude2Sum;    // sum of squares of all pad amplitudes
   double mRms2;             // square rms of pad amplitudes
};

inline int StRichCluster::numberOfPads() const { return mNumberOfPads; } 
inline int StRichCluster::numberOfLocalMax() const { return mNumberOfLocalMax; } 
inline int StRichCluster::firstPad() const { return mFirstPad; } 
inline double StRichCluster::minimumAmplitudeOfLocalMax() const { return mMinimumAmplitudeOfLocalMax; } 
inline double StRichCluster::amplitudeSum() const { return mAmplitudeSum; } 
inline double StRichCluster::amplitude2Sum() const { return mAmplitude2Sum; } 
inline double StRichCluster::rms2() const { return mRms2; } 
inline void StRichCluster::increaseNumberOfLocalMax() { mNumberOfLocalMax++; } 
inline void StRichCluster::increaseNumberOfPads() { mNumberOfPads++; } 
inline void StRichCluster::setFirstPad(int index) { mFirstPad=index; } 
inline void StRichCluster::setNumberOfPads(int newNPads)  { mNumberOfPads=newNPads; } 
inline void StRichCluster::updateAmplitude(double newamp)
{
    mAmplitudeSum += newamp;
    mAmplitude2Sum += newamp*newamp;
}
inline double StRichCluster::rms2Calc()
{ return (mRms2 = mAmplitude2Sum/mNumberOfPads - (mAmplitudeSum*mAmplitudeSum/mNumberOfPads/mNumberOfPads)); }
inline void StRichCluster::setMinimumAmplitudeOfLocalMax(double newLocMax)
{ mMinimumAmplitudeOfLocalMax = newLocMax; } 

#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StRichCluster*>    ClusterVector;
#else
typedef vector<StRichCluster*, allocator<StRichSinglePixel*> >    ClusterVector;
#endif

#endif

