/***************************************************************************
 *
 * $Id: StRichSimpleCluster.h,v 1.1 2000/05/18 11:34:26 lasiuk Exp $
 *
 * Author: bl
 ***************************************************************************
 *
 * Description: Definition of the Cluster  object (Internal
 *  The work is done in:
 *
 ***************************************************************************
 *
 * $Log: StRichSimpleCluster.h,v $
 * Revision 1.1  2000/05/18 11:34:26  lasiuk
 * iRename revision
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

class StRichSimpleCluster {
public:
    StRichSimpleCluster();
    ~StRichSimpleCluster();

    //StRichSimpleCluster(const StRichSimpleCluster&) {}
    //StRichSimpleCluster& operator=(const StRichSimpleCluster&) {}
    
    bool operator==(const StRichSimpleCluster&) const;
    
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
    //int    split(vector<StRichSimpleCluster>&, StRichSinglePixelCollection&, int );
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

inline int StRichSimpleCluster::numberOfPads() const { return mNumberOfPads; } 
inline int StRichSimpleCluster::numberOfLocalMax() const { return mNumberOfLocalMax; } 
inline int StRichSimpleCluster::firstPad() const { return mFirstPad; } 
inline double StRichSimpleCluster::minimumAmplitudeOfLocalMax() const { return mMinimumAmplitudeOfLocalMax; } 
inline double StRichSimpleCluster::amplitudeSum() const { return mAmplitudeSum; } 
inline double StRichSimpleCluster::amplitude2Sum() const { return mAmplitude2Sum; } 
inline double StRichSimpleCluster::rms2() const { return mRms2; } 
inline void StRichSimpleCluster::increaseNumberOfLocalMax() { mNumberOfLocalMax++; } 
inline void StRichSimpleCluster::increaseNumberOfPads() { mNumberOfPads++; } 
inline void StRichSimpleCluster::setFirstPad(int index) { mFirstPad=index; } 
inline void StRichSimpleCluster::setNumberOfPads(int newNPads)  { mNumberOfPads=newNPads; } 
inline void StRichSimpleCluster::updateAmplitude(double newamp)
{
    mAmplitudeSum += newamp;
    mAmplitude2Sum += newamp*newamp;
}
inline double StRichSimpleCluster::rms2Calc()
{ return (mRms2 = mAmplitude2Sum/mNumberOfPads - (mAmplitudeSum*mAmplitudeSum/mNumberOfPads/mNumberOfPads)); }
inline void StRichSimpleCluster::setMinimumAmplitudeOfLocalMax(double newLocMax)
{ mMinimumAmplitudeOfLocalMax = newLocMax; } 

#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StRichSimpleCluster*>    ClusterVector;
#else
typedef vector<StRichSimpleCluster*, allocator<StRichSimpleCluster*> >    ClusterVector;
#endif

#endif

