/***************************************************************************
 *
 * $Id: StTrsWireHistogram.hh,v 1.5 1999/07/09 03:46:49 lasiuk Exp $
 *
 * Author: brian, May 1998 
 ***************************************************************************
 *
 * Description: Collection of all StTrsMiniChargeSegment transported to
 *              the pad-plane
 *
 ***************************************************************************
 *
 * $Log: StTrsWireHistogram.hh,v $
 * Revision 1.5  1999/07/09 03:46:49  lasiuk
 * add switch for singleElectron multiplication, gaussian random
 * number generator
 *
 * Revision 1.5  1999/07/09 03:46:49  lasiuk
 * add switch for singleElectron multiplication, gaussian random
 * number generator
 *
 * Revision 1.4  1999/06/16 14:26:51  fisyak
 * Add flags for egcs on Solaris
 *
 * Revision 1.3  1999/02/10 18:03:27  lasiuk
 * gas gain manual setting
 *
 * Revision 1.2  1998/11/16 14:50:22  lasiuk
 * remove diagnostics (mLastEntry, mLastWire)
 *
 * Revision 1.1  1998/11/10 17:12:13  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.5  1998/11/08 17:07:53  lasiuk
 * change from boolean macros
 * use resize() for LINUX/ allocators for SUN
 * add typedefs for vector<> types
 *
 * Revision 1.4  1998/11/03 17:31:47  lasiuk
 * incorporate gas gain/fluctuations
 * add time delay of collection depending on charge position wrt wire position
 * rename wire()
 *
 * Revision 1.3  1998/10/22 00:23:26  lasiuk
 * Oct 22
 *
 * Revision 1.2  1998/06/30 22:56:59  lasiuk
 * added clear(), wire() and wireHistogram()
 *
 * Revision 1.1  1998/06/04 23:32:21  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_TRS_WIRE_HISTOGRAM_HH
#define ST_TRS_WIRE_HISTOGRAM_HH

#include <vector>

#if  defined(__sun) && ! defined(__GNUG__)
#include <stdcomp.h>
#endif

#include "StThreeVector.hh"
#include "Randomize.h"

#include "StTpcGeometry.hh"
#include "StTpcSlowControl.hh"
#include "StTrsWireBinEntry.hh"

#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StTrsWireBinEntry> aTpcWire;
typedef vector<aTpcWire>          aTpcWirePlane;
#else
typedef vector<StTrsWireBinEntry, allocator<StTrsWireBinEntry> > aTpcWire;
typedef vector<aTpcWire, allocator<aTpcWire> >                   aTpcWirePlane;
#endif

    
class StTrsWireHistogram {
public:
    static StTrsWireHistogram* instance(StTpcGeometry*, StTpcSlowControl*);
    ~StTrsWireHistogram();

    //StTrsWireHistogram(const StTrsWireHistogramy&);
    //StTrsWireHistogram& operator=(cont StTrsWireHistogram&);

    // access functions
    int min() const;
    int max() const;
    
    // Book-keeping
    void                addEntry(StTrsWireBinEntry&);
    void                clear();
    
    // Wire Operations (and access)
    double              wireCoordinate(int) const;
    aTpcWire&           getWire(int)             ;
    aTpcWirePlane&      getWireHistogram()       ;

    
    //Maybe needed later?
    //void sortTimeBins();
    //void putWire(vector<StTrsWireBinEntry>&);

    // Gas Gain
    void   setDoGasGain(bool)            ;
    void   setGasGainInnerSector(double) ;
    void   setGasGainOuterSector(double) ;
    void   setDoGasGainFluctuations(bool);
    void   setDoSingleElectronMultiplication(bool);
    double avalanche(int)                ;
    double gaussianMultiplication(int)   ;
  
    // Time Delay For Charge Collection
    void   setDoTimeDelay(bool);
    
private:
    void   gasGainCalculation()                ;
    double exponentialFluctuations(int)   const;
    double noFluctuations(int)            const;
    //double polyaFluctuations(int)         const;

private:
    StTrsWireHistogram(StTpcGeometry*, StTpcSlowControl*);
    
private:
    int             mMin;                      // minimum bin filled
    int             mMax;                      // maximum bin filled
    int             mNumberOfInnerSectorAnodeWires; // from dataBase
    int             mNumberOfOuterSectorAnodeWires; // from dataBase
    int             mTotalNumberOfAnodeWires;

    // Gas Gain
    bool            mDoGasGain;
    bool            mDoGasGainFluctuations;
    bool            mGasGainCalculationDone;
    bool            mDoSingleElectronMultiplication;
    double          mInnerSectorGasGain;
    double          mOuterSectorGasGain;

    // Time Delay
    bool            mDoTimeDelay;
    
    StTpcGeometry*    mGeomDb;
    StTpcSlowControl* mSCDb;
    
    aTpcWirePlane      mSectorWires;

    static StTrsWireHistogram* mInstance;

    static HepJamesRandom  mEngine;
    static RandGauss       mGaussianDistribution;
    static RandExponential mExponentialDistribution;
};
int inline StTrsWireHistogram::min() const {return mMin;}
int inline StTrsWireHistogram::max() const {return mMax;}
void inline StTrsWireHistogram::setDoGasGain(bool gg) {mDoGasGain = gg;}
void inline StTrsWireHistogram::setDoTimeDelay(bool t) {mDoTimeDelay = t;}
void inline StTrsWireHistogram::setDoSingleElectronMultiplication(bool t) {mDoSingleElectronMultiplication = t;}
#endif
