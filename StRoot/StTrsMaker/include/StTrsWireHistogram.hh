/***************************************************************************
 *
 * $Id: StTrsWireHistogram.hh,v 1.9 2001/02/15 21:34:48 perev Exp $
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
 * Revision 1.9  2001/02/15 21:34:48  perev
 * clear improved
 *
 * Revision 1.8  2000/07/30 02:43:13  long
 * add erf function look up table,table builder,etc.
 *
 * Revision 1.7  1999/12/08 02:10:25  calderon
 * Modified to eliminate warnings on Linux.
 *
 * Revision 1.6  1999/07/19 21:39:31  lasiuk
 * - addEntry() distributes charge on a (user) settable range of wires
 * - setRangeOfWiresForChargeDistribution(int) added (default is 0)
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
#include "TRandom.h"

#include "StTpcGeometry.hh"
#include "StTpcSlowControl.hh"
#include "StTrsDeDx.hh"
#include "StMagneticField.hh"
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
    static StTrsWireHistogram* instance(StTpcGeometry*, StTpcSlowControl*, StTrsDeDx*,StMagneticField*);
    static void dropit(){delete mInstance;mInstance=0;}
    ~StTrsWireHistogram();

    //StTrsWireHistogram(const StTrsWireHistogramy&);
    //StTrsWireHistogram& operator=(cont StTrsWireHistogram&);

    // access functions
    int minWire() const;
    int maxWire() const;
    
    // Book-keeping
    void                addEntry(StTrsWireBinEntry&,int );
    void                clear();
    
    // Wire Operations (and access)
    double              wireCoordinate(int) const;
    aTpcWire&           getWire(int)             ;
    aTpcWirePlane&      getWireHistogram()       ;
    void    FreqFunctionTableBuilder();
    double  table_fast(double) const;
 
#ifndef ST_NO_TEMPLATE_DEF_ARGS 
  vector<double> mFreqFunctionTable; 
  
#else

  vector<double, allocator<double> > mFreqFunctionTable; 
  
  
#endif
  double         mNumberOfEntriesInTable;
  double         mRangeOfTable ;
    //Maybe needed later?
    //void sortTimeBins();
    //void putWire(vector<StTrsWireBinEntry>&);

  
    
    // Gas Gain
    void   setDoGasGain(bool)            ;
    void   setGasGainInnerSector(double) ;
    void   setGasGainOuterSector(double) ;
    void   setDoGasGainFluctuations(bool);
    void   setDoSingleElectronMultiplication(bool);
    double exponentialAvalanche(int, double)      ;
    double polyaAvalanche(int, double)      ; 
  double polya()      ;//b=0.4
    double gaussianAvalanche(int, double)         ;
  
    // Time Delay For Charge Collection
    void   setDoTimeDelay(bool);
    
private:
    void   gasGainCalculation()                ;
    
    double noFluctuations(int)            const;
    TRandom *random;  
private:
    StTrsWireHistogram(StTpcGeometry*, StTpcSlowControl*, StTrsDeDx*,StMagneticField*);
    
private:
    int             mMin;                      // minimum bin filled
    int             mMax;                      // maximum bin filled

    StTpcGeometry*    mGeomDb;
    StTpcSlowControl* mSCDb;
    StTrsDeDx*        mGasDb;
     
  //  void   setRangeOfWiresForChargeDistribution(int);

    // Gas Gain
    bool            mDoGasGain;
    bool            mDoGasGainFluctuations;
    bool            mGasGainCalculationDone;
    bool            mDoSingleElectronMultiplication;

    // Time Delay
    bool            mDoTimeDelay;
    
    // Charge Distribution
    
    aTpcWirePlane      mSectorWires;
    
    int             mNumberOfInnerSectorAnodeWires; // from dataBase
    int             mNumberOfOuterSectorAnodeWires; // from dataBase
    int             mTotalNumberOfAnodeWires;

    double          mInnerSectorGasGain;
    double          mOuterSectorGasGain;
   
    static StTrsWireHistogram* mInstance;

    static HepJamesRandom  mEngine;
    static RandGauss       mGaussianDistribution;
    static RandExponential mExponentialDistribution;
     float           mOmegaTau;
     float dx[4]; 
     float dz[4];
    
};
int inline StTrsWireHistogram::minWire() const {return mMin;}
int inline StTrsWireHistogram::maxWire() const {return mMax;}
void inline StTrsWireHistogram::setDoGasGain(bool gg) {mDoGasGain = gg;}
void inline StTrsWireHistogram::setDoTimeDelay(bool t) {mDoTimeDelay = t;}
void inline StTrsWireHistogram::setDoSingleElectronMultiplication(bool t) {mDoSingleElectronMultiplication = t;}





;

#endif
