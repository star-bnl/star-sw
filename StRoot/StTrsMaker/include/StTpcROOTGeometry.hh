/**********************************************************************
 *
 * $Id: StTpcROOTGeometry.hh,v 1.1 1999/03/23 03:38:48 lasiuk Exp $
 *
 * Author: brian March 22, 1999
 *
 **********************************************************************
 *
 * Description:  Database interface for Geometrical parameters
 *               for the STAR Main TPC
 *
 **********************************************************************
 *
 * $Log: StTpcROOTGeometry.hh,v $
 * Revision 1.1  1999/03/23 03:38:48  lasiuk
 * Initial Revision
 *
 **********************************************************************/
#ifdef __ROOT__
#ifndef ST_TPC_ROOT_GEOMETRY_HH
#define ST_TPC_SIMPLE_GEOMETRY_HH
#include <iostream.h>
#include <vector>

#include "geometryDataSet.h"

#include "StGlobals.hh"
#include "StThreeVector.hh"
#include "StTpcGeometry.hh"

class StTpcROOTGeometry : public StTpcGeometry {
    
public:
    ~StTpcROOTGeometry();
    //StTpcROOTGeometry(const StTpcROOTGeometry&);
    //StTpcROOTGeometry& operator=(cont StTpcROOTGeometry&);
    
    static StTpcGeometry* instance();
    static StTpcGeometry* instance(geometryDataSet*);

    // Rows
    int    numberOfRows()                  const;
    int    numberOfInnerRows()             const;
    int    numberOfInnerRows48()           const;
    int    numberOfInnerRows52()           const;
    int    numberOfOuterRows()             const;
    int    numberOfPadsAtRow(int)          const;
    double radialDistanceAtRow(int)        const;
    double innerSectorRowPitch1()          const;
    double innerSectorRowPitch2()          const;
    double outerSectorRowPitch()           const;
    double ioSectorSpacing()               const;

    // TimeBuckets
    int    numberOfTimeBuckets()           const;
    
    // Pads
    double innerSectorPadWidth()           const;
    double outerSectorPadWidth()           const;
    double innerSectorPadLength()          const;
    double outerSectorPadLength()          const;
    double innerSectorPadPitch()           const;
    double outerSectorPadPitch()           const;

    // Wire Plane
    double anodeWireRadius()                         const;
    double frischGridWireRadius()                    const;
    double gateWireRadius()                          const;
    
    double anodeWirePitch()                          const;
    double frischGridPitch()                         const;
    double gatePitch()                               const;

    double innerSectorAnodeWirePadPlaneSeparation()  const;
    double innerSectorFrischGridPadPlaneSeparation() const;
    double innerSectorGatingGridPadPlaneSeparation() const;

    double outerSectorAnodeWirePadPlaneSeparation()  const;
    double outerSectorFrischGridPadPlaneSeparation() const;
    double outerSectorGatingGridPadPlaneSeparation() const;
    
    int    numberOfInnerSectorAnodeWires() const;
    double firstInnerSectorAnodeWire()     const;
    double lastInnerSectorAnodeWire()      const;
    double innerSectorAnodeWire(int)       const;

    
    int    numberOfOuterSectorAnodeWires() const;
    double firstOuterSectorAnodeWire()     const;
    double lastOuterSectorAnodeWire()      const;
    double outerSectorAnodeWire(int)       const;

    double innerSectorEdge()               const;
    double outerSectorEdge()               const;

    // General -- Field Cage
    double endCapZ()                    const;
    double driftDistance()              const;
    double ifcRadius()                  const;
    double ofcRadius()                  const;
    double frischGrid()                 const;
    
    bool   acceptance(StThreeVector<StDouble>&) const;

    // Diagnostic: print out complete database
    void print(ostream& os = cout)              const;

protected:
    StTpcROOTGeometry();
    StTpcROOTGeometry(geometryDataSet*);

private:
    static StTpcGeometry*    mInstance;
    
private:
    int    mPadRows;
    int    mInnerPadRows;
    int    mInnerPadRows48;
    int    mInnerPadRows52;
    int    mOuterPadRows;
    int    mTimeBuckets;
    int    mSectors;
    double mIfcRadius;
    double mOfcRadius;
    double mEndCapZ;
    double mInnerSectorPadWidth;
    double mInnerSectorPadLength;
    double mInnerSectorPadPitch;
    double mInnerSectorRowPitch1;
    double mInnerSectorRowPitch2;
    double mFirstPadRow;
    double mFirstOuterSectorPadRow;
    double mLastOuterSectorPadRow;
    double mFirstRowWidth;
    double mLastRowWidth;
    double mInnerSectorEdge;
    
    double mOuterSectorPadWidth;
    double mOuterSectorPadLength;
    double mOuterSectorPadPitch;
    double mOuterSectorRowPitch;
    double mOuterSectorLength;
    double mIoSectorSeparation;
    double mOuterSectorEdge;
    double mIoSectorSpacing;
    
    double mFrischGrid;
    double mDriftDistance;
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<int> mPadsInRow;
    vector<double> mRadialDistanceAtRow;
#else
    vector<int, allocator<int> > mPadsInRow;
    vector<double, allocator<double> > mRadialDistanceAtRow;
#endif
    // Wires
    double mAnodeWireRadius;
    double mFrischGridWireRadius;
    double mGateWireRadius;

    double mAnodeWirePitch;
    double mFrischGridWirePitch;
    double mGateWirePitch;
    
    double mInnerSectorAnodeWirePadPlaneSeparation;
    double mInnerSectorFrischGridPadPlaneSeparation;
    double mInnerSectorGatingGridPadPlaneSeparation;

    double mOuterSectorAnodeWirePadPlaneSeparation;
    double mOuterSectorFrischGridPadPlaneSeparation;
    double mOuterSectorGatingGridPadPlaneSeparation;
    
    double mFirstInnerSectorAnodeWire;
    double mLastInnerSectorAnodeWire;
    int    mNumberOfInnerSectorAnodeWires;

    double mFirstOuterSectorAnodeWire;
    double mLastOuterSectorAnodeWire;
    int    mNumberOfOuterSectorAnodeWires;
};

inline int StTpcROOTGeometry::numberOfRows() const {return(mPadRows);}
inline int StTpcROOTGeometry::numberOfInnerRows() const {return(mInnerPadRows);}
inline int StTpcROOTGeometry::numberOfInnerRows48() const {return(mInnerPadRows48);}
inline int StTpcROOTGeometry::numberOfInnerRows52() const {return(mInnerPadRows52);}
inline int StTpcROOTGeometry::numberOfOuterRows() const {return(mOuterPadRows);}
inline int StTpcROOTGeometry::numberOfTimeBuckets() const {return(mTimeBuckets);}
inline double StTpcROOTGeometry::innerSectorRowPitch1() const {return (mInnerSectorRowPitch1);}
inline double StTpcROOTGeometry::innerSectorRowPitch2() const {return (mInnerSectorRowPitch2);}
inline double StTpcROOTGeometry::outerSectorRowPitch() const {return (mOuterSectorRowPitch);}

inline double StTpcROOTGeometry::innerSectorPadWidth() const {return (mInnerSectorPadWidth);}
inline double StTpcROOTGeometry::outerSectorPadWidth() const {return (mOuterSectorPadWidth);}
inline double StTpcROOTGeometry::innerSectorPadLength() const {return (mInnerSectorPadLength);}
inline  double StTpcROOTGeometry::outerSectorPadLength() const {return (mOuterSectorPadLength);}

inline double StTpcROOTGeometry::innerSectorPadPitch() const {return (mInnerSectorPadPitch);}
inline double StTpcROOTGeometry::outerSectorPadPitch() const {return (mOuterSectorPadPitch);}

inline double StTpcROOTGeometry::frischGrid() const {return (mFrischGrid);}
inline double StTpcROOTGeometry::endCapZ()    const {return (mEndCapZ);}
inline double StTpcROOTGeometry::driftDistance() const {return (mDriftDistance);}
inline double StTpcROOTGeometry::ifcRadius()    const {return (mIfcRadius);}
inline double StTpcROOTGeometry::ofcRadius()    const {return (mOfcRadius);}

// Wires
inline double StTpcROOTGeometry::anodeWireRadius() const {return mAnodeWireRadius;}
inline double StTpcROOTGeometry::frischGridWireRadius() const {return mFrischGridWireRadius;}
inline double StTpcROOTGeometry::gateWireRadius() const {return mGateWireRadius;}
    
inline double StTpcROOTGeometry::anodeWirePitch() const {return mAnodeWirePitch;}
inline double StTpcROOTGeometry::frischGridPitch() const {return mFrischGridWirePitch;}
inline double StTpcROOTGeometry::gatePitch() const {return mGateWirePitch;}

inline double StTpcROOTGeometry::innerSectorAnodeWirePadPlaneSeparation() const {return mInnerSectorAnodeWirePadPlaneSeparation;}
inline double StTpcROOTGeometry::innerSectorFrischGridPadPlaneSeparation() const {return mInnerSectorFrischGridPadPlaneSeparation;}
inline double StTpcROOTGeometry::innerSectorGatingGridPadPlaneSeparation() const {return mInnerSectorGatingGridPadPlaneSeparation;}
inline double StTpcROOTGeometry::outerSectorAnodeWirePadPlaneSeparation() const {return mOuterSectorAnodeWirePadPlaneSeparation;}
inline double StTpcROOTGeometry::outerSectorFrischGridPadPlaneSeparation() const {return mOuterSectorFrischGridPadPlaneSeparation;}
inline double StTpcROOTGeometry::outerSectorGatingGridPadPlaneSeparation() const {return mOuterSectorGatingGridPadPlaneSeparation;}


inline double StTpcROOTGeometry::firstInnerSectorAnodeWire() const {return (mFirstInnerSectorAnodeWire);}
inline double StTpcROOTGeometry::lastInnerSectorAnodeWire() const {return (mLastInnerSectorAnodeWire);}
inline int StTpcROOTGeometry::numberOfInnerSectorAnodeWires() const {return (mNumberOfInnerSectorAnodeWires);}

inline double StTpcROOTGeometry::firstOuterSectorAnodeWire() const{ return (mFirstOuterSectorAnodeWire);}
inline double StTpcROOTGeometry::lastOuterSectorAnodeWire() const{ return (mLastOuterSectorAnodeWire);}
inline int StTpcROOTGeometry::numberOfOuterSectorAnodeWires() const { return (mNumberOfOuterSectorAnodeWires);}

inline double StTpcROOTGeometry::innerSectorEdge() const { return (mInnerSectorEdge);}
inline double StTpcROOTGeometry::outerSectorEdge() const { return (mOuterSectorEdge);}
inline double StTpcROOTGeometry::ioSectorSpacing() const { return (mIoSectorSpacing);}
#endif
#endif // __ROOT__
