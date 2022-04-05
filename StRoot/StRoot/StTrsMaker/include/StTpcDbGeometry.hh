/**********************************************************************
 *
 * $Id: StTpcDbGeometry.hh,v 1.5 2012/06/11 15:04:55 fisyak Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez & Brian Lasiuk Sept 13, 1999
 *
 **********************************************************************
 *
 * Description:  Database interface for Geometrical parameters
 *               for the STAR Main TPC using the Star Tpc DB.
 *
 **********************************************************************
 *
 * $Log: StTpcDbGeometry.hh,v $
 * Revision 1.5  2012/06/11 15:04:55  fisyak
 * std namespace
 *
 * Revision 1.4  2003/09/02 17:59:16  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  2000/02/10 01:21:46  calderon
 * Switch to use StTpcDb.
 * Coordinates checked for consistency.
 * Fixed problems with StTrsIstream & StTrsOstream.
 *
 * Revision 1.2  2000/01/10 23:11:29  lasiuk
 * Include MACROS for compatibility with SUN CC5.0
 *
 * Revision 1.1  1999/10/11 23:55:09  calderon
 * Version with Database Access and persistent file.
 * Not fully tested due to problems with cons, it
 * doesn't find the local files at compile time.
 * Yuri suggests forcing commit to work directly with
 * files in repository.
 *
 ***********************************************************************/
#ifndef ST_TPC_DB_GEOMETRY_HH
#define ST_TPC_DB_GEOMETRY_HH
#include <Stiostream.h>
#include <vector>

using std::vector;

#include "StGlobals.hh"
#include "StThreeVector.hh"
#include "StTpcGeometry.hh"

class StTpcDb;
class StTpcDbGeometry : public StTpcGeometry {
    
public:
    ~StTpcDbGeometry();
    //StTpcDbGeometry(const StTpcDbGeometry&);
    //StTpcDbGeometry& operator=(cont StTpcDbGeometry&);
    
    static StTpcGeometry* instance();
    static StTpcGeometry* instance(StTpcDb*);

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

    int    numberOfSectors()               const;
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
    double innerSectorzOffSet()         const;
    double outerSectorzOffSet()         const;
    
    bool   acceptance(StThreeVector<StDouble>&) const;

    // Diagnostic: print out complete database
    void print(ostream& os = cout)              const;

private:
    //StTpcDbGeometry();
    StTpcDbGeometry(StTpcDb*);

private:
    static StTpcGeometry*    mInstance;
    StTpcDb* gTpcDbPtr;

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
    double mInnerSectorzOffSet;
    double mOuterSectorzOffSet;
    
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

inline int StTpcDbGeometry::numberOfRows() const {return(mPadRows);}
inline int StTpcDbGeometry::numberOfInnerRows() const {return(mInnerPadRows);}
inline int StTpcDbGeometry::numberOfInnerRows48() const {return(mInnerPadRows48);}
inline int StTpcDbGeometry::numberOfInnerRows52() const {return(mInnerPadRows52);}
inline int StTpcDbGeometry::numberOfOuterRows() const {return(mOuterPadRows);}
inline int StTpcDbGeometry::numberOfTimeBuckets() const {return(mTimeBuckets);}
inline int StTpcDbGeometry::numberOfSectors() const {return(mSectors);}
inline double StTpcDbGeometry::innerSectorRowPitch1() const {return (mInnerSectorRowPitch1);}
inline double StTpcDbGeometry::innerSectorRowPitch2() const {return (mInnerSectorRowPitch2);}
inline double StTpcDbGeometry::outerSectorRowPitch() const {return (mOuterSectorRowPitch);}

inline double StTpcDbGeometry::innerSectorPadWidth() const {return (mInnerSectorPadWidth);}
inline double StTpcDbGeometry::outerSectorPadWidth() const {return (mOuterSectorPadWidth);}
inline double StTpcDbGeometry::innerSectorPadLength() const {return (mInnerSectorPadLength);}
inline  double StTpcDbGeometry::outerSectorPadLength() const {return (mOuterSectorPadLength);}

inline double StTpcDbGeometry::innerSectorPadPitch() const {return (mInnerSectorPadPitch);}
inline double StTpcDbGeometry::outerSectorPadPitch() const {return (mOuterSectorPadPitch);}

inline double StTpcDbGeometry::frischGrid() const {return (mFrischGrid);}
inline double StTpcDbGeometry::endCapZ()    const {return (mEndCapZ);}
inline double StTpcDbGeometry::driftDistance() const {return (mDriftDistance);}
inline double StTpcDbGeometry::innerSectorzOffSet() const {return mInnerSectorzOffSet;}
inline double StTpcDbGeometry::outerSectorzOffSet() const {return mOuterSectorzOffSet;}

inline double StTpcDbGeometry::ifcRadius()    const {return (mIfcRadius);}
inline double StTpcDbGeometry::ofcRadius()    const {return (mOfcRadius);}

// Wires
inline double StTpcDbGeometry::anodeWireRadius() const {return mAnodeWireRadius;}
inline double StTpcDbGeometry::frischGridWireRadius() const {return mFrischGridWireRadius;}
inline double StTpcDbGeometry::gateWireRadius() const {return mGateWireRadius;}
    
inline double StTpcDbGeometry::anodeWirePitch() const {return mAnodeWirePitch;}
inline double StTpcDbGeometry::frischGridPitch() const {return mFrischGridWirePitch;}
inline double StTpcDbGeometry::gatePitch() const {return mGateWirePitch;}

inline double StTpcDbGeometry::innerSectorAnodeWirePadPlaneSeparation() const {return mInnerSectorAnodeWirePadPlaneSeparation;}
inline double StTpcDbGeometry::innerSectorFrischGridPadPlaneSeparation() const {return mInnerSectorFrischGridPadPlaneSeparation;}
inline double StTpcDbGeometry::innerSectorGatingGridPadPlaneSeparation() const {return mInnerSectorGatingGridPadPlaneSeparation;}
inline double StTpcDbGeometry::outerSectorAnodeWirePadPlaneSeparation() const {return mOuterSectorAnodeWirePadPlaneSeparation;}
inline double StTpcDbGeometry::outerSectorFrischGridPadPlaneSeparation() const {return mOuterSectorFrischGridPadPlaneSeparation;}
inline double StTpcDbGeometry::outerSectorGatingGridPadPlaneSeparation() const {return mOuterSectorGatingGridPadPlaneSeparation;}


inline double StTpcDbGeometry::firstInnerSectorAnodeWire() const {return (mFirstInnerSectorAnodeWire);}
inline double StTpcDbGeometry::lastInnerSectorAnodeWire() const {return (mLastInnerSectorAnodeWire);}
inline int StTpcDbGeometry::numberOfInnerSectorAnodeWires() const {return (mNumberOfInnerSectorAnodeWires);}

inline double StTpcDbGeometry::firstOuterSectorAnodeWire() const{ return (mFirstOuterSectorAnodeWire);}
inline double StTpcDbGeometry::lastOuterSectorAnodeWire() const{ return (mLastOuterSectorAnodeWire);}
inline int StTpcDbGeometry::numberOfOuterSectorAnodeWires() const { return (mNumberOfOuterSectorAnodeWires);}

inline double StTpcDbGeometry::innerSectorEdge() const { return (mInnerSectorEdge);}
inline double StTpcDbGeometry::outerSectorEdge() const { return (mOuterSectorEdge);}
inline double StTpcDbGeometry::ioSectorSpacing() const { return (mIoSectorSpacing);}
#endif
