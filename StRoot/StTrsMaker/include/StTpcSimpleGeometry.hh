/**********************************************************************
 *
 * $Id: StTpcSimpleGeometry.hh,v 1.7 2012/06/11 15:04:55 fisyak Exp $
 *
 * Author: brian May 20, 1998
 *
 **********************************************************************
 *
 * Description:  Database interface for Geometrical parameters
 *               for the STAR Main TPC
 *
 **********************************************************************
 *
 * $Log: StTpcSimpleGeometry.hh,v $
 * Revision 1.7  2012/06/11 15:04:55  fisyak
 * std namespace
 *
 * Revision 1.6  2003/09/02 17:59:16  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.5  2000/01/10 23:11:30  lasiuk
 * Include MACROS for compatibility with SUN CC5.0
 *
 * Revision 1.4  1999/10/11 23:55:10  calderon
 * Version with Database Access and persistent file.
 * Not fully tested due to problems with cons, it
 * doesn't find the local files at compile time.
 * Yuri suggests forcing commit to work directly with
 * files in repository.
 *
 * Revision 1.3  1999/04/07 00:47:50  lasiuk
 * add z offset for driftLength
 *
 * Revision 1.2  1998/12/15 11:20:38  lasiuk
 * add i/o sector spacing = 3 mm
 *
 * Revision 1.1  1998/11/10 17:12:07  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.7  1998/11/08 17:01:39  lasiuk
 * allocators for vectors, resize() for LINUX compatibility
 *
 * Revision 1.6  1998/11/05 18:18:32  lasiuk
 * additional wire info
 *
 * Revision 1.5  1998/11/04 20:19:49  lasiuk
 * ensure unit integrity
 *
 * Revision 1.4  1998/10/22 14:59:32  lasiuk
 * include pad length functions
 *
 * Revision 1.3  1998/06/30 22:55:45  lasiuk
 * added anode wire member functions
 *
 * Revision 1.2  1998/05/25 17:01:57  lasiuk
 * No drift velocity (SC now)
 * padrows in STL container
 *
 * Revision 1.1  1998/05/21 21:27:38  lasiuk
 * Initial revision
 *
 **********************************************************************/
#ifndef ST_TPC_SIMPLE_GEOMETRY_HH
#define ST_TPC_SIMPLE_GEOMETRY_HH

#include <Stiostream.h>
#include <vector>
using std::vector;

#include "StGlobals.hh"
#include "StThreeVector.hh"
#include "StTpcGeometry.hh"

class StTpcSimpleGeometry : public StTpcGeometry {
    
public:
    ~StTpcSimpleGeometry();
    //StTpcSimpleGeometry(const StTpcSimpleGeometry&);
    //StTpcSimpleGeometry& operator=(cont StTpcSimpleGeometry&);
    
    static StTpcGeometry* instance();
    static StTpcGeometry* instance(const char*);

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
    
    int    numberOfSectors()      const;
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

protected:
    StTpcSimpleGeometry();
    StTpcSimpleGeometry(const char*);

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

inline int StTpcSimpleGeometry::numberOfRows() const {return(mPadRows);}
inline int StTpcSimpleGeometry::numberOfInnerRows() const {return(mInnerPadRows);}
inline int StTpcSimpleGeometry::numberOfInnerRows48() const {return(mInnerPadRows48);}
inline int StTpcSimpleGeometry::numberOfInnerRows52() const {return(mInnerPadRows52);}
inline int StTpcSimpleGeometry::numberOfOuterRows() const {return(mOuterPadRows);}
inline int StTpcSimpleGeometry::numberOfTimeBuckets() const {return(mTimeBuckets);}
inline int StTpcSimpleGeometry::numberOfSectors() const {return(mSectors);}
inline double StTpcSimpleGeometry::innerSectorRowPitch1() const {return (mInnerSectorRowPitch1);}
inline double StTpcSimpleGeometry::innerSectorRowPitch2() const {return (mInnerSectorRowPitch2);}
inline double StTpcSimpleGeometry::outerSectorRowPitch() const {return (mOuterSectorRowPitch);}

inline double StTpcSimpleGeometry::innerSectorPadWidth() const {return (mInnerSectorPadWidth);}
inline double StTpcSimpleGeometry::outerSectorPadWidth() const {return (mOuterSectorPadWidth);}
inline double StTpcSimpleGeometry::innerSectorPadLength() const {return (mInnerSectorPadLength);}
inline  double StTpcSimpleGeometry::outerSectorPadLength() const {return (mOuterSectorPadLength);}

inline double StTpcSimpleGeometry::innerSectorPadPitch() const {return (mInnerSectorPadPitch);}
inline double StTpcSimpleGeometry::outerSectorPadPitch() const {return (mOuterSectorPadPitch);}

inline double StTpcSimpleGeometry::frischGrid() const {return (mFrischGrid);}
inline double StTpcSimpleGeometry::endCapZ()    const {return (mEndCapZ);}
inline double StTpcSimpleGeometry::driftDistance() const {return (mDriftDistance);}
inline double StTpcSimpleGeometry::ifcRadius()    const {return (mIfcRadius);}
inline double StTpcSimpleGeometry::ofcRadius()    const {return (mOfcRadius);}
inline double StTpcSimpleGeometry::innerSectorzOffSet() const {return mInnerSectorzOffSet;}
inline double StTpcSimpleGeometry::outerSectorzOffSet() const {return mOuterSectorzOffSet;}

// Wires
inline double StTpcSimpleGeometry::anodeWireRadius() const {return mAnodeWireRadius;}
inline double StTpcSimpleGeometry::frischGridWireRadius() const {return mFrischGridWireRadius;}
inline double StTpcSimpleGeometry::gateWireRadius() const {return mGateWireRadius;}
    
inline double StTpcSimpleGeometry::anodeWirePitch() const {return mAnodeWirePitch;}
inline double StTpcSimpleGeometry::frischGridPitch() const {return mFrischGridWirePitch;}
inline double StTpcSimpleGeometry::gatePitch() const {return mGateWirePitch;}

inline double StTpcSimpleGeometry::innerSectorAnodeWirePadPlaneSeparation() const {return mInnerSectorAnodeWirePadPlaneSeparation;}
inline double StTpcSimpleGeometry::innerSectorFrischGridPadPlaneSeparation() const {return mInnerSectorFrischGridPadPlaneSeparation;}
inline double StTpcSimpleGeometry::innerSectorGatingGridPadPlaneSeparation() const {return mInnerSectorGatingGridPadPlaneSeparation;}
inline double StTpcSimpleGeometry::outerSectorAnodeWirePadPlaneSeparation() const {return mOuterSectorAnodeWirePadPlaneSeparation;}
inline double StTpcSimpleGeometry::outerSectorFrischGridPadPlaneSeparation() const {return mOuterSectorFrischGridPadPlaneSeparation;}
inline double StTpcSimpleGeometry::outerSectorGatingGridPadPlaneSeparation() const {return mOuterSectorGatingGridPadPlaneSeparation;}


inline double StTpcSimpleGeometry::firstInnerSectorAnodeWire() const {return (mFirstInnerSectorAnodeWire);}
inline double StTpcSimpleGeometry::lastInnerSectorAnodeWire() const {return (mLastInnerSectorAnodeWire);}
inline int StTpcSimpleGeometry::numberOfInnerSectorAnodeWires() const {return (mNumberOfInnerSectorAnodeWires);}

inline double StTpcSimpleGeometry::firstOuterSectorAnodeWire() const{ return (mFirstOuterSectorAnodeWire);}
inline double StTpcSimpleGeometry::lastOuterSectorAnodeWire() const{ return (mLastOuterSectorAnodeWire);}
inline int StTpcSimpleGeometry::numberOfOuterSectorAnodeWires() const { return (mNumberOfOuterSectorAnodeWires);}

inline double StTpcSimpleGeometry::innerSectorEdge() const { return (mInnerSectorEdge);}
inline double StTpcSimpleGeometry::outerSectorEdge() const { return (mOuterSectorEdge);}
inline double StTpcSimpleGeometry::ioSectorSpacing() const { return (mIoSectorSpacing);}
#endif
