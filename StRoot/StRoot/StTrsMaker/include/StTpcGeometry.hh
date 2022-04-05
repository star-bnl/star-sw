/**********************************************************************
 *
 * $Id: StTpcGeometry.hh,v 1.4 1999/10/11 23:55:10 calderon Exp $
 *
 * Author: brian May 20, 1998
 *
 **********************************************************************
 *
 * Description:  Abstract Class interface for Geometrical parameters
 *
 **********************************************************************
 *
 * $Log: StTpcGeometry.hh,v $
 * Revision 1.4  1999/10/11 23:55:10  calderon
 * Version with Database Access and persistent file.
 * Not fully tested due to problems with cons, it
 * doesn't find the local files at compile time.
 * Yuri suggests forcing commit to work directly with
 * files in repository.
 *
 * Revision 1.3  1999/04/07 00:47:49  lasiuk
 * add z offset for driftLength
 *
 * Revision 1.2  1998/12/15 11:20:36  lasiuk
 * add i/o sector spacing = 3 mm
 *
 * Revision 1.1  1998/11/10 17:12:06  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.5  1998/11/05 18:18:31  lasiuk
 * additional wire info
 *
 * Revision 1.4  1998/11/04 20:19:48  lasiuk
 * ensure unit integrity
 *
 * Revision 1.3  1998/10/22 14:59:30  lasiuk
 * include pad length functions
 *
 * Revision 1.2  1998/06/30 22:55:43  lasiuk
 * added anode wire member functions
 *
 * Revision 1.1  1998/05/21 21:27:38  lasiuk
 * Initial revision
 **********************************************************************/
#ifndef ST_TPC_GEOMETRY_HH
#define ST_TPC_GEOMETRY_HH

#include "StGlobals.hh"
#include "StThreeVector.hh"

class StTpcGeometry {
public:
    virtual ~StTpcGeometry() {}
    //StTpcGeometry(const StTpcGeometry&);
    //StTpcGeometry& operator=(cont StTpcGeometry&);
    
    // Rows
    virtual int    numberOfRows()         const = 0;
    virtual int    numberOfInnerRows()    const = 0;
    virtual int    numberOfInnerRows48()  const = 0;
    virtual int    numberOfInnerRows52()  const = 0;
    virtual int    numberOfOuterRows()    const = 0;
    virtual double innerSectorRowPitch1() const = 0;
    virtual double innerSectorRowPitch2() const = 0;
    virtual double outerSectorRowPitch()  const = 0;
    
    virtual int    numberOfPadsAtRow(int)   const = 0;
    virtual double radialDistanceAtRow(int) const = 0;

    virtual int    numberOfSectors()      const = 0;
    // Time buckets
    virtual int    numberOfTimeBuckets()  const = 0;
    
    // Pads
    virtual double innerSectorPadWidth()    const = 0;
    virtual double outerSectorPadWidth()    const = 0;
    virtual double innerSectorPadLength()   const = 0;
    virtual double outerSectorPadLength()   const = 0;
    virtual double innerSectorPadPitch()    const = 0;
    virtual double outerSectorPadPitch()    const = 0;

    // Sector Dimensions
    virtual double innerSectorEdge()        const = 0;
    virtual double outerSectorEdge()        const = 0;
    virtual double ioSectorSpacing()        const = 0;
    
    // Wire Plane
    virtual double anodeWireRadius()                         const = 0;
    virtual double frischGridWireRadius()                    const = 0;
    virtual double gateWireRadius()                          const = 0;
    
    virtual double anodeWirePitch()                          const = 0;
    virtual double frischGridPitch()                         const = 0;
    virtual double gatePitch()                               const = 0;
    
    virtual double innerSectorAnodeWirePadPlaneSeparation()  const = 0;
    virtual double innerSectorFrischGridPadPlaneSeparation() const = 0;
    virtual double innerSectorGatingGridPadPlaneSeparation() const = 0;

    virtual double outerSectorAnodeWirePadPlaneSeparation()  const = 0;
    virtual double outerSectorFrischGridPadPlaneSeparation() const = 0;
    virtual double outerSectorGatingGridPadPlaneSeparation() const = 0;

    virtual int    numberOfInnerSectorAnodeWires() const = 0;
    virtual double firstInnerSectorAnodeWire()     const = 0;
    virtual double lastInnerSectorAnodeWire()      const = 0;
    virtual double innerSectorAnodeWire(int)       const = 0;

    virtual int    numberOfOuterSectorAnodeWires() const = 0;
    virtual double firstOuterSectorAnodeWire()     const = 0;
    virtual double lastOuterSectorAnodeWire()      const = 0;
    virtual double outerSectorAnodeWire(int)       const = 0;
    
    // General -- Field Cage
    virtual double frischGrid()             const = 0;
    virtual double driftDistance()          const = 0;
    virtual double ifcRadius()              const = 0;
    virtual double ofcRadius()              const = 0;
    virtual double endCapZ()                const = 0;
    virtual double innerSectorzOffSet()     const = 0;
    virtual double outerSectorzOffSet()     const = 0;

    // careful for sun
    virtual bool   acceptance(StThreeVector<StDouble>&) const = 0;

    // print out complete database
    virtual void print(ostream& os = cout)              const = 0;    
};

#endif
