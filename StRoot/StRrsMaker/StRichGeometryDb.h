/*****************************************************************
 * $Id: StRichGeometryDb.h,v 1.5 2000/03/12 22:18:45 lasiuk Exp $
 *
 * Description:
 *  Both have common_fill,star_fill and my_fill private
 *  functions, that are called from the ctors. The firsts
 *  hold any member initializations. The second is the 
 *  main way to fill the DBs - from STAR central database.
 *  The last fill is for times where the central DB is
 *  not accessible. 
 *
 *  Both classes are accessible through a static 
 *  member, getDB(), that returns a pointer to the 
 *  only instance.
 *
 *****************************************************************
 * $Log: StRichGeometryDb.h,v $
 * Revision 1.5  2000/03/12 22:18:45  lasiuk
 * add from materials Db
 * add normal vector value
 *
 * Revision 1.5  2000/03/12 22:18:45  lasiuk
 * add from materials Db
 * add normal vector value
 *
 * Revision 1.4  2000/02/12 21:55:45  lasiuk
 * Wire position adjustment
 *
 * Revision 1.3  2000/02/08 16:26:02  lasiuk
 * rmove vector and StGlobals from Interface.
 * allocate space for survey parameters
 * calculate sector origins and pad corner positions
 *
 * Revision 1.2  2000/01/25 22:02:20  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/18 21:32:02  lasiuk
 * Initial Revision
 *
 *  Revision history:
 *    7/27/1999 First Approach, C & A
 *    8/8/1999 Secondary Revision, C & A
 *    8/10/1999 Static access method getDB() added,
 *                                with help of Valery Fine
 *****************************************************************/
#ifndef ST_RICH_GEOMETRY_H
#define ST_RICH_GEOMETRY_H

#include <vector>

#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

#include "StThreeVector.hh"

#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif
#include "StRichRrsMacros.h"
#include "StRichGeometryDbInterface.h"

class StRichGeometryDb : public StRichGeometryDbInterface {
public:
	
    static StRichGeometryDb* getDb();
	
    double mVersion;
    struct Quadrant {
	Quadrant() { }
	
	double x0;                // center of first pad in each quadrant
	double y0;                // same thing but verticaly
    };

#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<Quadrant> quads;       // the quadrants of the pad plane
    vector<Quadrant> quadsOrigin; // the quadrants of the pad plane    
#else
    vector<Quadrant, allocator<Quadrant> > quads;
    vector<Quadrant, allocator<Quadrant> > quadsOrigin;
#endif   
	
	
    double quad_gap_x;          // gap between quadrants in z
    double quad_gap_y;          // gap between quadrants in x
    
    double wire_spacing;        // distance between wires
    double wire_y0[2];             // first wire offset
    int    number_of_wires;     // total number of wires

    StThreeVector<double> mRadiatorDimension;
    StThreeVector<double> mQuartzDimension;
    StThreeVector<double> mPadPlaneDimension;
    double mProximityGap; 
    StThreeVector<double> mNormalVectorToPadPlane;
    
    double pad_spacing;         // gap between pads
    double row_spacing;
    double pad_side_x;          // width of pads
    double pad_pitch;
    double pad_side_y;          // length of pads
    double row_pitch;
    
    int n_pad_x, n_pad_y;       // number of pads in both directions (quadrant)
    int mNumberOfPadsInaColumn;
    int mNumberOfPadsInaRow;
    int number_of_pads;         // total number of pads   
    
    double length;              // detector length
    double width;               // detector width
    double height;              // vertical distance from pad to anode

    //
    // survey geometry
    double mInclinationAngle;
    double mRadialDistanceToRich;
    
    // Interface
    double version()              const;
    double quadrantX0(int)        const;
    double quadrantY0(int)        const;
    double quadrantXOrigin(int)   const;
    double quadrantYOrigin(int)   const;

    double quadrantGapInX()       const;
    double quadrantGapInY()       const;

    const StThreeVector<double>& radiatorDimension() const;
    const StThreeVector<double>& quartzDimension()   const;
    const StThreeVector<double>& padPlaneDimension() const;
    double proximityGap()                            const; 
    
    double firstWirePositionInY(int) const;
    double wirePitch()            const;
    int    numberOfWires()        const;
   
    double padLength()            const;
    double padWidth()             const;
    double padPitch()             const;
    double rowPitch()             const;
    double rowSpacing()           const;
    double padSpacing()           const;
    
    int    numberOfPadsInARow()            const; // X
    int    numberOfPadsInAQuadrantRow()    const; // X
    int    numberOfRowsInAColumn()         const; // Z
    int    numberOfRowsInAQuadrantColumn() const; // Z
    int    numberOfPads()                  const;
	
    double detectorLength()    const;
    double detectorWidth()     const;
    double anodeToPadSpacing() const;
	
    //
    // Survey Geometry
    double radialDistanceToRich()   const;
    double inclinationAngle()       const;
    const StThreeVector<double>& normalVectorToPadPlane() const;
    
    void   print(ostream& os = cout) const;
protected:
    StRichGeometryDb();
    
private:
    //void star_fill();           // fill from STAR DB
    void my_fill();             // fill with my own stuff
    
    static StRichGeometryDb* p2Db;   // handle to only instance
};

inline double StRichGeometryDb::version() const {return mVersion;}
inline double StRichGeometryDb::quadrantX0(int n) const {return quads[n].x0;}
inline double StRichGeometryDb::quadrantY0(int n) const {return quads[n].y0;}
inline double StRichGeometryDb::quadrantXOrigin(int n) const {return quadsOrigin[n].x0;}
inline double StRichGeometryDb::quadrantYOrigin(int n) const {return quadsOrigin[n].y0;}
inline double StRichGeometryDb::quadrantGapInX() const {return quad_gap_x;}
inline double StRichGeometryDb::quadrantGapInY() const { return quad_gap_y;}

inline const StThreeVector<double>& StRichGeometryDb::radiatorDimension() const { return mRadiatorDimension;}
inline const StThreeVector<double>& StRichGeometryDb::quartzDimension() const { return mQuartzDimension;}
inline const StThreeVector<double>& StRichGeometryDb::padPlaneDimension() const { return mPadPlaneDimension;}
inline double StRichGeometryDb::proximityGap() const { return mProximityGap;} 
inline const StThreeVector<double>& StRichGeometryDb::normalVectorToPadPlane() const { return mNormalVectorToPadPlane;}

inline double StRichGeometryDb::wirePitch() const { return wire_spacing;}
inline double StRichGeometryDb::firstWirePositionInY(int q) const { return (q>0) ? wire_y0[0] : wire_y0[1]; }
inline int    StRichGeometryDb::numberOfWires() const { return number_of_wires;}
    
inline double StRichGeometryDb::padPitch() const { return pad_pitch;}
inline double StRichGeometryDb::rowPitch() const { return row_pitch;}
inline double StRichGeometryDb::padSpacing() const { return pad_spacing;}
inline double StRichGeometryDb::rowSpacing() const { return row_spacing;}
inline double StRichGeometryDb::padLength() const { return pad_side_y;}
inline double StRichGeometryDb::padWidth() const { return pad_side_x;}

inline int    StRichGeometryDb::numberOfPadsInARow() const { return mNumberOfPadsInaRow;} // X
inline int    StRichGeometryDb::numberOfPadsInAQuadrantRow() const { return n_pad_x;} // X
inline int    StRichGeometryDb::numberOfRowsInAColumn() const { return mNumberOfPadsInaColumn;} // Y
inline int    StRichGeometryDb::numberOfRowsInAQuadrantColumn() const { return n_pad_y;} // Y
inline int    StRichGeometryDb::numberOfPads() const { return number_of_pads;}
	
inline double StRichGeometryDb::detectorLength() const { return length;}
inline double StRichGeometryDb::detectorWidth() const { return width;}
inline double StRichGeometryDb::anodeToPadSpacing() const { return height;}

inline double StRichGeometryDb::inclinationAngle() const { return mInclinationAngle;}
inline double StRichGeometryDb::radialDistanceToRich() const { return mRadialDistanceToRich;}

#ifndef ST_NO_NAMESPACES
//}
#endif  

#endif
