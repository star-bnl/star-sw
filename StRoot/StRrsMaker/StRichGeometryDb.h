/*****************************************************************
 * $Id: StRichGeometryDb.h,v 1.3 2000/02/08 16:26:02 lasiuk Exp $
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
 * Revision 1.3  2000/02/08 16:26:02  lasiuk
 * rmove vector and StGlobals from Interface.
 * allocate space for survey parameters
 * calculate sector origins and pad corner positions
 *
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
	double z0;                // same thing but verticaly
	Quadrant() { }
	
	double x0;                // center of first pad in each quadrant
	double y0;                // same thing but verticaly
    };

#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<Quadrant> quads;       // the quadrants of the pad plane
    vector<Quadrant> quadsOrigin; // the quadrants of the pad plane    
#else
    vector<Quadrant, allocator<Quadrant> > quads;
    double quad_gap_z;          // gap between quadrants in z
    double quad_gap_x;          // gap between quadrants in x
	
	
    double wire_x0;             // first wire offset
    double quad_gap_y;          // gap between quadrants in x
    StThreeVector<double> mQuartzDimension;
    StThreeVector<double> mPadPlaneDimension;
    double mProximityGap; 
    double pad_side_x;          // length of pads
    
    double pad_side_z;          // width of pads
    double row_spacing;
    double pad_side_x;          // width of pads
    int n_pad_x, n_pad_z;       // number of pads in both directions (quadrant)
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
    double quadrantZ0(int)        const;
    double mRadialDistanceToRich;
    double quadrantZOrigin(int)   const;
    // Interface
    double quadrantX0(int)        const;
    double quadrantGapInZ()       const;
    double quadrantY0(int)        const;
	
    const StThreeVector<double>& padPlaneDimension() const;
    double firstWirePositionInX() const;
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
    
inline double StRichGeometryDb::quadrantZ0(int n) const {return quads[n].z0;}
    static StRichGeometryDb* p2Db;   // handle to only instance
inline double StRichGeometryDb::quadrantZOrigin(int n) const {return quadsOrigin[n].z0;}

inline double StRichGeometryDb::quadrantGapInZ() const {return quad_gap_z;}
inline double StRichGeometryDb::quadrantGapInX() const { return quad_gap_x;}
inline const StThreeVector<double>& StRichGeometryDb::quartzDimension() const { return mQuartzDimension;}
inline const StThreeVector<double>& StRichGeometryDb::padPlaneDimension() const { return mPadPlaneDimension;}
inline double StRichGeometryDb::firstWirePositionInX() const { return wire_x0;}
inline const StThreeVector<double>& StRichGeometryDb::normalVectorToPadPlane() const { return mNormalVectorToPadPlane;}

	
inline double StRichGeometryDb::wirePitch() const { return wire_spacing;}
inline double StRichGeometryDb::firstWirePositionInY(int q) const { return (q>0) ? wire_y0[0] : wire_y0[1]; }
inline int    StRichGeometryDb::numberOfWires() const { return number_of_wires;}
    
inline double StRichGeometryDb::padLength() const { return pad_side_x;}
inline double StRichGeometryDb::padWidth() const { return pad_side_z;}
inline double StRichGeometryDb::padSpacing() const { return pad_spacing;}
inline double StRichGeometryDb::rowSpacing() const { return row_spacing;}
inline int    StRichGeometryDb::numberOfPadsInAQuadrantRow() const { return n_pad_z;} // X
inline int    StRichGeometryDb::numberOfRowsInAColumn() const { return mNumberOfPadsInaColumn;} // Z
inline int    StRichGeometryDb::numberOfRowsInAQuadrantColumn() const { return n_pad_x;} // Z
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
