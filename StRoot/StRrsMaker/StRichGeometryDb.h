/***********************************************
 * $Id: StRichGeometryDb.h,v 1.1 2000/01/18 21:32:02 lasiuk Exp $
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
 * $Log: StRichGeometryDb.h,v $
 * Revision 1.1  2000/01/18 21:32:02  lasiuk
 * Initial Revision
 *
 *
 * Revision 1.2  2000/01/25 22:02:20  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/18 21:32:02  lasiuk
 * Initial Revision
 *
 *  Revision history:
 ***********************************************************/
 *    8/8/1999 Secondary Revision, C & A
 *    8/10/1999 Static access method getDB() added,
 *                                with help of Valery Fine
 *****************************************************************/
//#include <memory>
#ifndef ST_RICH_GEOMETRY_H
#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500

#include <vector>

    
    class StRichGeometryDb {
    public:
#include "StRichRrsMacros.h"
	static StRichGeometryDb* getDb();
public:
	double version;
	struct Quadrant {
	    Quadrant() { }
	    
	    double x0;                // center of first pad in each quadrant
	    double z0;                // same thing but verticaly
	};
    double mVersion;
	double z0;                // same thing but verticaly
	vector<Quadrant> quads;   // the quadrants of the pad plane
	
	vector<Quadrant, allocator<Quadrant> > quads;
    vector<Quadrant> quads;   // the quadrants of the pad plane

    double quadrantX0(int)        const;
	double quad_gap_z;          // gap between quadrants in z
	double quad_gap_x;          // gap between quadrants in x
    int    numberOfRowsInAColumn()         const; // Z
	double wire_spacing;        // distance between wires
	double wire_x0;             // first wire offset
	int    number_of_wires;     // total number of wires
	
	double pad_spacing;         // gap between pads
	double pad_side_x;          // length of pads
	double pad_side_z;          // width of pads
	int n_pad_x, n_pad_z;       // number of pads in both directions
	int number_of_pads;         // total number of pads   
	
	double length;              // detector length
	double width;               // detector width
	double height;              // vertical distance from pad to anode
inline double StRichGeometryDb::firstWirePositionInX() const { return wire_x0;}
	double ampl_factor;         // gas amplification factor
inline int    StRichGeometryDb::numberOfRowsInAColumn() const { return mNumberOfPadsInaColumn;} // Y
	
    protected:
	StRichGeometryDb();
	
    private:
	//void star_fill();           // fill from STAR DB
	void my_fill();             // fill with my own stuff
	
	static StRichGeometryDb* p2Db;   // handle to only instance
    };
    
inline int    StRichGeometryDb::numberOfPads() const { return number_of_pads;}
	
inline double StRichGeometryDb::gasGainAmplificationFactor() const { return ampl_factor;}

inline double StRichGeometryDb::inclinationAngle() const { return mInclinationAngle;}
inline double StRichGeometryDb::radialDistanceToRich() const { return mRadialDistanceToRich;}

#ifndef ST_NO_NAMESPACES
//}
#endif  

#endif
