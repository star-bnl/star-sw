//StiDetPolygon.h
//M.L. Miller (Yale Software)
//06/01

/*
  StiDetPolygon is derived from StiPolygon and is meant to represent an easily navigable geometrical
  representation of a generic "cylindrical" collider detector.  That is, it preserves the genreal cylindrical
  symmetry of the detector while allowing for detector planes assembled in polygons.  Navigation is meant
  to preceed by stepping from one polygon to the other.

  Each polygon has n-sides, and each side will hold a pointer to a detector object.  Therefore, navigation
  can proceed without any information about the detector itself

  BEWARE!!! StiDetPolygon is derived from a concrete base class (generally this is a poor design), so don't push it too far!

*/

#ifndef StiDetPolygon_HH
#define StiDetPolygon_HH

//Temp
//#include "TObject.h"

#include <map>
#include "StiPolygon.h"

class StiDetector;

class StiDetPolygon : public StiPolygon
{
public:

    typedef multimap<unsigned int, StiDetector*> det_polygon_map;
    typedef det_polygon_map::value_type det_polygon_map_ValType;
    
    StiDetPolygon();
    StiDetPolygon(unsigned int nsides, double phi0, double r);
    virtual ~StiDetPolygon();

    //Access

    //Number of detectors
    unsigned int numberOfDetectors() const;

    //Does each side have a detector associated with it?
    bool isValid() const;
    
    //Add a detector to the polygon
    void push_back(StiDetector*);

    void clear();
    void clearAndDestroy();

    //return pointer to detector for a given side
    StiDetector* detector(unsigned int side);
    //return pointer to detector closest to this angle
    StiDetector* detector(double angle);

    //Utility
    unsigned int side(double angle, bool debug=false) const;

    void print() const; //stream msidemap
    
protected:
    det_polygon_map msidemap; //!
    
private:
    //ClassDef(StiDetPolygon, 1)
};

#endif
