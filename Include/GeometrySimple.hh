// Abstract base class for components

#ifndef G_GEOMETRY_SIMPLE_H
#define G_GEOMETRY_SIMPLE_H

#include <vector>

#include "GeometryBase.hh"
#include "Solid.hh"

namespace Garfield {

class GeometrySimple : public GeometryBase {

  public:
    // Constructor
    GeometrySimple();
    ~GeometrySimple() {}

    // Add a solid to the geometry
    void AddSolid(Solid* s, Medium* m);
    // Get the solid at a given location (x, y, z)
    bool GetSolid(const double x, const double y, const double z, Solid*& s);
    // Get the medium at a given location (x, y, z)
    bool GetMedium(const double x, const double y, const double z, Medium*& m);
    // Number of solids/media in the geometry
    int GetNumberOfSolids() const {return nSolids;}
    int GetNumberOfMedia() const  {return nMedia; }
    // Get a solid/medium from the list
    bool GetSolid(const int i, Solid*& s) const;
    virtual
    bool GetMedium(const int i, Medium*& m) const;
    // Reset the geometry
    void Clear();

    bool IsInside(const double x, const double y, const double z);
    // Bounding box (envelope of geometry)
    bool IsInBoundingBox(const double x, const double y, const double z);
    bool GetBoundingBox(double& xmin, double& ymin, double& zmin,
                        double& xmax, double& ymax, double& zmax) {
      xmin = xMinBoundingBox; ymin = yMinBoundingBox; zmin = zMinBoundingBox; 
      xmax = xMaxBoundingBox; ymax = yMaxBoundingBox; zmax = zMaxBoundingBox;
      return true;
    }

    // Switch on/off debugging and warning messages
    void EnableDebugging()  {debug = true;}
    void DisableDebugging() {debug = false;}

  protected:
    
    // List of media
    int nMedia;
    struct medium {
      Medium* medium;
    };
    std::vector<medium> media;
    
    // List of solids
    int nSolids;
    struct solid {
      Solid* solid;
      int medium;
    };
    std::vector<solid> solids;

    // Bounding box ranges
    bool hasBoundingBox;
    double xMinBoundingBox, yMinBoundingBox, zMinBoundingBox;
    double xMaxBoundingBox, yMaxBoundingBox, zMaxBoundingBox;

    // Switch on/off debugging messages
    bool debug; 
    
};
  
}

#endif
