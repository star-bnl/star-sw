// Abstract base class for components

#ifndef G_GEOMETRY_ROOT_H
#define G_GEOMETRY_ROOT_H

#include <vector>

#include <TGeoManager.h>
#include <TGeoMaterial.h>

#include "GeometryBase.hh"

namespace Garfield {

class GeometryRoot : public GeometryBase {

  public:
    // Constructors
    GeometryRoot();
    // Destructor
    ~GeometryRoot() {}

    // Set the geometry (pointer to ROOT TGeoManager)
    void SetGeometry(TGeoManager* geoman);

    // Get the medium at a given point (x, y, z)
    bool GetMedium(const double x, const double y, const double z, Medium*& m);

    // Get the number of materials defined in the ROOT geometry
    int GetNumberOfMaterials();
    // Get pointer to ROOT material with given index/name
    TGeoMaterial* GetMaterial(const int i);
    TGeoMaterial* GetMaterial(const char* name);
    // Associate ROOT material with Garfield medium
    void SetMedium(const int imat, Medium* med);
    void SetMedium(const char* mat, Medium* med);

    bool IsInside(const double x, const double y, const double z) {

      if (theGeoManager != 0) {
        theGeoManager->SetCurrentPoint(x, y, z);
        return !theGeoManager->IsOutside();
      }
      return false;

    }

    // Bounding box (envelope of geometry)
    bool GetBoundingBox(double& xmin, double& ymin, double& zmin,
                        double& xmax, double& ymax, double& zmax); 

    // Switch on/off debugging and warning messages
    void EnableDebugging()  {debug = true;}
    void DisableDebugging() {debug = false;}

  protected:

    // ROOT geometry manager
    TGeoManager* theGeoManager;
        
    // List of ROOT materials associated to Garfield media
    int nMaterials;
    struct material {
      std::string name;
      Medium* medium;
    };
    std::vector<material> materials;
    
    // Switch on/off debugging messages
    bool debug; 
    
};
  
}

#endif
