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
  Medium* GetMedium(const double x, const double y, 
                    const double z) const;

  // Get the number of materials defined in the ROOT geometry
  int GetNumberOfMaterials();
  // Get pointer to ROOT material with given index/name
  TGeoMaterial* GetMaterial(const int i);
  TGeoMaterial* GetMaterial(const char* name);
  // Associate ROOT material with Garfield medium
  void SetMedium(const int imat, Medium* med);
  void SetMedium(const char* mat, Medium* med);

  bool IsInside(const double x, const double y, const double z) const {

    if (m_geoManager) {
      m_geoManager->SetCurrentPoint(x, y, z);
      return !m_geoManager->IsOutside();
    }
    return false;
  }

  // Bounding box (envelope of geometry)
  bool GetBoundingBox(double& xmin, double& ymin, double& zmin, double& xmax,
                      double& ymax, double& zmax);

  // Switch on/off debugging and warning messages
  void EnableDebugging() { m_debug = true; }
  void DisableDebugging() { m_debug = false; }

 protected:
  // ROOT geometry manager
  TGeoManager* m_geoManager;

  // List of ROOT materials associated to Garfield media
  int m_nMaterials;
  struct material {
    std::string name;
    Medium* medium;
  };
  std::vector<material> m_materials;

  // Switch on/off debugging messages
  bool m_debug;
};
}

#endif
