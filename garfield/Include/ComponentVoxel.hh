// Interpolation in a regular mesh

#ifndef G_COMPONENT_VOXEL_H
#define G_COMPONENT_VOXEL_H

#ifndef __CINT__
#include <map>
#endif
#include "ComponentBase.hh"

namespace Garfield {

class ComponentVoxel : public ComponentBase {

 public:
  // Constructor
  ComponentVoxel();
  // Destructor
  ~ComponentVoxel() {}

  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, double& v, Medium*& m,
                     int& status);
  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, Medium*& m, int& status);

  void WeightingField(const double x, const double y, const double z,
                            double& wx, double& wy, double& wz,
                            const std::string& label); 
  
  Medium* GetMedium(const double& x, const double& y, const double& z);

  bool GetVoltageRange(double& vmin, double& vmax);
  bool GetElectricFieldRange(double& exmin, double& exmax, double& eymin,
                             double& eymax, double& ezmin, double& ezmax);
  bool GetBoundingBox(double& xmin, double& ymin, double& zmin, double& xmax,
                      double& ymax, double& zmax);
  // Setup the grid.
  void SetMesh(const unsigned int nx, const unsigned int ny,
               const unsigned int nz, const double xmin, const double xmax,
               const double ymin, const double ymax, const double zmin,
               const double zmax);
  // Import electric field and potential values from a file.
  // The file is supposed to contain one line for each mesh point starting with
  //   - either two or three floating point numbers,
  //     specifying the coordinates (in cm) of the element centre or
  //   - two or three integers specifying the index of the element in the mesh,
  // followed by
  //   - two or three floating point numbers for the electric field (in V/cm),
  // and (depending on the values of withPotential and withRegion),
  //   - a floating point number specifying the potential (in V), and
  //   - an integer specifying the "region" of the element.
  // Format types are:
  // "xy", "xyz": elements are specified by the coordinates of their centres
  // "ij", "ijk": elements are specified by their indices
  bool LoadData(const std::string filename, std::string format,
                const bool withPotential, const bool withRegion,
                const double scaleX = 1., const double scaleE = 1.,
                const double scaleP = 1.);

  bool GetElement(const double xi, const double yi, const double zi,
                  unsigned int& i, unsigned int& j, unsigned int& k,
                  bool& xMirrored, bool& yMirrored, bool& zMirrored);
  bool GetElement(const unsigned int i, const unsigned int j,
                  const unsigned int k, double& v, double& ex, double& ey,
                  double& ez);

  void SetMedium(const int i, Medium* m);
  Medium* GetMedium(const unsigned int& i);
  void PrintRegions();

 private:
#ifndef __CINT__
  std::map<int, Medium*> m_media;
#endif
  struct element {
    // Electric field
    double ex, ey, ez;
    // Potential
    double v;
    // Index of region
    int region;
  };
  std::vector<std::vector<std::vector<element> > > m_mesh;
  // Dimensions of the mesh
  unsigned int m_nX, m_nY, m_nZ;
  double m_xMin, m_yMin, m_zMin;
  double m_xMax, m_yMax, m_zMax;

  bool m_hasMesh;
  bool m_hasPotential;
  bool m_hasField;

  // Voltage range
  double m_pMin, m_pMax;

  // Reset the component
  void Reset();
  // Periodicities
  void UpdatePeriodicity();
};
}
#endif
