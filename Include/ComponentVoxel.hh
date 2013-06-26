// Interpolation in a regular mesh 

#ifndef G_COMPONENT_VOXEL_H
#define G_COMPONENT_VOXEL_H

#include <string>
#include <vector>
#include <map>

#include "ComponentBase.hh"

namespace Garfield {

class ComponentVoxel : public ComponentBase {

  public:
    // Constructor
    ComponentVoxel();
    // Destructor
    ~ComponentVoxel() {}
    
    void ElectricField(const double x, const double y, const double z,
                       double& ex, double& ey, double& ez, double& v,
                       Medium*& m, int& status);
    void ElectricField(const double x, const double y, const double z,
                       double& ex, double& ey, double& ez,
                       Medium*& m, int& status); 
    
    bool GetMedium(const double x, const double y, const double z,
                   Medium*& medium);

    bool GetVoltageRange(double& vmin, double& vmax);
    bool GetElectricFieldRange(double& exmin, double& exmax,
                               double& eymin, double& eymax,
                               double& ezmin, double& ezmax);
    bool GetBoundingBox(double& xmin, double& ymin, double& zmin,
                        double& xmax, double& ymax, double& zmax); 
    // Setup the grid.
    void SetMesh(const unsigned int nx, 
                 const unsigned int ny,
                 const unsigned int nz,
                 const double xmin, const double xmax,
                 const double ymin, const double ymax,
                 const double zmin, const double zmax);
    // Import electric field and potential values from a file.
    // The file is supposed to contain one line for each mesh point starting with 
    //   - either three floating point numbers,
    //     specifying the coordinates (in cm) of the element centre or
    //   - three integers specifying the index of the element in the mesh,
    // followed by 
    //   - three floating point numbers for the electric field (in V/cm),
    // and (depending on the values of withPotential and withRegion),
    //   - a floating point number specifying the potential (in V), and
    //   - an integer specifying the "region" of the element.
    // Format types are:
    // "xyz": elements is specified by the coordinates of their centres
    // "ijk": elements are specified by its index 
    bool LoadData(const std::string filename, std::string format,
                  const bool withPotential, const bool withRegion,
                  const double scaleX = 1., const double scaleE = 1.,
                  const double scaleP = 1.);

    bool GetElement(const double xi, const double yi, const double zi,
                    unsigned int& i, unsigned int& j, unsigned int& k,
                    bool& xMirrored, bool& yMirrored, bool& zMirrored);
    bool GetElement(const unsigned int i, const unsigned int j, 
                    const unsigned int k,
                    double& v, double& ex, double& ey, double& ez);

    void SetMedium(const int i, Medium* m);
    bool GetMedium(const int i, Medium*& m);
    void PrintRegions();

  private:

    std::map<int, Medium*> media;

    struct element {
      // Electric field
      double ex, ey, ez;
      // Potential
      double v;
      // Index of region
      int region;
    };
    std::vector<std::vector<std::vector<element> > > mesh;
    // Dimensions of the mesh
    unsigned int nX, nY, nZ;
    double xMin, yMin, zMin;
    double xMax, yMax, zMax;

    bool hasMesh;
    bool hasPotential;
    bool hasField;

    // Voltage range
    double pMin, pMax;

    // Reset the component
    void Reset();
    // Periodicities
    void UpdatePeriodicity(); 

};

}
#endif
