// Interpolation in a two-dimensional field map created by Sentaurus Device

#ifndef G_COMPONENT_TCAD_2D_H
#define G_COMPONENT_TCAD_2D_H

#include <string>
#include <vector>

#include "ComponentBase.hh"

namespace Garfield {

class ComponentTcad2d : public ComponentBase {

  public:
    // Constructor
    ComponentTcad2d();
    // Destructor
    ~ComponentTcad2d() {}
    
    void ElectricField(const double x, const double y, const double z,
                       double& ex, double& ey, double& ez, double& v,
                       Medium* m, int& status);
    void ElectricField(const double x, const double y, const double z,
                       double& ex, double& ey, double& ez,
                       Medium* m, int& status);    
    
    bool GetMedium(const double x, const double y, const double z,
                   Medium*& medium);

    bool GetVoltageRange(double& vmin, double& vmax);
    bool GetBoundingBox(double& xmin, double& ymin, double& zmin,
                        double& xmax, double& ymax, double& zmax); 
                        
    // Import mesh and field map from files
    bool Initialise(const std::string gridfilename, const std::string datafilename);

    int  GetNumberOfRegions() const {return nRegions;}
    void GetRegion(const int i, std::string& name, bool& active);
    void SetDriftRegion(const int i, const bool active);
    // Set/get the medium for a given region
    void SetMedium(const int i, Medium* m);
    bool GetMedium(const int i, Medium*& m) const;

  private:
  
    // Regions
    int nRegions;
    struct region {
      // Name of region (from Tcad)
      std::string name;
      // Flag indicating if the region is active (i. e. a drift medium)
      bool drift;
      Medium* medium;
    };
    std::vector<region> regions;

    // Vertices
    int nVertices;
    struct vertex {
      // Coordinates [cm]
      double x, y;
      // Potential [V] and electric field [V / cm]
      double p, ex, ey;
      // Flag indicating if vertex belongs to more than one region
      bool   isShared;
    };
    std::vector<vertex> vertices;

    // Elements
    int nElements;
    struct element {
      // Indices of vertices
      int vertex[4];
      // Type of element (3: rectangle / 2: triangle / 1: line)
      int type;
      // Associated region
      int region; 
    };
    std::vector<element> elements;

    // Voltage range
    double pMin, pMax;

    // Bounding box
    bool hasBoundingBox;
    double xMinBoundingBox, yMinBoundingBox, zMinBoundingBox;
    double xMaxBoundingBox, yMaxBoundingBox, zMaxBoundingBox;

    // Element from the previous call
    int lastElement;
    // Weighting factors for node points used for interpolation
    double a, b, c, d;
    
    // Reset the component
    void Reset() {Cleanup();}
    // Periodicities
    void UpdatePeriodicity();    

    bool CheckRectangle(const double x, const double y, const int i);
    bool CheckTriangle(const double x, const double y, const int i);
    bool CheckLine(const double x, const double y, const int i);

    bool LoadGrid(const std::string gridfilename);
    bool LoadData(const std::string datafilename);
    void Cleanup();

};

}
#endif
