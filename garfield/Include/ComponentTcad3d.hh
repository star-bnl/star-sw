// Interpolation in a three-dimensional field map created by Sentaurus Device

#ifndef G_COMPONENT_TCAD_3D_H
#define G_COMPONENT_TCAD_3D_H

#include "ComponentBase.hh"

namespace Garfield {

class ComponentTcad3d : public ComponentBase {

 public:
  // Constructor
  ComponentTcad3d();
  // Destructor
  ~ComponentTcad3d() {}

  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, double& v, Medium*& m,
                     int& status);
  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, Medium*& m, int& status);

  Medium* GetMedium(const double x, const double y, const double z);

  bool GetVoltageRange(double& vmin, double& vmax);
  bool GetBoundingBox(double& xmin, double& ymin, double& zmin, double& xmax,
                      double& ymax, double& zmax);

  // Import mesh and field map from files.
  bool Initialise(const std::string gridfilename,
                  const std::string datafilename);

  // List all currently defined regions.
  void PrintRegions();
  // Get the number of regions in the device.
  int GetNumberOfRegions() const { return m_nRegions; }
  void GetRegion(const int ireg, std::string& name, bool& active);
  void SetDriftRegion(const int ireg);
  void UnsetDriftRegion(const int ireg);
  // Set/get the medium for a given region
  void SetMedium(const int ireg, Medium* m);
  bool GetMedium(const int ireg, Medium*& m) const;

  int GetNumberOfElements() const { return m_nElements; }
  bool GetElement(const int i, double& vol, double& dmin, double& dmax,
                  int& type);
  bool GetElement(const int i, double& vol, double& dmin, double& dmax,
                  int& type, int& node1, int& node2, int& node3, int& node4,
                  int& node5, int& node6, int& node7, int& reg);
  int GetNumberOfNodes() const { return m_nVertices; }
  bool GetNode(const int i, double& x, double& y, double& z, double& v,
               double& ex, double& ey, double& ez);

 private:
  // Max. number of vertices per element
  static const int nMaxVertices = 7;

  // Regions
  int m_nRegions;
  struct region {
    // Name of region (from Tcad)
    std::string name;
    // Flag indicating if the region is active (i. e. a drift medium)
    bool drift;
    Medium* medium;
  };
  std::vector<region> m_regions;

  // Vertices
  int m_nVertices;
  struct vertex {
    // Coordinates [cm]
    double x, y, z;
    // Potential [V] and electric field [V / cm]
    double p, ex, ey, ez;
    // Flag indicating if vertex belongs to more than one region
    bool isShared;
  };
  std::vector<vertex> m_vertices;

  // Elements
  int m_nElements;
  struct element {
    // Indices of vertices
    int vertex[nMaxVertices];
    // Type of element
    // 1: Segment (line)
    // 2: Triangle
    // 3: Rectangle
    // 4: Polygon
    // 5: Tetrahedron
    // 6: Pyramid
    // 7: Prism
    // 8: Brick
    // 9: Tetrabrick
    // 10: Polyhedron
    // Only types 2 and 5 are supported by this class.
    int type;
    // Associated region
    int region;
  };
  std::vector<element> m_elements;

  // Face
  struct face {
    // Indices of edges
    int edge[4];
    int type;
  };

  // Voltage range
  double m_pMin, m_pMax;

  // Bounding box
  double m_xMinBoundingBox, m_yMinBoundingBox, m_zMinBoundingBox;
  double m_xMaxBoundingBox, m_yMaxBoundingBox, m_zMaxBoundingBox;

  // Element from the previous call
  int m_lastElement;
  // Node point weighting factors for interpolation
  // (local coordinates)
  double m_w[nMaxVertices];

  // Reset the component
  void Reset();
  // Periodicities
  void UpdatePeriodicity();

  bool CheckTetrahedron(const double x, const double y, const double z,
                        const int i);
  bool CheckTriangle(const double x, const double y, const double z,
                     const int i);

  bool LoadGrid(const std::string gridfilename);
  bool LoadData(const std::string datafilename);
  void Cleanup();
};
}
#endif
