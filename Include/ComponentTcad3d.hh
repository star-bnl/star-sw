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
  bool Initialise(const std::string& gridfilename,
                  const std::string& datafilename);

  // List all currently defined regions.
  void PrintRegions();
  // Get the number of regions in the device.
  unsigned int GetNumberOfRegions() const { return m_regions.size(); }
  void GetRegion(const unsigned int ireg, std::string& name, 
                 bool& active) const;
  void SetDriftRegion(const unsigned int ireg);
  void UnsetDriftRegion(const unsigned int ireg);
  // Set/get the medium for a given region
  void SetMedium(const unsigned int ireg, Medium* m);
  bool GetMedium(const unsigned int ireg, Medium*& m) const;

  int GetNumberOfElements() const { return m_elements.size(); }
  bool GetElement(const unsigned int i, double& vol, double& dmin, double& dmax,
                  int& type) const;
  bool GetElement(const unsigned int i, double& vol, double& dmin, double& dmax,
                  int& type, int& node1, int& node2, int& node3, int& node4,
                  int& node5, int& node6, int& node7, int& reg) const;
  unsigned int GetNumberOfNodes() const { return m_vertices.size(); }
  bool GetNode(const unsigned int i, double& x, double& y, double& z, double& v,
               double& ex, double& ey, double& ez) const;

 private:
  // Max. number of vertices per element
  static const int nMaxVertices = 7;

  // Regions
  struct Region {
    // Name of region (from Tcad)
    std::string name;
    // Flag indicating if the region is active (i. e. a drift medium)
    bool drift;
    Medium* medium;
  };
  std::vector<Region> m_regions;

  // Vertices
  struct Vertex {
    // Coordinates [cm]
    double x, y, z;
    // Potential [V] and electric field [V / cm]
    double p, ex, ey, ez;
  };
  std::vector<Vertex> m_vertices;

  // Elements
  struct Element {
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
    std::vector<int> neighbours;
    // Bounding box
    double xmin, xmax;
    double ymin, ymax;
    double zmin, zmax; 
  };
  std::vector<Element> m_elements;

  // Face
  struct Face {
    // Indices of edges
    int edge[4];
    int type;
  };

  // Voltage range
  double m_pMin, m_pMax;

  // Bounding box
  double m_xMinBB, m_yMinBB, m_zMinBB;
  double m_xMaxBB, m_yMaxBB, m_zMaxBB;

  // Element from the previous call
  int m_lastElement;

  // Reset the component
  void Reset();
  // Periodicities
  void UpdatePeriodicity();

  bool CheckElement(const double x, const double y, const double z,
                    const Element& element, double w[nMaxVertices]) const {

    bool inside = false;
    switch (element.type) {
      case 2:
        if (CheckTriangle(x, y, z, element, w)) inside = true;
        break;
      case 5:
        if (CheckTetrahedron(x, y, z, element, w)) inside = true;
        break;
      default:
        std::cerr << m_className << "::CheckElement:\n"
                  << "    Invalid element type (" << element.type << ").\n";
        break;
    }
    return inside;
  }
  bool CheckTetrahedron(const double x, const double y, const double z,
                        const Element& element, double w[nMaxVertices]) const;
  bool CheckTriangle(const double x, const double y, const double z,
                     const Element& element, double w[nMaxVertices]) const;

  void FindNeighbours();
  bool LoadGrid(const std::string& gridfilename);
  bool LoadData(const std::string& datafilename);
  bool ReadDataset(std::ifstream& datafile, const std::string& dataset);
  void Cleanup();

  void MapCoordinates(double& x, double& y, double& z,
                      bool& xmirr, bool& ymirr, bool& zmirr) const;
  int FindRegion(const std::string& name) const;
};
}
#endif
