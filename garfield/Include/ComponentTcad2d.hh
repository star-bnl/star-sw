// Interpolation in a two-dimensional field map created by Sentaurus Device

#ifndef G_COMPONENT_TCAD_2D_H
#define G_COMPONENT_TCAD_2D_H

#include "ComponentBase.hh"

namespace Garfield {

class ComponentTcad2d : public ComponentBase {

 public:
  // Constructor
  ComponentTcad2d();
  // Destructor
  ~ComponentTcad2d() {}

  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, double& v, Medium*& m,
                     int& status);

  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, Medium*& m, int& status) {

    double v = 0.;
    ElectricField(x, y, z, ex, ey, ez, v, m, status);
  }

  void WeightingField(const double x, const double y, const double z,
                            double& wx, double& wy, double& wz,
                            const std::string& label); 

  Medium* GetMedium(const double x, const double y, const double z);

  bool GetVoltageRange(double& vmin, double& vmax);
  bool GetBoundingBox(double& xmin, double& ymin, double& zmin, double& xmax,
                      double& ymax, double& zmax);
  void SetRangeZ(const double zmin, const double zmax);

  // Import mesh and field map from files.
  bool Initialise(const std::string& gridfilename,
                  const std::string& datafilename);

  // List all currently defined regions.
  void PrintRegions() const;
  // Get the number of regions in the device.
  unsigned int GetNumberOfRegions() const { return m_regions.size(); }
  void GetRegion(const unsigned int i, std::string& name, bool& active) const;
  void SetDriftRegion(const unsigned int ireg);
  void UnsetDriftRegion(const unsigned int ireg);
  // Set/get the medium for a given region.
  void SetMedium(const unsigned int ireg, Medium* m);
  Medium* GetMedium(const unsigned int ireg) const;

  // Retrieve information about the mesh.
  unsigned int GetNumberOfElements() const { return m_elements.size(); }
  bool GetElement(const unsigned int i, 
                  double& vol, double& dmin, double& dmax,
                  int& type) const;
  bool GetElement(const unsigned int i, double& vol, double& dmin, double& dmax,
                  int& type, int& node1, int& node2, int& node3, int& node4,
                  int& reg) const;
  unsigned int GetNumberOfNodes() const { return m_vertices.size(); }
  bool GetNode(const unsigned int i, double& x, double& y, double& v, 
               double& ex, double& ey) const;

  // Mobilities
  bool GetMobility(const double x, const double y, const double z, double& emob,
                   double& hmob);

  // Velocity field maps
  void ElectronVelocity(const double x, const double y, const double z,
                        double& vx, double& vy, double& vz,
                        Medium*& m, int& status);
  void HoleVelocity(const double x, const double y, const double z,
                    double& vx, double& vy, double& vz,
                    Medium*& m, int& status);
  // Lifetime field maps
  bool GetElectronLifetime(const double x, const double y, const double z, 
                           double& etau);
  bool GetHoleLifetime(const double x, const double y, const double z, 
                       double& htau);

  // Trapping 
  int GetNumberOfDonors() { return m_donors.size(); }
  int GetNumberOfAcceptors() { return m_acceptors.size(); }

  bool GetDonorOccupation(const double x, const double y, const double z, 
                          const unsigned int donorNumber, 
                          double& occupationFraction);
  bool GetAcceptorOccupation(const double x, const double y, const double z, 
                             const unsigned int acceptorNumber,
                             double& occupationFraction);
  bool SetDonor(const unsigned int donorNumber, 
                const double eXsec, const double hxSec, 
                const double concentration);
  bool SetAcceptor(const unsigned int acceptorNumber, 
                   const double eXsec, const double hxSec, 
                   const double concentration);

  bool ElectronAttachment(const double x, const double y, const double z,
                          double& eta);
  bool HoleAttachment(const double x, const double y, const double z,
                      double& eta);

 private:
  // Max. number of vertices per element
  static const int nMaxVertices = 4;

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
    double x, y;
    // Potential [V] and electric field [V / cm]
    double p, ex, ey;
    // Mobilities [cm2 / (V ns)]
    double emob, hmob;
    // Velocities [cm/ns]
    double eVx, eVy;
    double hVx, hVy;
    // Lifetimes [1/ns]
    double eTau, hTau;
    // Trap occupations [dimensionless] 
    std::vector<float> donorOcc;
    std::vector<float> acceptorOcc;
  };
  std::vector<Vertex> m_vertices;

  // Elements
  struct Element {
    // Indices of vertices
    int vertex[nMaxVertices];
    // Type of element
    // 0: Point
    // 1: Segment (line)
    // 2: Triangle
    // 3: Rectangle
    // 4: Polygon
    // Types 1 - 3 are supported by this class.
    int type;
    // Associated region
    int region;
    std::vector<int> neighbours;
    // Bounding box
    double xmin, xmax;
    double ymin, ymax;
  };
  std::vector<Element> m_elements;

  struct Defect {
    // Electron cross-section
    double xsece;
    // Hole cross-section
    double xsech;
    // Concentration
    double conc;
  };
  std::vector<Defect> m_donors;
  std::vector<Defect> m_acceptors;
  
  // Available data.
  bool m_hasPotential;
  bool m_hasField;
  bool m_hasElectronMobility;
  bool m_hasHoleMobility;
  bool m_hasElectronVelocity;
  bool m_hasHoleVelocity; 
  bool m_hasElectronLifetime;
  bool m_hasHoleLifetime;

  // Are all the cross-sections and concentrations valid and set.
  bool m_validTraps;

  // Voltage range
  double m_pMin, m_pMax;

  // Bounding box
  bool m_hasRangeZ;
  double m_xMinBB, m_yMinBB, m_zMinBB;
  double m_xMaxBB, m_yMaxBB, m_zMaxBB;

  // Element from the previous call
  int m_lastElement;

  // Reset the component
  void Reset();
  // Periodicities
  void UpdatePeriodicity();

  // Check whether a point is inside a given element and calculate the  
  // shape functions if it is.
  bool CheckElement(const double x, const double y,
                    const Element& element, double w[nMaxVertices]) const;
  bool CheckRectangle(const double x, const double y, const Element& element,
                      double w[nMaxVertices]) const;
  bool CheckTriangle(const double x, const double y, const Element& element,
                     double w[nMaxVertices]) const;
  bool CheckLine(const double x, const double y, const Element& element,
                 double w[nMaxVertices]) const;

  bool LoadGrid(const std::string& gridfilename);
  bool LoadData(const std::string& datafilename);
  bool ReadDataset(std::ifstream& datafile, const std::string& dataset);
  void FindNeighbours();
  void Cleanup();

  int FindRegion(const std::string& name) const;

  void MapCoordinates(double& x, double& y, bool& xmirr, bool& ymirr) const;
  bool CheckTraps() const;
};
}
#endif
