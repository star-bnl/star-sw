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
                     double& ey, double& ez, Medium*& m, int& status);

  void WeightingField(const double x, const double y, const double z,
                            double& wx, double& wy, double& wz,
                            const std::string& label); 

  Medium* GetMedium(const double x, const double y, const double z);

  bool GetVoltageRange(double& vmin, double& vmax);
  bool GetBoundingBox(double& xmin, double& ymin, double& zmin, double& xmax,
                      double& ymax, double& zmax);
  void SetRangeZ(const double zmin, const double zmax);

  // Import mesh and field map from files.
  bool Initialise(const std::string gridfilename,
                  const std::string datafilename);

  // List all currently defined regions.
  void PrintRegions();
  // Get the number of regions in the device.
  int GetNumberOfRegions() const { return nRegions; }
  void GetRegion(const int i, std::string& name, bool& active);
  void SetDriftRegion(const int ireg);
  void UnsetDriftRegion(const int ireg);
  // Set/get the medium for a given region.
  void SetMedium(const int ireg, Medium* m);
  Medium* GetMedium(const unsigned int& ireg) const;

  // Retrieve information about the mesh.
  int GetNumberOfElements() const { return nElements; }
  bool GetElement(const int i, double& vol, double& dmin, double& dmax,
                  int& type);
  bool GetElement(const int i, double& vol, double& dmin, double& dmax,
                  int& type, int& node1, int& node2, int& node3, int& node4,
                  int& reg);
  int GetNumberOfNodes() const { return nVertices; }
  bool GetNode(const int i, double& x, double& y, double& v, double& ex,
               double& ey);

  // Mobilities
  bool GetMobility(const double x, const double y, const double z, double& emob,
                   double& hmob);

  // Velocity field maps
  void ElectronVelocity(const double xin, const double yin,
                                    const double zin, double& vx, double& vy,
                                    double& vz, Medium*& m, int& status);
  void HoleVelocity(const double xin, const double yin,
                                    const double zin, double& vx, double& vy,
                                    double& vz, Medium*& m, int& status);
  // Lifetime field maps
  bool GetElectronLifetime(const double x, const double y, const double z, double& etau);
  bool GetHoleLifetime(const double x, const double y, const double z, double& htau);

  // Trapping 
  int GetNumberOfDonors(){return nDonor;}
  int GetNumberOfAcceptors(){return nAcceptor;}
  bool GetDonorOccupation(const double x, const double y, const double z, const int donorNumber,
		  double& occupationFraction);
  bool GetAcceptorOccupation(const double x, const double y, const double z, const int acceptorNumber,
                   double& occupationFraction);
  bool SetDonorXsec(const int donorNumber, const double eXsec, const double hXsec);
  bool SetAcceptorXsec(const int acceptorNumber, const double eXsec, const double hXsec);
  bool SetDonorConc(const int donorNumber, const double concentration);
  bool SetAcceptorConc(const int acceptorNumber, const double concentration);
  bool ElectronAttachment(const double x, const double y, const double z,
                          double& eta);
  bool HoleAttachment(const double x, const double y, const double z,
                          double& eta);

 private:
  // Max. number of vertices per element
  static const int nMaxVertices = 4;

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
    // Flag indicating if vertex belongs to more than one region
    bool isShared;
  };
  std::vector<vertex> vertices;

  // Elements
  int nElements;
  struct element {
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
    int nNeighbours;
    std::vector<int> neighbours;
  };
  std::vector<element> elements;

  // Available data.
  bool hasPotential;
  bool hasField;
  bool hasElectronMobility;
  bool hasHoleMobility;

  bool hasElectronVelocity;
  bool hasHoleVelocity; 
  bool hasElectronLifetime;
  bool hasHoleLifetime;

  // Number of available traps
  int nDonor;
  int nAcceptor;

  // List of cross-sections and trap concentrations
  std::vector<double> donorElectronXsec;
  std::vector<double> donorHoleXsec;
  std::vector<double> acceptorElectronXsec;
  std::vector<double> acceptorHoleXsec;
  std::vector<double> donorConc;
  std::vector<double> acceptorConc;

  // Are all the crossections and concentrations are valid and set
  bool validXsec;
  bool validConc;

  // Voltage range
  double pMin, pMax;

  // Bounding box
  bool hasRangeZ;
  double xMinBoundingBox, yMinBoundingBox, zMinBoundingBox;
  double xMaxBoundingBox, yMaxBoundingBox, zMaxBoundingBox;

  // Element from the previous call
  int lastElement;
  // Shape functions for interpolation
  // (local coordinates)
  double w[nMaxVertices];

  // Reset the component
  void Reset();
  // Periodicities
  void UpdatePeriodicity();

  bool CheckRectangle(const double x, const double y, const int i);
  bool CheckTriangle(const double x, const double y, const int i);
  bool CheckLine(const double x, const double y, const int i);

  bool LoadGrid(const std::string gridfilename);
  bool LoadData(const std::string datafilename);
  void FindNeighbours();
  void Cleanup();
};
}
#endif
