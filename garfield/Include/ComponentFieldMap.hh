#ifndef G_COMPONENT_FIELD_MAP_H
#define G_COMPONENT_FIELD_MAP_H

#include "ComponentBase.hh"
#include "TMatrixD.h"
#include "TetrahedralTree.hh"

namespace Garfield {

class ComponentFieldMap : public ComponentBase {

 public:
  // Constructor
  ComponentFieldMap();
  // Destructor
  virtual ~ComponentFieldMap();

  // Ranges
  // Calculates x, y, z, V and angular ranges
  virtual void SetRange();
  // Shows x, y, z, V and angular ranges
  void PrintRange();
  // Returns the current sensor size
  virtual bool IsInBoundingBox(const double x, const double y, const double z);
  virtual bool GetBoundingBox(double& xmin, double& ymin, double& zmin,
                              double& xmax, double& ymax, double& zmax);

  bool GetVoltageRange(double& vmin, double& vmax) {
    vmin = mapvmin;
    vmax = mapvmax;
    return true;
  }

  // Materials
  // List all currently defined materials
  void PrintMaterials();
  // Make into a drift medium
  void DriftMedium(const unsigned int imat);
  // Make into a non-drift medium
  void NotDriftMedium(const unsigned int imat);
  // Number of materials
  unsigned int GetNumberOfMaterials() { return m_nMaterials; }
  // Return permittivity
  double GetPermittivity(const unsigned int imat);
  // Return conductivity
  double GetConductivity(const unsigned int imat);
  // Associate a material with a Medium class
  void SetMedium(const unsigned int imat, Medium* medium);
  // Returns the medium for a material
  Medium* GetMedium(const unsigned int i) const;
  Medium* GetMedium(const double x, const double y, const double z) = 0;
  unsigned int GetNumberOfMedia() { return m_nMaterials; }

  int GetNumberOfElements() const { return nElements; }
  bool GetElement(const int i, double& vol, double& dmin, double& dmax);

  virtual void ElectricField(const double x, const double y, const double z,
                             double& ex, double& ey, double& ez, Medium*& m,
                             int& status) = 0;
  virtual void ElectricField(const double x, const double y, const double z,
                             double& ex, double& ey, double& ez, double& v,
                             Medium*& m, int& status) = 0;

  virtual void WeightingField(const double x, const double y, const double z,
                              double& wx, double& wy, double& wz,
                              const std::string& label) = 0;

  virtual double WeightingPotential(const double x, const double y,
                                    const double z,
                                    const std::string& label) = 0;

  // Options
  void EnableCheckMapIndices() {
    checkMultipleElement = true;
    lastElement = -1;
  }
  void DisableCheckMapIndices() { checkMultipleElement = false; }
  void EnableDeleteBackgroundElements() { deleteBackground = true; }
  void DisableDeleteBackgroundElements() { deleteBackground = false; }

  // Enable/disable the usage of tetrahedral tree for searching the element in mesh
  void EnableTetrahedralTreeForElementSearch() { useTetrahedralTreeForSearch = true; }
  void DisableTetrahedralTreeForElementSearch() { useTetrahedralTreeForSearch = false; }

  friend class ViewFEMesh;

 protected:
  bool is3d;

  // Elements
  int nElements;
  struct element {
    // Nodes
    int emap[10];
    // Material
    unsigned int matmap;
    bool degenerate;
    // Bounding box of the element
    double xmin, ymin, zmin, xmax, ymax, zmax;
  };
  std::vector<element> elements;
  int lastElement;
  // Flag to check if bounding boxes of elements are cached 
  bool cacheElemBoundingBoxes;

  // Nodes
  int nNodes;
  struct node {
    // Coordinates
    double x, y, z;
    // Potential
    double v;
    // Weighting potentials
    std::vector<double> w;
  };
  std::vector<node> nodes;

  // Materials
  unsigned int m_nMaterials;
  struct material {
    // Permittivity
    double eps;
    // Resistivity
    double ohm;
    bool driftmedium;
    // Associated medium
    Medium* medium;
  };
  std::vector<material> materials;

  int nWeightingFields;
  std::vector<std::string> wfields;
  std::vector<bool> wfieldsOk;

  // Bounding box
  bool hasBoundingBox;
  double xMinBoundingBox, yMinBoundingBox, zMinBoundingBox;
  double xMaxBoundingBox, yMaxBoundingBox, zMaxBoundingBox;

  // Ranges and periodicities
  double mapxmin, mapymin, mapzmin;
  double mapxmax, mapymax, mapzmax;
  double mapxamin, mapyamin, mapzamin;
  double mapxamax, mapyamax, mapzamax;
  double mapvmin, mapvmax;

  bool setangx, setangy, setangz;
  double mapsx, mapsy, mapsz;

  double cellsx, cellsy, cellsz;
  double mapnxa, mapnya, mapnza;

  // Options
  // Delete meshing in conductors
  bool deleteBackground;
  // Scan for multiple elements that contain a point
  bool checkMultipleElement;

  // Warnings flag
  bool warning;

  // Tetrahedral tree
  TetrahedralTree* tetTree;
  bool useTetrahedralTreeForSearch;
  bool isTreeInitialized;

  // Reset the component
  void Reset() {};

  // Periodicities
  virtual void UpdatePeriodicity() = 0;
  void UpdatePeriodicity2d();
  void UpdatePeriodicityCommon();

  // Local coordinates
  // Calculate coordinates for curved quadratic triangles
  int Coordinates3(double x, double y, double z, double& t1, double& t2,
                   double& t3, double& t4, double jac[4][4], double& det,
                   int imap);
  // Calculate coordinates for linear quadrilaterals
  int Coordinates4(double x, double y, double z, double& t1, double& t2,
                   double& t3, double& t4, double jac[4][4], double& det,
                   int imap);
  // Calculate coordinates for curved quadratic quadrilaterals
  int Coordinates5(double x, double y, double z, double& t1, double& t2,
                   double& t3, double& t4, double jac[4][4], double& det,
                   int imap);
  // Calculate coordinates in linear tetrahedra
  int Coordinates12(double x, double y, double z, double& t1, double& t2,
                    double& t3, double& t4, int imap);
  // Calculate coordinates for curved quadratic tetrahedra
  int Coordinates13(double x, double y, double z, double& t1, double& t2,
                    double& t3, double& t4, double jac[4][4], double& det,
                    int imap);
  // Calculate coordinates for a cube
  int CoordinatesCube(double x, double y, double z, double& t1, double& t2,
                      double& t3, TMatrixD*& jac, std::vector<TMatrixD*>& dN,
                      int imap);

  // Calculate Jacobian for curved quadratic triangles
  void Jacobian3(int i, double u, double v, double w, double& det,
                 double jac[4][4]);
  // Calculate Jacobian for curved quadratic quadrilaterals
  void Jacobian5(int i, double u, double v, double& det, double jac[4][4]);
  // Calculate Jacobian for curved quadratic tetrahedra
  void Jacobian13(int i, double t, double u, double v, double w, double& det,
                  double jac[4][4]);
  // Calculate Jacobian for a cube
  void JacobianCube(int i, double t1, double t2, double t3, TMatrixD*& jac,
                    std::vector<TMatrixD*>& dN);

  // Find the element for a point in curved quadratic quadrilaterals
  int FindElement5(const double x, const double y, const double z, double& t1,
                   double& t2, double& t3, double& t4, double jac[4][4],
                   double& det);
  // Find the element for a point in curved quadratic tetrahedra
  int FindElement13(const double x, const double y, const double z, double& t1,
                    double& t2, double& t3, double& t4, double jac[4][4],
                    double& det);
  // Find the element for a point in a cube
  int FindElementCube(const double x, const double y, const double z,
                      double& t1, double& t2, double& t3, TMatrixD*& jac,
                      std::vector<TMatrixD*>& dN);

  // Move (xpos, ypos, zpos) to field map coordinates
  void MapCoordinates(double& xpos, double& ypos, double& zpos, bool& xmirrored,
                      bool& ymirrored, bool& zmirrored, double& rcoordinate,
                      double& rotation) const;
  // Move (ex, ey, ez) to global coordinates
  void UnmapFields(double& ex, double& ey, double& ez, double& xpos,
                   double& ypos, double& zpos, bool& xmirrored, bool& ymirrored,
                   bool& zmirrored, double& rcoordinate, double& rotation);

  int ReadInteger(char* token, int def, bool& error);
  double ReadDouble(char* token, double def, bool& error);

  virtual double GetElementVolume(const int i) = 0;
  virtual void GetAspectRatio(const int i, double& dmin, double& dmax) = 0;

  // Calculate the bounding boxes of all elements after initialization
  void CalculateElementBoundingBoxes(void);

  // Initialize the tetrahedral tree
  bool InitializeTetrahedralTree(void);
};
}

#endif
