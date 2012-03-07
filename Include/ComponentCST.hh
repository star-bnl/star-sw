//Copied and modified ComponentAnsys123.hh
//This Interface assumes a certain format of the ascii files
//Please find the tools to extract the field data from CST
//in the correct way here: http://www.desy.de/~zenker/garfieldpp.html

#ifndef G_COMPONENT_CST_H
#define G_COMPONENT_CST_H

#include <vector>
#include "ComponentFieldMap.hh"

namespace Garfield {

class ComponentCST: public ComponentFieldMap {

  public:
    // Constructor
    ComponentCST();
    // Destructor
    ~ComponentCST() {}
    std::vector<double> m_xlines;
    std::vector<double> m_ylines;
    std::vector<double> m_zlines;

    bool GetMedium(const double x, const double y, const double z,
                   Medium*& medium);
    void ElectricField(const double x, const double y, const double z,
                       double& ex, double& ey, double& ez,
                       Medium*& m, int& status);
    void ElectricField(const double x, const double y, const double z,
                       double& ex, double& ey, double& ez, double& v,
                       Medium*& m, int& status);

    void WeightingField(const double x, const double y, const double z,
                        double& wx, double& wy, double& wz,
                        const std::string label);

    double WeightingPotential(const double x, const double y, const double z,
                              const std::string label);

    bool Initialise(std::string elist = "ELIST.lis",
                    std::string nlist = "NLIST.lis",
                    std::string mplist = "MPLIST.lis",
                    std::string prnsol = "PRNSOL.lis",
                    std::string unit = "cm");
    bool SetWeightingField(std::string prnsol, std::string label);

    // Range
    bool IsInBoundingBox(const double x, const double y, const double z) {
      return x >= xMinBoundingBox && x <= xMaxBoundingBox &&
             y >= yMinBoundingBox && y <= yMaxBoundingBox &&
             z >= zMinBoundingBox && z <= zMaxBoundingBox;
    }

    void SetRangeZ(const double zmin, const double zmax);

  protected:

    // Verify periodicities
    void UpdatePeriodicity();
    int FindElementCube(const double x, const double y, const double z,
                        double& t1, double& t2, double& t3,
                        double jac[3][3], double& det);
    double GetElementVolume(const int i);
    void GetAspectRatio(const int i, double& dmin, double& dmax);
    static bool Greater(const double &a, const double &b) {return (a > b);};
    void Element2Index(int element,int &i,int &j, int &k);
    void GetNodesForElement(int element, std::vector<int> &nodes);
};

struct PolygonInfo {
  // This struct is used for drawing the mesh with ViewFEMesh.h
  double p1[2];
  double p2[2];
  double p3[2];
  double p4[2];
  int element;
  int material;
};

}

#endif
