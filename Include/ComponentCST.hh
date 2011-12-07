#ifndef G_COMPONENT_CST_H
#define G_COMPONENT_CST_H

#include <RQ_OBJECT.h>

#include "ComponentFieldMap.hh"

namespace Garfield {

class ComponentCST: public ComponentFieldMap {

  RQ_OBJECT("ComponentCST")

  public:
    // Constructor
    ComponentCST();
    // Destructor
    ~ComponentCST() {delete m_SurroundingElements;m_SurroundingElements = 0;}

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

    double GetElementVolume(const int i);
    void GetAspectRatio(const int i, double& dmin, double& dmax);
    void Element2Index(int element,int &i,int &j, int &k);
    std::vector<int> GetSurroundingElements(int element);
    std::vector<int>* m_SurroundingElements;
    int m_LastElementCalled;
    int m_xlines;
    int m_ylines;
    int m_zlines;

};

}

#endif
