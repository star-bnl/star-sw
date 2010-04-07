#ifndef G_OPTICAL_DATA_H
#define G_OPTICAL_DATA_H

#include <string>
#include <vector>

namespace Garfield {

class OpticalData {

  public:
    // Constructor
    OpticalData() : hasData(false), 
                    emin(0.), emax(0.),
                    ionmin(0.), ionmax(0.) {}
    // Destructor
    ~OpticalData() {}

    bool SetMaterial(const std::string material);
    bool GetPhotoabsorptionCrossSection(const double e, double& cs);
    bool GetPhotoionisationYield(const double e, double& eta);

  private:

    bool hasData;

    std::vector<double> energy;
    std::vector<double> eps1;
    std::vector<double> eps2;
    std::vector<double> pacs;

    double emin, emax;

    std::vector<double> energyIon;
    std::vector<double> yieldIon;
    double ionmin, ionmax;

    void Argon();
    void Methane();
    void Isobutane();
 
};

}

#endif
