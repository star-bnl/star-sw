// Track generation using differential cross-sections computed by H. Bichsel
// References:
//   - H. Bichsel, Rev. Mod. Phys. 60 (1988), 663-699
//   - https://faculty.washington.edu/hbichsel/

#ifndef G_TRACK_BICHSEL_H
#define G_TRACK_BICHSEL_H

#include "Track.hh"

namespace Garfield {

class TrackBichsel : public Track {

  public:
    // Constructor
    TrackBichsel();
    // Destructor
    ~TrackBichsel() {}

    bool NewTrack(
        const double x0, const double y0, const double z0, const double t0,
        const double dx0, const double dy0, const double dz0);
    bool GetCluster(double& xcls, double& ycls, double& zcls, double& tcls,
                    int& n, double& e, double& extra);

    double GetClusterDensity();
    double GetStoppingPower();

    void SetDataFile(const std::string filename) {datafile = filename;}

  private:

    // Particle speed and rel. momentum
    double bg;
    double speed;

    // Particle position and direction
    double x, y, z, t;
    double dx, dy, dz;

    // Inverse mean free path    
    double imfp;
  
    std::string datafile;
    // Table of cumulative distribution functions
    std::vector<std::vector<double> > cdf;
    int iCdf;
    int nCdfEntries;

    bool isInitialised;
    bool isInMedium;

    double GetInverseMeanFreePath(const double bg);
    bool LoadCrossSectionTable(const std::string filename);
    void SelectCrossSectionTable();

};

}

#endif
