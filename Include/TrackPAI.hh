// Energy loss calculation using the Photoabsorption-Ionisation Model

#ifndef G_TRACK_PAI
#define G_TRACK_PAI

#include <string>
#include <vector>

#include "Track.hh"

namespace Garfield {

class TrackPAI : public Track {

  public:
    // Constructor
    TrackPAI();
    // Destructor
    ~TrackPAI();
    
    bool NewTrack(const double x0, const double y0, const double z0, 
                  const double t0, 
                  const double dx0, const double dy0, const double dz0);

    bool GetCluster(double& xcls, double& ycls, double& zcls, double& tcls,
                    int& ncls, double& ecls, double& extra);
  
    double GetClusterDensity();
    double GetStoppingPower();
        
  private:

    bool ready;
        
    // Particle coordinates and direction
    double x, y, z, t;
    double dx, dy, dz;
    // Particle energy and speed
    double e;
    double speed;    
    // Max. energy transfer in a collision
    double emax;
  
    // Total inelastic mean free path
    double imfp;
    // Stopping power
    double dedx;
    
    // Dielectric function
    int nSteps;
    struct opticalData {
      double eps1, eps2;
      double integral;
    };
    std::vector<opticalData> opticalDataTable;
  
    // Tables for interpolation of cumulative distribution functions
    std::vector<double> energies;
    std::vector<double> cdf;
    std::vector<double> rutherford;

    struct electron {
      // Direction
      double dx, dy, dz;
      // Energy
      double energy;
      // Type (electron, hole)
      int type;
    }; 
    std::vector<electron> electrons;
    std::vector<electron> holes;
   
    // Medium properties 
    std::string mediumName;
    double mediumDensity;
    double electronDensity;
    
    bool SetupMedium(Medium* medium);
    bool SetupCrossSectionTable();

    double ComputeMaxTransfer() const;
  
    double ComputeCsTail(const double emin, const double emax);
    double ComputeDeDxTail(const double emin, const double emax);

    double SampleAsymptoticCs(double u);
    double SampleAsymptoticCsSpinZero(const double emin, double u);
    double SampleAsymptoticCsSpinHalf(const double emin, double u);
    double SampleAsymptoticCsSpinOne(const double emin, double u);
    double SampleAsymptoticCsElectron(const double emin, double u);
    double SampleAsymptoticCsPositron(const double emin, double u);
    
    double LossFunction(const double eps1, const double eps2);
    
};

}

#endif
