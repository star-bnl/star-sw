
#ifndef G_TRACK_SIMPLE_H
#define G_TRACK_SIMPLE_H

#include "Track.hh"

namespace Garfield {

class TrackSimple: public Track {

  public:
    // Constructor
    TrackSimple();
    // Destructor
    ~TrackSimple() {}

    void SetEqualSpacing() {useEqualSpacing = true;}
    void SetExponentialSpacing() {useEqualSpacing = false;}

    void   SetClusterDensity(const double d);
    double GetClusterDensity();
    void   SetStoppingPower(const double dedx);
    double GetStoppingPower();

    bool NewTrack(const double x0, const double y0, const double z0, 
                  const double t0, 
                  const double dx0, const double dy0, const double dz0);
    bool GetCluster(double& xcls, double& ycls, double& zcls, double& tcls,
                    int& n, double& e, double& extra);

  protected:

    bool isReady;

    // Particle position, time and direction
    double x, y, z, t;
    double dx, dy, dz;
    // Mean free path (mean spacing between adjacent clusters)
    double mfp;
    // Average energy per cluster
    double eloss;

    bool useEqualSpacing;

};

}

#endif
