// Track generation using Heed

#ifndef G_TRACK_HEED_H
#define G_TRACK_HEED_H

#include "Track.hh"

namespace Garfield {

class TrackHeed : public Track {

  public:
    // Constructor
    TrackHeed() {}
    // Destructor
    ~TrackHeed() {}

    void SetParticle(std::string part);
    void NewTrack(
            const double x0, const double y0, const double z0, const double t0,
            const double dx, const double dy, const double dz);
    bool GetCluster(double& xcls, double& ycls, double& zcls,
                    int& n, double& e, double& extra);

  private:

    double mass;
    double q;

};

}

#endif
