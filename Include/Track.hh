// Abstract base class for track generation

#ifndef G_TRACK_H
#define G_TRACK_H

#include <string>

namespace Garfield {

class Track {

  public:
    // Constructor
    Track() : debug(false) {}
    // Destructor
    virtual ~Track() {}

    virtual void SetParticle(std::string part) = 0;
    virtual void NewTrack(
            const double x0, const double y0, const double z0, const double t0, 
            const double dx, const double dy, const double dz) = 0;
    virtual bool GetCluster(double& xcls, double& ycls, double& zcls,
                            int& n, double& e, double& extra) = 0;

    void EnableDebugging()  {debug = true;}
    void DisableDebugging() {debug = false;}

  protected:

    bool debug;

};

}

#endif
