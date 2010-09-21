// Abstract base class for track generation

#ifndef G_TRACK_H
#define G_TRACK_H

#include <string>
#include <cmath>

namespace Garfield {

class Sensor;
class ViewDrift;

class Track {

  public:
    // Constructor
    Track();
    // Destructor
    virtual ~Track() {}

    virtual void SetParticle(std::string part);

    void SetEnergy(const double e);
    void SetBetaGamma(const double bg);
    void SetBeta(const double beta);
    void SetGamma(const double gamma);
    void SetMomentum(const double p);
    void SetKineticEnergy(const double ekin);

    double GetEnergy() const    {return energy;}
    double GetBetaGamma() const {return sqrt(beta2 / (1. - beta2));}
    double GetBeta() const      {return sqrt(beta2);}
    double GetGamma() const     {return sqrt(1. / (1. - beta2));}
    double GetMomentum() const  {return mass * sqrt(beta2 / (1. - beta2));}
    double GetKineticEnergy() const {return energy - mass;}

    void SetSensor(Sensor* s);

    virtual void NewTrack(
            const double x0, const double y0, const double z0, const double t0, 
            const double dx0, const double dy0, const double dz0) = 0;
    virtual bool GetCluster(
            double& xcls, double& ycls, double& zcls, double& tcls,
            int& n, double& e, double& extra) = 0;

    void EnablePlotting(ViewDrift* viewer);
    void DisablePlotting();

    void EnableDebugging()  {debug = true;}
    void DisableDebugging() {debug = false;}

  protected:

    double q;
    int spin;
    double mass;
    double energy;
    double beta2;
    bool isElectron;
    std::string particleName;

    Sensor* sensor;

    bool isChanged;

    bool usePlotting;
    ViewDrift* viewer;

    bool debug;

    int plotId;
    void PlotNewTrack(const double x0, const double y0, const double z0);
    void PlotCluster(const double x0, const double y0, const double z0);

};

}

#endif
