#ifndef G_VIEW_DRIFT
#define G_VIEW_DRIFT

#include <string>

#include <RQ_OBJECT.h>
#include <TCanvas.h>
#include <TPolyLine3D.h>
#include <TPointSet3D.h>
#include <TView.h>

namespace Garfield {

class ViewDrift { 

  RQ_OBJECT("ViewDrift")
  
  public:
    // Constructor
    ViewDrift();
    // Destructor
    ~ViewDrift();
    
    void SetCanvas(TCanvas* c);
    
    // Set area to be plotted
    void SetArea(double xmin, double ymin, double zmin, 
                 double xmax, double ymax, double zmax);
    void Clear();
    void NewElectronDriftLine(const int np, int& id,
                        const double x0, const double y0, const double z0);
    void NewHoleDriftLine(const int np, int& id,
                          const double x0, const double y0, const double z0);
    void NewIonDriftLine(const int np, int& id,
                        const double x0, const double y0, const double z0);
    void NewPhotonTrack(const double x0, const double y0, const double z0,
                        const double x1, const double y1, const double z1);
    void NewChargedParticleTrack(const int np, int& id,
                        const double x0, const double y0, const double z0);

    void SetDriftLinePoint(const int iL, const int iP, 
                  const double x, const double y, const double z);
    void AddDriftLinePoint(const int iL,
                  const double x, const double y, const double z);
    void SetTrackPoint(const int iL, const int iP,
                  const double x, const double y, const double z);
    void AddTrackPoint(const int iL, 
                  const double x, const double y, const double z);

    void Plot();

    void EnableDebugging()  {debug = true;}
    void DisableDebugging() {debug = false;}

  private:

    std::string className;
 
    // Options
    bool debug;

    std::string label;

    // Canvas
    TCanvas* canvas;
    bool hasExternalCanvas;
    
    // Box dimensions
    double xMin, yMin, zMin, xMax, yMax, zMax;
    // View
    TView* view;

    int nDriftLines;
    std::vector<TPolyLine3D> driftLines;
    
    int nTracks;
    std::vector<TPointSet3D> tracks;

};

}
#endif
