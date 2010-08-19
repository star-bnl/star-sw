#ifndef G_VIEW_DRIFT
#define G_VIEW_DRIFT

#include <string>

#include <RQ_OBJECT.h>
#include <TCanvas.h>
#include <TPolyLine3D.h>
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
    void NewElectronDriftLine(const int n);
    void NewIonDriftLine(const int n);
    void NewPhotonTrack(const double x0, const double y0, const double z0,
                        const double x1, const double y1, const double z1);
    void SetPoint(const int i, 
                  const double x, const double y, const double z);
    void Plot();

  private:
 
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

};

}
#endif
