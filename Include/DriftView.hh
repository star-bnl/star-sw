#ifndef G_DRIFT_VIEW
#define G_DRIFT_VIEW

#include <RQ_OBJECT.h>
#include <TCanvas.h>
#include <TH3F.h>
#include <TPolyLine3D.h>

namespace Garfield {

// -------------------------------------------------------------------------------------------------------------------------------------

class DriftView { 

  RQ_OBJECT("DriftView")
  
  public:
    // Constructor
    DriftView();
    // Destructor
    ~DriftView() {}
    
    // Set area to be plotted
    void SetArea(double xmin, double ymin, double zmin, 
                 double xmax, double ymax, double zmax);
    void Clear();
    void NewElectronDriftLine(const int n);
    void NewIonDriftLine(const int n);
    void SetPoint(const int i, const double x, const double y, const double z);
    void Plot();

  private:
  
    // Options
    bool debug;

    // Canvas
    TCanvas canvas;
    
    // Box dimensions
    double xMin, yMin, zMin, xMax, yMax, zMax;
    // Frame    
    TH3F frame;

    int nDriftLines;
    std::vector<TPolyLine3D> driftLines;

};

}
#endif
