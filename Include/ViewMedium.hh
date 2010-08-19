#ifndef G_VIEW_MEDIUM
#define G_VIEW_MEDIUM

#include <string>
#include <vector>

#include <RQ_OBJECT.h>
#include <TCanvas.h>
#include <TF1.h>

namespace Garfield {

class Medium;

class ViewMedium { 

  RQ_OBJECT("ViewMedium")
  
  public:
    // Constructor
    ViewMedium();
    // Destructor
    ~ViewMedium();
    
    void SetCanvas(TCanvas* c);
    
    void SetMedium(Medium* m);
    
    void SetElectricFieldRange(const double emin, const double emax);
    void SetMagneticFieldRange(const double bmin, const double bmax); 

    void PlotElectronVelocity(const bool keep = false);
    void PlotHoleVelocity(const bool keep = false);
    void PlotIonVelocity(const bool keep = false);

    void PlotElectronTownsend(const bool keep = false);
    void PlotHoleTownsend(const bool keep = false);

    void PlotElectronAttachment(const bool keep = false);
    void PlotHoleAttachment(const bool keep = false);
    
    double EvaluateFunction(double* pos, double* par);

  private:
 
    // Options
    bool debug;

    // Canvas
    TCanvas* canvas;
    bool hasExternalCanvas;

    Medium* medium;
  
    // Ranges 
    double eMin, eMax;
    double bMin, bMax;

    // Functions
    std::vector<TF1> functions;
    
    void SetupCanvas();
    void AddFunction(const double xmin, const double xmax, const bool keep, 
                     const std::string xlabel, const std::string ylabel,
                     const int type);
 
};

}
#endif
