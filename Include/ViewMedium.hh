#ifndef G_VIEW_MEDIUM
#define G_VIEW_MEDIUM

#include <string>
#include <vector>

#include <RQ_OBJECT.h>
#include <TCanvas.h>
#include <TF1.h>
#include <TGraph.h>

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
    void SetBAngleRange(const double amin, const double amax); //new
    void SetFunctionRange(const double vmin, const double vmax);
    void SetFunctionRange();

    void PlotElectronVelocity(const char xaxis, const double e, const double b, const double a);
    void PlotHoleVelocity(const char xaxis, const double e, const double b, const double a);
    void PlotIonVelocity(const char xaxis, const double e, const double b, const double a);
    void PlotElectronDiffusion(const char xaxis, const double e, const double b, const double a);
    void PlotHoleDiffusion(const char xaxis, const double e, const double b, const double a);
    void PlotIonDiffusion(const char xaxis, const double e, const double b, const double a);
    void PlotElectronTownsend(const char xaxis, const double e, const double b, const double a);
    void PlotHoleTownsend(const char xaxis, const double e, const double b, const double a);
    void PlotElectronAttachment(const char xaxis, const double e, const double b, const double a);
    void PlotHoleAttachment(const char xaxis, const double e, const double b, const double a);
    void PlotElectronCrossSections(const char xaxis, const double e, const double b, const double a);
    double EvaluateFunction(double* pos, double* par);

  private:

    std::string className;
 
    // Options
    bool debug;

    // Canvas
    TCanvas* canvas;
    bool hasExternalCanvas;

    Medium* medium;
  
    // Ranges for variable parameters
    double eMin, eMax;
    double bMin, bMax;
    double aMin, aMax;
    double vMin, vMax;
    
    // Fixed parameters
    double efield;
    double bfield;
    double angle;    
    
    // Tolerances for marker plotting
    double etolerance;
    double btolerance;
    double atolerance;

    // Functions
    std::vector<TF1> functions;
    int nFunctions;
    // Graphs
    std::vector<TGraph> graphs;
    int nGraphs;
    
    void SetupCanvas();
    void AddFunction(const double xmin, const double xmax, 
                     const double ymin, const double ymax,
                     const bool keep, 
                     const std::string xlabel, const std::string ylabel,
                     const int type, const char xaxis, const double e, 
                     const double b, const double a);
 
};

}
#endif
