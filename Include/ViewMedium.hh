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
    void SetFunctionRange(const double vmin, const double vmax);
    void SetFunctionRange();

    void PlotElectronVelocity();
    void PlotHoleVelocity();
    void PlotIonVelocity();

    void PlotElectronDiffusion();
    void PlotHoleDiffusion();
    void PlotIonDiffusion();

    void PlotElectronTownsend();
    void PlotHoleTownsend();

    void PlotElectronAttachment();
    void PlotHoleAttachment();
   
    void PlotElectronCrossSections();

    double EvaluateFunction(double* pos, double* par);

  private:

    std::string className;
 
    // Options
    bool debug;

    // Canvas
    TCanvas* canvas;
    bool hasExternalCanvas;

    Medium* medium;
  
    // Ranges 
    double eMin, eMax;
    double bMin, bMax;
    double vMin, vMax;

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
                     const int type);
 
};

}
#endif
