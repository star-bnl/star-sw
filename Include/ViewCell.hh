#ifndef G_VIEW_CELL
#define G_VIEW_CELL

#include <string>

#include <RQ_OBJECT.h>
#include <TCanvas.h>

namespace Garfield {

class ComponentAnalyticField;

class ViewCell { 

  RQ_OBJECT("ViewCell")
  
  public:
    // Constructor
    ViewCell();
    // Destructor
    ~ViewCell();
    
    void SetCanvas(TCanvas* c);

    void SetComponent(ComponentAnalyticField* comp);
    
    // Set area to be plotted
    void SetArea(double xmin, double ymin, double zmin, 
                 double xmax, double ymax, double zmax);
    void SetArea();

    void Clear();

    void Plot();

    void EnableDebugging()  {debug = true;}
    void DisableDebugging() {debug = false;}

    void EnableWireMarkers()  {useWireMarker = true;}
    void DisableWireMarkers() {useWireMarker = false;}  
  
  private:
 
    // Options
    bool debug;
    bool useWireMarker;

    std::string label;

    // Canvas
    TCanvas* canvas;
    bool hasExternalCanvas;
    
    // Box dimensions
    bool hasUserArea;
    double xMin, yMin, zMin, xMax, yMax, zMax;

    ComponentAnalyticField* component;

    void PlotWire(const double x, const double y, const double d);
    void PlotLine(const double x0, const double y0, 
                  const double x1, const double y1);
    void PlotTube(const double x0, const double y0, 
                  const double r, const int n);

};

}
#endif
