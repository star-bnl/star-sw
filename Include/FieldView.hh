#ifndef G_FIELD_VIEW
#define G_FIELD_VIEW

#include <RQ_OBJECT.h>
#include <TCanvas.h>
#include <TF2.h>

#include "Sensor.hh"

namespace Garfield {

class FieldView { 

  RQ_OBJECT("FieldView")
  
  public:
    // Constructors
    FieldView();
    FieldView(Sensor* s);
    // Destructor
    ~FieldView();
   
    void SetSensor(Sensor* s);
    void SetCanvas(TCanvas* c);
 
    // Establish area, normal vector, in-plane vector ...
    void SetArea(double xmin, double ymin, double zmin, 
                 double xmax, double ymax, double zmax);
    void SetVoltageRange(const double minval, const double maxval);
    void SetNumberOfContours(const int n);
    void SetDefaultProjection();
    void SetPlane(double fx, double fy, double fz, 
                  double x0, double y0, double z0);
    // Rotate the viewing plane
    void Rotate(double angle);

    void PlotContour();
    void PlotSurface();

    double EvaluatePotential(double* pos, double* par);

    void EnableDebugging()  {debug = true;}
    void DisableDebugging() {debug = false;}

  private:
 
    static const int nMaxContours = 20;
 
    // Options
    bool debug;
    
    // Sensor
    Sensor* sensor;

    // Projection for viewing
    double project[3][3];
    double plane[4];
    char xLabel[50], yLabel[50], description[50];
    
    // Box dimensions
    double pxmin, pymin, pzmin, pxmax, pymax, pzmax;
    // Function range
    double fmin, fmax;
    
    // Number of contours
    int nContours;
    
    // Canvas
    TCanvas* canvas;
    bool hasExternalCanvas;

    // Potential function
    TF2* fPot;
    
    void Labels();
    void CreateFunction(); 

};

}
#endif
