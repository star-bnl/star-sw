#ifndef G_VIEW_FIELD
#define G_VIEW_FIELD

#include <RQ_OBJECT.h>
#include <TCanvas.h>
#include <TF2.h>
#include <TF1.h>

namespace Garfield {

class Sensor;
class ComponentBase;

class ViewField { 

  RQ_OBJECT("ViewField")
  
  public:
    // Constructor
    ViewField();
    // Destructor
    ~ViewField();
   
    void SetSensor(Sensor* s);
    void SetComponent(ComponentBase* c);
    void SetCanvas(TCanvas* c);
 
    // Establish area, normal vector, in-plane vector ...
    void SetArea(double xmin, double ymin, 
                 double xmax, double ymax);
    void SetVoltageRange(const double minval, const double maxval);
    void SetNumberOfContours(const int n);
    void SetDefaultProjection();
    void SetPlane(double fx, double fy, double fz, 
                  double x0, double y0, double z0);
    void SetNumberOfSamples1d(const int n);
    void SetNumberOfSamples2d(const int nx, const int ny);

    // Rotate the viewing plane
    void Rotate(double angle);

    void PlotContour();
    void PlotSurface();
    void PlotProfile(const double x0, const double y0, const double z0,
                     const double x1, const double y1, const double z1); 

    double EvaluatePotential(double* pos, double* par);
    double EvaluatePotentialProfile(double* pos, double* par);

    void EnableAcknowledgeStatus(const double v0 = 0.) {
      useStatus = true; vBkg = v0;
    }
    void DisableAcknowledgeStatus() {useStatus = false;}

    void EnableDebugging()  {debug = true;}
    void DisableDebugging() {debug = false;}

  private:

    std::string className;
 
    static const int nMaxContours = 30;

    // Options
    bool debug;
   
    bool useStatus;
    double vBkg;
 
    // Sensor
    Sensor* sensor;
    ComponentBase* component;

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
    // Number of points used to draw the functions
    int nSamples1d;
    int nSamples2dX, nSamples2dY;
    
    // Canvas
    TCanvas* canvas;
    bool hasExternalCanvas;

    // Potential function
    TF2* fPot;
    TF1* fPotProfile;
    
    void Labels();
    void CreateFunction();
    void CreateProfileFunction(); 

};

}
#endif
