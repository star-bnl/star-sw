#ifndef G_FIELD_VIEW
#define G_FIELD_VIEW

#include <RQ_OBJECT.h>
#include <TCanvas.h>
#include <TF2.h>

#include "Sensor.hh"

namespace Garfield {

// -------------------------------------------------------------------------------------------------------------------------------------

class FieldView { 

  RQ_OBJECT("FieldView")
  
  public:
    // Constructor
    FieldView(Sensor* s);
    // Destructor
    ~FieldView() {}
    
    // Establish area, normal vector, in-plane vector ...
    void Area(double xmin, double ymin, double zmin, 
              double xmax, double ymax, double zmax);
    void Contour(char* function, TCanvas* c);
    void Default();
    void NContour(const int n) {nContours = n;}
    void Plane(double fx, double fy, double fz, double x0, double y0, double z0);
    void Range(const double minval, const double maxval);
    // Rotate the viewing plane
    void Rotate(double angle);

  private:
  
    // Options
    bool debug;
    
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
    
    // Contour function
    TF2* fcont;
    
    void Labels();    

};

}
#endif
