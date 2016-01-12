#ifndef G_VIEW_FIELD
#define G_VIEW_FIELD

#include <TCanvas.h>
#include <TF2.h>
#include <TF1.h>

namespace Garfield {

class Sensor;
class ComponentBase;

class ViewField {

 public:
  // Constructor
  ViewField();
  // Destructor
  ~ViewField();

  void SetSensor(Sensor* s);
  void SetComponent(ComponentBase* c);
  void SetCanvas(TCanvas* c);

  // Range of the plot
  void SetVoltageRange(const double minval, const double maxval);
  void SetElectricFieldRange(const double minval, const double maxval);
  void SetWeightingFieldRange(const double minval, const double maxval);

  // Establish area, normal vector, in-plane vector ...
  void SetArea(double xmin, double ymin, double xmax, double ymax);
  void SetNumberOfContours(const int n);
  void SetDefaultProjection();
  void SetPlane(double fx, double fy, double fz, double x0, double y0,
                double z0);
  void SetNumberOfSamples1d(const int n);
  void SetNumberOfSamples2d(const int nx, const int ny);

  // Rotate the viewing plane
  void Rotate(double angle);

  void PlotContour(const std::string option = "v");
  void PlotSurface(const std::string option = "v");
  void PlotProfile(const double x0, const double y0, const double z0,
                   const double x1, const double y1, const double z1,
                   const std::string option = "v");
  void PlotContourWeightingField(const std::string label,
                                 const std::string option);
  void PlotSurfaceWeightingField(const std::string label,
                                 const std::string option);

  double EvaluatePotential(double* pos, double* par);
  double EvaluatePotentialProfile(double* pos, double* par);
  double EvaluateWeightingField(double* pos, double* par);

  void EnableAcknowledgeStatus(const double v0 = 0.) {
    useStatus = true;
    vBkg = v0;
  }
  void DisableAcknowledgeStatus() { useStatus = false; }

  void EnableDebugging() { debug = true; }
  void DisableDebugging() { debug = false; }

 private:
  std::string className;

  static const int nMaxContours = 50;

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
  double emin, emax;
  double wmin, wmax;

  // Number of contours
  int nContours;
  // Number of points used to draw the functions
  int nSamples1d;
  int nSamples2dX, nSamples2dY;
  // Weighting field label
  std::string electrode;

  // Canvas
  TCanvas* canvas;
  bool hasExternalCanvas;

  // Potential function
  TF2* fPot;
  TF2* fWfield;
  TF1* fPotProfile;

  void Labels();
  void CreateFunction();
  void CreateProfileFunction();
  void CreateFunctionWeightingField();
};
}
#endif
