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
  void SetNumberOfContours(const unsigned int n);
  void SetDefaultProjection();
  void SetPlane(double fx, double fy, double fz, double x0, double y0,
                double z0);
  void SetNumberOfSamples1d(const unsigned int n);
  void SetNumberOfSamples2d(const unsigned int nx, const unsigned int ny);

  // Rotate the viewing plane
  void Rotate(double angle);

  void PlotContour(const std::string& option = "v");
  void PlotSurface(const std::string& option = "v");
  void Plot(const std::string& option = "v", 
            const std::string& drawopt = "arr");
  void PlotProfile(const double x0, const double y0, const double z0,
                   const double x1, const double y1, const double z1,
                   const std::string& option = "v");
  void PlotContourWeightingField(const std::string& label,
                                 const std::string& option);
  void PlotSurfaceWeightingField(const std::string& label,
                                 const std::string& option);

  double EvaluatePotential(double* pos, double* par);
  double EvaluatePotentialProfile(double* pos, double* par);
  double EvaluateWeightingField(double* pos, double* par);

  void EnableAcknowledgeStatus(const double v0 = 0.) {
    m_useStatus = true;
    m_vBkg = v0;
  }
  void DisableAcknowledgeStatus() { m_useStatus = false; }

  void EnableDebugging() { m_debug = true; }
  void DisableDebugging() { m_debug = false; }

 private:
  std::string m_className;

  static const int nMaxContours = 50;

  // Options
  bool m_debug;

  bool m_useStatus;
  double m_vBkg;

  // Sensor
  Sensor* m_sensor;
  ComponentBase* m_component;

  // Projection for viewing
  double m_project[3][3];
  double m_plane[4];
  char m_xLabel[50], m_yLabel[50], m_description[50];

  // Box dimensions
  double m_pxmin, m_pymin, m_pzmin;
  double m_pxmax, m_pymax, m_pzmax;
  // Function range
  double m_fmin, m_fmax;
  double m_emin, m_emax;
  double m_wmin, m_wmax;

  // Number of contours
  unsigned int m_nContours;
  // Number of points used to draw the functions
  unsigned int m_nSamples1d;
  unsigned int m_nSamples2dX, m_nSamples2dY;
  // Weighting field label
  std::string m_electrode;

  // Canvas
  TCanvas* m_canvas;
  bool m_hasExternalCanvas;

  // Potential function
  TF2* m_fPot;
  TF2* m_fWfield;
  TF1* m_fPotProfile;

  void Labels();
  void CreateFunction();
  void CreateProfileFunction();
  void CreateFunctionWeightingField();
  int SetupFunction(const std::string& option, TF2* f);
  void SetupCanvas(const bool twod);
};
}
#endif
