#ifndef G_VIEW_CELL
#define G_VIEW_CELL

#include <string>

#include <TCanvas.h>
#include <TGeoManager.h>

namespace Garfield {

class ComponentAnalyticField;

class ViewCell {

 public:
  // Constructor
  ViewCell();
  // Destructor
  ~ViewCell();

  void SetCanvas(TCanvas* c);

  void SetComponent(ComponentAnalyticField* comp);

  // Set area to be plotted
  void SetArea(const double& xmin, const double& ymin, const double& zmin, 
               const double& xmax, const double& ymax, const double& zmax);
  void SetArea();

  void Plot2d();
  void Plot3d();

  void EnableDebugging() { m_debug = true; }
  void DisableDebugging() { m_debug = false; }

  void EnableWireMarkers() { m_useWireMarker = true; }
  void DisableWireMarkers() { m_useWireMarker = false; }

 private:
  std::string m_className;

  // Options
  bool m_debug;
  bool m_useWireMarker;

  std::string m_label;

  // Canvas
  TCanvas* m_canvas;
  bool m_hasExternalCanvas;

  // Box dimensions
  bool m_hasUserArea;
  double m_xMin, m_yMin, m_zMin;
  double m_xMax, m_yMax, m_zMax;

  ComponentAnalyticField* m_component;

  // 3d objects
  std::vector<TGeoVolume*> m_volumes;
  std::vector<TGeoMedium*> m_media;

  TGeoManager* m_geoManager;

  bool Plot(const bool use3d);
  void PlotWire(const double& x, const double& y, const double& d, 
                const int& type);
  void PlotLine(const double& x0, const double& y0, 
                const double& x1, const double& y1);
  void PlotTube(const double& x0, const double& y0, const double& r, 
                const int& n);

  void Reset();

};
}
#endif
