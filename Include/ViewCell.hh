#ifndef G_VIEW_CELL
#define G_VIEW_CELL

#include <string>
#include <memory>

#include <TCanvas.h>
#include <TGeoManager.h>

namespace Garfield {

class ComponentAnalyticField;

/// Visualize the "cell" defined in an analytic-field component.

class ViewCell {

 public:
  /// Constructor
  ViewCell();
  /// Destructor
  ~ViewCell();

  /// Set the canvas on which to draw the cell geometry.
  void SetCanvas(TCanvas* c);
  /// Set the component for which to draw the cell geometry.
  void SetComponent(ComponentAnalyticField* comp);

  /// Set the plot range explicitly.
  void SetArea(const double xmin, const double ymin, const double zmin, 
               const double xmax, const double ymax, const double zmax);
  ///  Take the plot range from the bounding box of the component class.
  void SetArea() { m_hasUserArea = false; }

  /// Make a two-dimensional drawing of the cell geometry.
  void Plot2d();
  /// Make a three-dimensional drawing of the cell geometry (using TGeo).
  void Plot3d();

  /// Switch on/off debugging output.
  void EnableDebugging(const bool on = true) { m_debug = on; }

  /// Visualize wirers using markers or as a circle with the actual wire radius.
  /// The default is markers.
  void EnableWireMarkers(const bool on = true) { m_useWireMarker = on; }
  void DisableWireMarkers() { EnableWireMarkers(false); }

 private:
  std::string m_className = "ViewCell";

  // Options
  bool m_debug = false;
  bool m_useWireMarker = true;

  std::string m_label = "Cell Layout";

  // Canvas
  TCanvas* m_canvas = nullptr;
  bool m_hasExternalCanvas = false;

  // Box dimensions
  bool m_hasUserArea = false;
  double m_xMin = -1., m_yMin = -1., m_zMin = -1.;
  double m_xMax =  1., m_yMax =  1., m_zMax =  1.;

  ComponentAnalyticField* m_component = nullptr;

  // 3D geometry.
  std::unique_ptr<TGeoManager> m_geo;

  bool Plot(const bool use3d);
  void PlotWire(const double x, const double y, const double d, 
                const int type);
  void PlotTube(const double x0, const double y0, const double r, const int n);

};
}
#endif
