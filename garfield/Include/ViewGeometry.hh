#ifndef G_VIEW_GEOMETRY
#define G_VIEW_GEOMETRY

#include <string>

#include <TCanvas.h>
#include <TGeoManager.h>

namespace Garfield {

class GeometrySimple;

/// Visualize a geometry defined using the "native" shapes.

class ViewGeometry {

 public:
  /// Constructor
  ViewGeometry();
  /// Destructor
  ~ViewGeometry();

  /// Set the canvas to be painted on.
  void SetCanvas(TCanvas* c);
  /// Set the geometry to be drawn.
  void SetGeometry(GeometrySimple* geo);
  /// Draw the geometry.
  void Plot();

  /// Enable/disable debugging messages.
  void EnableDebugging(const bool on = true) { m_debug = on; }

 private:
  std::string m_className;

  // Options
  bool m_debug;

  // Canvas
  TCanvas* m_canvas;
  bool m_hasExternalCanvas;

  GeometrySimple* m_geometry;

  std::vector<TGeoVolume*> m_volumes;
  std::vector<TGeoMedium*> m_media;

  TGeoManager* m_geoManager;

  void Reset();

};
}
#endif
