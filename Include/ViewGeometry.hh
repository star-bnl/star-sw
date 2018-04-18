#ifndef G_VIEW_GEOMETRY
#define G_VIEW_GEOMETRY

#include <string>
#include <vector>
#include <memory>

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
  std::string m_className = "ViewGeometry";

  // Options
  bool m_debug = false;

  // Canvas
  TCanvas* m_canvas = nullptr;
  bool m_hasExternalCanvas = false;

  GeometrySimple* m_geometry = nullptr;

  std::vector<TGeoVolume*> m_volumes;
  std::vector<TGeoMedium*> m_media;

  std::unique_ptr<TGeoManager> m_geoManager;

  void Reset();

};
}
#endif
