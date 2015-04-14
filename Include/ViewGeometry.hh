#ifndef G_VIEW_GEOMETRY
#define G_VIEW_GEOMETRY

#include <string>

#include <TCanvas.h>
#include <TGeoManager.h>

namespace Garfield {

class GeometrySimple;

class ViewGeometry {

 public:
  // Constructor
  ViewGeometry();
  // Destructor
  ~ViewGeometry();

  void SetCanvas(TCanvas* c);

  void SetGeometry(GeometrySimple* geo);

  void Plot();

  void EnableDebugging() { m_debug = true; }
  void DisableDebugging() { m_debug = false; }

 private:
  std::string m_className;

  // Options
  bool m_debug;

  std::string m_label;

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
