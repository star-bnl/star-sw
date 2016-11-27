#ifndef G_VIEW_DRIFT
#define G_VIEW_DRIFT

#include <string>
#include <TGraph.h>
#include <TCanvas.h>
#include <TPolyLine3D.h>
#include <TPolyMarker3D.h>
#include <TView.h>

namespace Garfield {

class ViewDrift {

 public:
  // Constructor
  ViewDrift();
  // Destructor
  ~ViewDrift();

  void SetCanvas(TCanvas* c);

  // Set area to be plotted.
  void SetArea(const double& xmin, const double& ymin, const double& zmin, 
               const double& xmax, const double& ymax, const double& zmax);
  void Clear();
  void Plot(const bool twod = false, const bool axis = true);

  void SetClusterMarkerSize(const double& size);
  void SetCollisionMarkerSize(const double& size);

  // Functions to be used by transport classes.
  void NewElectronDriftLine(const unsigned int np, int& id, const double x0,
                            const double y0, const double z0);
  void NewHoleDriftLine(const unsigned int np, int& id, const double x0,
                        const double y0, const double z0);
  void NewIonDriftLine(const unsigned int np, int& id, const double x0,
                       const double y0, const double z0);
  void NewPhotonTrack(const double x0, const double y0, const double z0,
                      const double x1, const double y1, const double z1);
  void NewChargedParticleTrack(const unsigned int np, int& id, const double x0,
                               const double y0, const double z0);

  void SetDriftLinePoint(const unsigned int iL, const unsigned int iP,
                         const double x, const double y, const double z);
  void AddDriftLinePoint(const unsigned int iL, const double x, const double y,
                         const double z);
  void SetTrackPoint(const unsigned int iL, const unsigned int iP,
                     const double x, const double y, const double z);
  void AddTrackPoint(const unsigned int iL, const double x, const double y,
                     const double z);
  void AddExcitationMarker(const double x, const double y, const double z);
  void AddIonisationMarker(const double x, const double y, const double z);
  void AddAttachmentMarker(const double x, const double y, const double z);

  void EnableDebugging() { m_debug = true; }
  void DisableDebugging() { m_debug = false; }

  friend class ViewFEMesh;

 private:
  std::string m_className;

  // Options
  bool m_debug;

  std::string m_label;

  struct marker {
    double x;
    double y;
    double z;
  };
  // Canvas
  TCanvas* m_canvas;
  bool m_hasExternalCanvas;

  // Box dimensions
  double m_xMin, m_yMin, m_zMin;
  double m_xMax, m_yMax, m_zMax;
  // View
  TView* m_view;

  struct driftLine {
    std::vector<marker> vect;
    int n;  // what kind of particle?
  };
  std::vector<driftLine> m_driftLines;
  std::vector<TPolyLine3D*> m_driftLinePlots;

  struct track {
    std::vector<marker> vect;
  };
  std::vector<track> m_tracks;
  std::vector<TPolyMarker3D*> m_trackPlots;
  std::vector<TPolyLine3D*> m_trackLinePlots;

  std::vector<marker> m_excMarkers;
  TPolyMarker3D* m_excPlot;
  std::vector<marker> m_ionMarkers;
  TPolyMarker3D* m_ionPlot;
  std::vector<marker> m_attMarkers;
  TPolyMarker3D* m_attPlot;

  double m_markerSizeCluster;
  double m_markerSizeCollision;

  void Plot2d(const bool axis);
  void Plot3d(const bool axis);
};
}
#endif
