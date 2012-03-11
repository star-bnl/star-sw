#ifndef G_VIEW_DRIFT
#define G_VIEW_DRIFT

#include <string>

#include <RQ_OBJECT.h>
#include <TCanvas.h>
#include <TPolyLine3D.h>
#include <TPointSet3D.h>
#include <TView.h>

namespace Garfield {

class ViewDrift { 

  RQ_OBJECT("ViewDrift")
  
  public:
    // Constructor
    ViewDrift();
    // Destructor
    ~ViewDrift();
    
    void SetCanvas(TCanvas* c);
    
    // Set area to be plotted.
    void SetArea(double xmin, double ymin, double zmin, 
                 double xmax, double ymax, double zmax);
    void Clear();
    void Plot();

    void SetClusterMarkerSize(const double size);
    void SetCollisionMarkerSize(const double size);

    // Functions to be used by transport classes.
    void NewElectronDriftLine(const int np, int& id,
                              const double x0, const double y0, 
                              const double z0);
    void NewHoleDriftLine(const int np, int& id,
                          const double x0, const double y0, const double z0);
    void NewIonDriftLine(const int np, int& id,
                         const double x0, const double y0, const double z0);
    void NewPhotonTrack(const double x0, const double y0, const double z0,
                        const double x1, const double y1, const double z1);
    void NewChargedParticleTrack(const int np, int& id,
                                 const double x0, const double y0, 
                                 const double z0);

    void SetDriftLinePoint(const int iL, const int iP, 
                  const double x, const double y, const double z);
    void AddDriftLinePoint(const int iL,
                  const double x, const double y, const double z);
    void SetTrackPoint(const int iL, const int iP,
                  const double x, const double y, const double z);
    void AddTrackPoint(const int iL, 
                  const double x, const double y, const double z);
    void AddExcitationMarker(const double x, const double y, const double z);
    void AddIonisationMarker(const double x, const double y, const double z);
    void AddAttachmentMarker(const double x, const double y, const double z);

    void EnableDebugging()  {debug = true;}
    void DisableDebugging() {debug = false;}

    friend class ViewFEMesh;

  private:

    std::string className;
 
    // Options
    bool debug;

    std::string label;

    // Canvas
    TCanvas* canvas;
    bool hasExternalCanvas;
    
    // Box dimensions
    double xMin, yMin, zMin, xMax, yMax, zMax;
    // View
    TView* view;

    int nDriftLines;
    std::vector<TPolyLine3D> driftLines;
    
    int nTracks;
    std::vector<TPointSet3D> tracks;

    struct marker {
      double x;
      double y;
      double z;
    };
    int nExcMarkers;
    std::vector<marker> excMarkers;
    TPointSet3D* excPlot;
    int nIonMarkers;
    std::vector<marker> ionMarkers;
    TPointSet3D* ionPlot;
    int nAttMarkers;
    std::vector<marker> attMarkers;
    TPointSet3D* attPlot;

    double markerSizeCluster;
    double markerSizeCollision;

};

}
#endif
