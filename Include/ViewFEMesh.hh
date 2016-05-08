// Some functionality for this class was copied/modified from ViewField.hh
#ifndef G_VIEW_FE_MESH
#define G_VIEW_FE_MESH

#include <string>
#ifndef __CINT__
#include <map>
#endif

#include <TCanvas.h>
#include <TMatrixD.h>
#include <TArrayD.h>
#include <TGaxis.h>
#include <TPolyLine.h>
#include <TPolyLine3D.h>
#include <TString.h>
#include <TH2D.h>

#include "ViewDrift.hh"
#include "ComponentFieldMap.hh"
#include "ComponentCST.hh"

namespace Garfield {

class ViewFEMesh {

 public:
  // Constructor
  ViewFEMesh();
  // Destructor
  ~ViewFEMesh();

  TCanvas* GetCanvas();

  void SetCanvas(TCanvas* c);
  void SetComponent(ComponentFieldMap* comp);

  // Set area to be plotted
  void SetArea();
  void SetArea(const double xmin, const double ymin, const double zmin, 
               const double xmax, const double ymax, const double zmax);

  // Projection plane
  void SetDefaultProjection();
  void SetPlane(double fx, double fy, double fz, double x0, double y0,
                double z0);

  // Axes
  void SetXaxis(TGaxis* ax);
  void SetYaxis(TGaxis* ay);
  void SetXaxisTitle(const char* xtitle);
  void SetYaxisTitle(const char* ytitle);
  void EnableAxes() { drawAxes = true; }
  void DisableAxes() { drawAxes = false; }

  // Plot method to be called by user
  bool Plot();

  // Element fill switch; 2D only, set false for wireframe mesh
  void SetFillMesh(bool f) { fillMesh = f; }

  // Associate a color with each element material map ID;
  //  Uses ROOT color numberings
  void SetColor(int matID, int colorID) { colorMap[matID] = colorID; }
  void SetFillColor(int matID, int colorID) { colorMap_fill[matID] = colorID; }

  // Set the optional associated ViewDrift
  void SetViewDrift(ViewDrift* vd) { viewDrift = vd; }

  // Show filled mesh elements
  void SetFillMeshWithBorders() {
    plotMeshBorders = true;
    fillMesh = true;
  }

  // Debugging switch
  void EnableDebugging() { debug = true; }
  void DisableDebugging() { debug = false; }

  // Create a default set of custom-made axes.
  void CreateDefaultAxes();

  // Disable a material so that its mesh cells are not drawn
  void DisableMaterial(int materialID) { disabledMaterial[materialID] = true; }

 private:
  std::string className;
  std::string label;

  // Options
  bool debug;
  bool fillMesh;

  // Canvas
  TCanvas* canvas;
  bool hasExternalCanvas;

  // Viewing plane
  double project[3][3];
  double plane[4];

  // Box dimensions
  bool hasUserArea;
  double xMin, yMin, zMin, xMax, yMax, zMax;

  // The field map object
  ComponentFieldMap* component;

  // Optional associated ViewDrift object
  ViewDrift* viewDrift;
  bool plotMeshBorders;

  // Axes
  TGaxis* xaxis, *yaxis;
  TH2D* axes;
  bool drawAxes;

  // The mesh, stored as a vector of TPolyLine(3D) objects
  std::vector<TPolyLine*> mesh;
  std::vector<TPolyLine*> driftLines;

// The color map
#ifndef __CINT__
  std::map<int, int> colorMap;
  std::map<int, int> colorMap_fill;

  // Disabled materials -> not shown in the mesh view
  std::map<int, bool> disabledMaterial;
#endif
  // Element plotting methods
  void DrawElements();
  void DrawCST(ComponentCST* componentCST);
  bool InView(double x, double y);
  bool LinesCrossed(double x1, double y1, double x2, double y2, double u1,
                    double v1, double u2, double v2, double& xc, double& yc);
  bool OnLine(double x1, double y1, double x2, double y2, double u, double v);
  void RemoveCrossings(std::vector<double>& x, std::vector<double>& y);
  bool PlaneCut(double x1, double y1, double z1, double x2, double y2,
                double z2, TMatrixD& xMat);
  bool PlaneCoords(double x, double y, double z, const TMatrixD& projMat,
                   TMatrixD& xMat);
  void ClipToView(std::vector<double>& px, std::vector<double>& py,
                  std::vector<double>& cx, std::vector<double>& cy);
  bool IsInPolygon(double x, double y, std::vector<double>& px,
                   std::vector<double>& py, bool& edge);

  // Plot method to be called by Plot() for CST cubic elements
  // available are "xy", "yz" and "xz"
};
}
#endif
