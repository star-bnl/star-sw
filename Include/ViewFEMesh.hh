// Some functionality for this class was copied/modified from ViewField.hh
#ifndef G_VIEW_FE_MESH
#define G_VIEW_FE_MESH

#include <string>

#include <RQ_OBJECT.h>
#include <TCanvas.h>
#include <TMatrixD.h>
#include <TArrayD.h>
#include <TGaxis.h>
#include <map>
#include <TPolyLine.h>
#include <TPolyLine3D.h>

#include "ViewDrift.hh"
#include "ComponentFieldMap.hh"

namespace Garfield {

class ViewFEMesh { 

  RQ_OBJECT("ViewFEMesh")
  
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
    void SetArea(double xmin, double ymin,
                 double xmax, double ymax);

    // Projection plane
    void SetDefaultProjection();
    void SetPlane(double fx, double fy, double fz, 
                  double x0, double y0, double z0);

    // Axes
    void SetXaxis(TGaxis* ax);
    void SetYaxis(TGaxis* ay);
    void EnableAxes()  {drawAxes = true;}
    void DisableAxes() {drawAxes = false;}

    // Plot method to be called by user
    bool Plot();

    // Element fill switch; 2D only, set false for wireframe mesh
    void SetFillMesh(bool f) { fillMesh = f;}

    // Associate a color with each element material map ID;
    //  Uses ROOT color numberings
    void SetColor(int matID, int colorID) { colorMap[matID] = colorID; }

    // Set the optional associated ViewDrift
    void SetViewDrift(ViewDrift * vd) { viewDrift = vd; }

    // Debugging switch
    void EnableDebugging()  {debug = true;}
    void DisableDebugging() {debug = false;}
  
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
    ViewDrift * viewDrift;

    // Axes
    TGaxis * xaxis;
    TGaxis * yaxis;
    bool drawAxes;

    // The mesh, stored as a vector of TPolyLine(3D) objects
    std::vector<TPolyLine*> mesh;
    std::vector<TPolyLine*> driftLines;

    // The color map
    std::map<int,int> colorMap;

    // Element plotting methods
    void CreateDefaultAxes();
    void DrawElements();
    bool InView(double x, double y);
    bool LinesCrossed(double x1, double y1, double x2, double y2,
             double u1, double v1, double u2, double v2);
    bool OnLine(double x1, double y1, double x2, double y2, double u, double v);
    void RemoveCrossings(std::vector<double> & x, std::vector<double> & y);
    bool PlaneCut(double x1, double y1, double z1, double x2, double y2, 
                  double z2, TMatrixD & xMat);
    bool PlaneCoords(double x, double y, double z, const TMatrixD& projMat, 
                     TMatrixD& xMat);

};

}
#endif
