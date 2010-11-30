#ifndef G_VIEW_CELL
#define G_VIEW_CELL

#include <string>

#include <RQ_OBJECT.h>
#include <TObject.h>
#include <TCanvas.h>

namespace Garfield {

class ComponentAnalyticField;

class ViewCellWire : public TObject {

  public:
    ViewCellWire(const double x, const double y, const double z,
                 const double diameter, const double length);
    ~ViewCellWire() {}
    TBuffer3D& GetBuffer(bool& ok, const bool debug = false);

  private:

    std::string className;
    // Center
    double x0, y0, z0;
    // Radius 
    double r;
    // Half-length
    double l;
    
};

class ViewCellPlane : public TObject {

  public:
    ViewCellPlane(const double center, const bool vert, const double size);
    ~ViewCellPlane() {}
    TBuffer3D& GetBuffer(bool& ok, const bool debug = false);

  private:

    std::string className;
    double planeCenter;
    bool   isVertical;
    double planeSize;
    
};

class ViewCellTube : public TObject {

  public:
    ViewCellTube(const double x, const double y, const double z,
                 const double radius, const int nEdges);
    ~ViewCellTube() {}

    TBuffer3D& GetBuffer(bool& ok, const bool debug = false);
     
  private:
    std::string className;
    // Center
    double x0, y0, z0;
    // Radius
    double r;
    // Number of edges
    int n;

    TBuffer3D& GetBufferCylinder(bool& ok, const bool debug);
    TBuffer3D& GetBufferPolygon(bool& ok, const bool debug);

};

class ViewCell : public TObject { 

  RQ_OBJECT("ViewCell")
  
  public:
    // Constructor
    ViewCell();
    // Destructor
    ~ViewCell();
    
    void SetCanvas(TCanvas* c);

    void SetComponent(ComponentAnalyticField* comp);
    
    // Set area to be plotted
    void SetArea(double xmin, double ymin, double zmin, 
                 double xmax, double ymax, double zmax);
    void SetArea();

    void Plot2d();
    void Plot3d();

    void EnableDebugging()  {debug = true;}
    void DisableDebugging() {debug = false;}

    void EnableWireMarkers()  {useWireMarker = true;}
    void DisableWireMarkers() {useWireMarker = false;}  

  protected:

    void Draw(Option_t* option);
    void Paint(Option_t* option);
  
  private:

    std::string className;
 
    // Options
    bool debug;
    bool useWireMarker;

    std::string label;

    // Canvas
    TCanvas* canvas;
    bool hasExternalCanvas;
    
    // Box dimensions
    bool hasUserArea;
    double xMin, yMin, zMin, xMax, yMax, zMax;

    ComponentAnalyticField* component;

    // 3d objects
    int nWires3d;
    std::vector<ViewCellWire> wires3d;
    int nTubes3d;
    std::vector<ViewCellTube> tubes3d;
    int nPlanes3d;
    std::vector<ViewCellPlane> planes3d;

    bool Plot(const bool use3d);
    void PlotWire(const double x, const double y, const double d);
    void PlotLine(const double x0, const double y0, 
                  const double x1, const double y1);
    void PlotTube(const double x0, const double y0, 
                  const double r, const int n);

    ClassDef(ViewCell, 0);

};

}
#endif
