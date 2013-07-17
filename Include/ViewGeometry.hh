#ifndef G_VIEW_GEOMETRY
#define G_VIEW_GEOMETRY

#include <string>

#include <RQ_OBJECT.h>
#include <TObject.h>
#include <TCanvas.h>
#include <TBuffer3D.h>

namespace Garfield {

class GeometrySimple;
class Solid;

class ViewGeometryShape : public TObject {

  public:
    ViewGeometryShape();
    ~ViewGeometryShape() {}

    void SetSolid(Solid* s);    
    void SetColor(int col);
    TBuffer3D& GetBuffer(bool& ok);

  private:
    std::string className;

    Solid* solid;
    int col;

};

class ViewGeometry : public TObject { 

  RQ_OBJECT("ViewGeometry")
  
  public:
    // Constructor
    ViewGeometry();
    // Destructor
    ~ViewGeometry();
    
    void SetCanvas(TCanvas* c);
    
    void SetGeometry(GeometrySimple* geo);
    
    void Plot();

    void EnableDebugging()  {debug = true;}
    void DisableDebugging() {debug = false;}

  protected:

    void Draw(Option_t* option);
    void Paint(Option_t* option);

  private:
   
    std::string className;
 
    // Options
    bool debug;

    std::string label;

    // Canvas
    TCanvas* canvas;
    bool hasExternalCanvas;
    
    GeometrySimple* geometry;

    int nShapes;
    std::vector<ViewGeometryShape> shapes;

    ClassDef(ViewGeometry, 0);

};

}
#endif
