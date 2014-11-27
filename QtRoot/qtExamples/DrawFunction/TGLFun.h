#ifndef STAR_TGLFun
#define STAR_TGLFun

#include <QGLViewer/qglviewer.h>
#include <vector>
#include "Rtypes.h"

class TF2;

class TGLFun 
{
   private:
      friend class TQtFunViewer;
      struct point3D { float x; float y; float z; };
      TF2 *fFun;
      Float_t fDx;
      Float_t fDy;
      Bool_t  fColZ;
      Int_t   fTop; // +1 light the "bottom" (external) surface 
                    // -1 light the "top" (internal) surface 
      Double_t fZColorScale; // Z color scale
      Double_t fFunMin;
      Double_t fFunMax;
      std::vector<point3D> fMeshpoints;
      std::vector<point3D> fMeshpointsFast;
      Int_t fNpx;  // Duplicate TF2 data-members
      Int_t fNpy;  // Duplicate TF2 data-members
      Bool_t  fGLListReady; // OpenGL list is ready
      Int_t   fFunList;     // OpenGL List
      double fRadius;       // scene radius
  protected :
    virtual void draw();
    void    drawFunction(bool fast=true);
    virtual void fastDraw();
    void    Normal(Float_t x,Float_t y, Float_t *normal) const;
    const point3D &Normal(int x,int y, Float_t *normal) const;
    Int_t   GetZColor(Double_t zc);
    void    InitFun(TF2 *fun);
    void    CalculateMinMax();
    const point3D &GetPoint(int x, int y) const;
    void  CreatGLShape(bool fast);
  public:
    TGLFun(TF2 *fun);
    virtual ~TGLFun();
    virtual Float_t Eval(Float_t x, Float_t y) const;
    double  Radius() const { return fRadius;} 
    const char* GetTitle() const;

    void SetFun(TF2 *fun);
    void SetColZ(Bool_t on=kTRUE);
    void SetTop(Bool_t on=kTRUE);
    void SetRange(double xmin,double ymin,double xmax,double ymax);
};
#endif
