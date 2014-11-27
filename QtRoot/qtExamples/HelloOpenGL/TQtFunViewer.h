#ifndef STAR_TQtFunViewer
#define STAR_TQtFunViewer

#include <QGLViewer/qglviewer.h>
#include <vector>
#include "Rtypes.h"

class TF2;

class TQtFunViewer : public QGLViewer
{
   Q_OBJECT
   private:
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
  protected :
    virtual void draw();
    void    drawFunction(bool fast=true);
    virtual void fastDraw();
    virtual void init();
    virtual QString helpString() const;
    void    Normal(Float_t x,Float_t y, Float_t *normal) const;
    const point3D &Normal(int x,int y, Float_t *normal) const;
    Int_t   GetZColor(Double_t zc);
    void    InitFun(TF2 *fun);
    void    CalculateMinMax();
    const point3D &GetPoint(int x, int y) const;
  public:
    TQtFunViewer(TF2 *fun,QWidget *parent=0);
    virtual ~TQtFunViewer();
    virtual Float_t Eval(Float_t x, Float_t y) const;
  public slots:
    void SetFun(TF2 *fun);
    void SetColZ(Bool_t on=kTRUE);
    void SetTop(Bool_t on=kTRUE);
};
#endif
