#ifndef STAR_TQtFunViewer
#define STAR_TQtFunViewer

#include <QGLViewer/qglviewer.h>
#include <vector>
#include "Rtypes.h"

class TF2;
class TGLFun;

class TQtFunViewer : public QGLViewer
{
   Q_OBJECT
   private:
      Bool_t  fColZ;
      std::list<TGLFun*> fFuns;      
      double fRadius;       // scene radius
   protected :
    virtual void draw();
    void    drawFunction(bool fast=true);
    virtual void fastDraw();
    virtual void init();
    virtual QString helpString() const;
    Int_t   GetZColor(Double_t zc);
    void    InitFun(TF2 *fun);
    void    Clear();
  public:
    TQtFunViewer(TF2 *fun,QWidget *parent=0);
    virtual ~TQtFunViewer();
  public slots:
    void Reset();
    void SetFun(TF2 *fun);
    void SetFun(TF2 *fun,double xmin, double xmax, double ymin, double ymax);
    void SetColZ(Bool_t on=kTRUE);
    void SetTop(Bool_t on=kTRUE);
};
#endif
