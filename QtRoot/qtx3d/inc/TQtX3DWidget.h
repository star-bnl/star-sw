#ifndef QTROOT_TQtX3DWidget
#define QTROOT_TQtX3DWidget

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtX3DWidget                                                         //
//                                                                      //
// Qt Widget to control X3D X11 window (for example by TQtViewerX3D).   //
//                                                                      //
// It can be used to insert x3d view into any QWidget                   //
// The X3D graphics goes into this widget.                              //
// This class is used to enable input events on this graphics           //
// widget and forward the events to X3D.                                //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef __CINT__
#include "Rtypes.h" 
#include "qwidget.h" 

class TQtX3DWidget : public QWidget {
  Q_OBJECT   
  protected:   // data-members
     Long_t         fX3DWin;    // child x3d X11 widget

  protected:   // methods
     virtual void mouseReleaseEvent(QMouseEvent *ev);
     virtual void paintEvent ( QPaintEvent * );
     virtual void resizeEvent ( QResizeEvent * );

     virtual bool x11Event(XEvent *ev);

  public:
     TQtX3DWidget(Float_t *longitude, Float_t *latitude, Float_t *psi,QWidget *p=0,Option_t *option=0);
    ~TQtX3DWidget();
  public slots:
     virtual int execCommand(int px, int py, char command);
  signals:
     void x3dPosition(float longitude_rad, float latitude_rad, float psi_rad);
};
#endif
#endif
