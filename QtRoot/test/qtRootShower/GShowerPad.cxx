#include "GShowerPad.h"
//#include <qhbuttongroup.h> 
#include <Q3HButtonGroup>
#include <qpushbutton.h> 
#include <qwidget.h> 
#include <qtooltip.h>
#include "TQtWidget.h"
#include "TCanvas.h"
#include <TView.h>

#if  ROOT_VERSION_CODE >= ROOT_VERSION(4,03,3)   
#  include "TVirtualViewer3D.h"
#  include "TGQt.h"
#endif

//______________________________________________________________________________
GShowerPad::GShowerPad(QWidget *parent, const char * name, Qt::WFlags f ):
Q3VBox(parent,name,f) 
{
   
   setMargin(2);
   // add pad
   cA = new TQtWidget(this,"EmbeddedCanvas");
   cA->setSizePolicy(QSizePolicy::Expanding,QSizePolicy::Expanding);

   cA->GetCanvas()->SetBorderMode(0);
   cA->GetCanvas()->SetFillColor(1);


   //add the button group
   Q3HButtonGroup  *group = new Q3HButtonGroup(this,"zoom");
   // First button
   QPushButton *button = new QPushButton("&Zoom Forward",group);
   QToolTip::add(button,"Zoom forward event view");
   connect(button,SIGNAL(clicked ()),this,SLOT(PadZoomForward()));

   // Second "Backwad" button
   button = new QPushButton("&Zoom &Backward",group);
   QToolTip::add(button,"Zoom backward event view");
   connect(button,SIGNAL(clicked ()),this,SLOT(PadZoomBackward()));
}
//______________________________________________________________________________
TCanvas *GShowerPad::GetCanvas()
{ return cA->GetCanvas(); }

//      Slots:  case M_ZOOM_PLUS:
//______________________________________________________________________________
void GShowerPad::PadZoomForward(){
   TCanvas *c = cA->GetCanvas();
   c->cd();
   c->GetView()->ZoomView(0, 1.25);
   c->Modified();
   c->Update();
}
//______________________________________________________________________________
void GShowerPad::PadZoomBackward()
{
   TCanvas *c = cA->GetCanvas();
   c->cd();
   c->GetView()->UnzoomView(0, 1.25);
   c->Modified();
   c->Update();
}
//______________________________________________________________________________
void GShowerPad::Show3D()
{
   TCanvas *c = cA->GetCanvas();
#if  ROOT_VERSION_CODE >= ROOT_VERSION(4,03,03)   
         TVirtualViewer3D *viewer = TVirtualViewer3D::Viewer3D(c,"ogl");
         if (viewer) {
            // Create Open GL viewer
            TGQt::SetCoinFlag(0);
            viewer->BeginScene();
          viewer->EndScene();
         }
#else   
  c->x3d("OPENGL");
#endif
}
