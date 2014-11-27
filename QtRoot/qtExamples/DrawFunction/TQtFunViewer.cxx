#include "TQtFunViewer.h"
#include <QDebug>
#include <QColor>
#include <QString>
#include "TGLFun.h"

using namespace std;

//______________________________________________________________________________
TQtFunViewer::TQtFunViewer(TF2 *fun,QWidget *parent) : QGLViewer(parent)
,fColZ(kFALSE),fRadius(0)
{
   InitFun(fun);
}
//______________________________________________________________________________
TQtFunViewer::~TQtFunViewer() {
   Clear();
}
//______________________________________________________________________________
void    TQtFunViewer::InitFun(TF2 *fun)
{
   QString title = "ROOT Function Viewer";
   if (fun) {
      TGLFun *nextFun = new TGLFun(fun);
      fRadius = max(fRadius,nextFun->Radius());
      fFuns.push_back(nextFun);
      setSceneRadius(fRadius);
   }
   setWindowTitle(title);
}

//______________________________________________________________________________
void TQtFunViewer::SetFun(TF2 *fun)
{
   InitFun(fun);
}
//______________________________________________________________________________
void TQtFunViewer::SetFun(TF2 *fun,double xmin, double xmax, double ymin, double ymax)
{
   std::list<TGLFun*>::iterator i = fFuns.begin();
   for (;i != fFuns.end(); i++ )  { 
       fRadius = 0;
       (*i)->SetRange(xmin,ymin,xmax,ymax);
       fRadius = max(fRadius,(*i)->Radius());
   }
   if (fun) InitFun(fun);
   else setSceneRadius(fRadius);
}
//________________________________________________________________________
void TQtFunViewer::draw()
{
  drawFunction(false);
}
//________________________________________________________________________
void TQtFunViewer::fastDraw()
{
  drawFunction(true);
}
// Draws a function
//________________________________________________________________________
inline void TQtFunViewer::drawFunction(bool fast)
{
   std::list<TGLFun*>::iterator i = fFuns.begin();
   for (;i != fFuns.end(); i++ )  (*i)->drawFunction(fast);
}

//________________________________________________________________________
void TQtFunViewer::init()
{
  // Increase the material shininess
  glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 50.0);
  GLfloat specular_color[4] = { 0.8f, 0.8f, 0.8f, 1.0 };
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR,  specular_color);
  // Restore previous viewer state.
  restoreStateFromFile();
  // Opens help window
//  help();
}

//________________________________________________________________________
QString TQtFunViewer::helpString() const
{
  QString text("<h2>R O O T   F u n c t i o n   V i e w e r</h2>");
  text += "Use the mouse to move the camera around the object. ";
  text += "You can respectively revolve around, zoom and translate with the three mouse buttons. ";
  text += "Left and middle buttons pressed together rotate around the camera view direction axis<br><br>";
  text += "Pressing <b>Alt</b> and one of the function keys (<b>F1</b>..<b>F12</b>) defines a camera keyFrame. ";
  text += "Simply press the function key again to restore it. Several keyFrames define a ";
  text += "camera path. Paths are saved when you quit the application and restored at next start.<br><br>";
  text += "Press <b>F</b> to display the frame rate, <b>A</b> for the world axis, ";
  text += "<b>Alt+Return</b> for full screen mode and <b>Control+S</b> to save a snapshot. ";
  text += "See the <b>Keyboard</b> tab in this window for a complete shortcut list.<br><br>";
  text += "Double clicks automates single click actions: A left button double click aligns the closer axis with the camera (if close enough). ";
  text += "A middle button double click fits the zoom of the camera and the right button re-centers the scene.<br><br>";
  text += "A left button double click while holding right button pressed defines the camera <i>Revolve Around Point</i>. ";
  text += "See the <b>Mouse</b> tab and the documentation web pages for details.<br><br>";
  text += "Press <b>Escape</b> to exit the viewer.";
  return text;
}
//________________________________________________________________________
void TQtFunViewer::Clear()
{
   hide();
   std::list<TGLFun*>::iterator i = fFuns.begin();
   for (;i != fFuns.end(); i++ ) delete *i;
   fFuns.clear();
}

//________________________________________________________________________
void TQtFunViewer::Reset()
{
   Clear();
}

//________________________________________________________________________
void TQtFunViewer::SetColZ(Bool_t on)
{ 
   // Use the ROOT palette to show the Z-values [slot]
   if (!fFuns.empty()) {
      // Change the title 
      QString title =  fFuns.front()->GetTitle();
      if (fColZ) title += ": Colored";
      setWindowTitle(title);
   }
   fColZ = on;
   std::list<TGLFun*>::iterator i = fFuns.begin();
   for (;i != fFuns.end(); i++ ) (*i)->SetColZ(on);
} 
//________________________________________________________________________
void TQtFunViewer::SetTop(Bool_t on) 
{
   // [slot]
   std::list<TGLFun*>::iterator i = fFuns.begin();
   for (;i != fFuns.end(); i++ ) (*i)->SetTop(on);
}
