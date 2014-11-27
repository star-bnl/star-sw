#include "TQtFunViewer.h"
#include "TMath.h"
#include "TF2.h"
#include "TROOT.h"
#include "TColor.h"
#include <QDebug>
#include <TColor.h>
#include <TStyle.h>
#include <QColor>
#include <QString>

using namespace std;

//______________________________________________________________________________
Float_t TQtFunViewer::Eval(Float_t x, Float_t y) const 
{
   // you may want to reimplemnt this method to be able  to define
   // the function differently
   return fFun ? fFun->Eval(x,y) : 0; 
}
//______________________________________________________________________________
TQtFunViewer::TQtFunViewer(TF2 *fun,QWidget *parent) : QGLViewer(parent),fFun(fun), fDx(0),fDy(0),fColZ(kFALSE),fTop(1)
, fZColorScale(0),fFunMin(-1956),fFunMax(1956),fGLListReady(false),fFunList(-1)
{
   InitFun(fun);
}
//______________________________________________________________________________
TQtFunViewer::~TQtFunViewer() {
   // Free GL list
    if (glIsList(fFunList)) glDeleteLists(fFunList,1);
}

//______________________________________________________________________________
const TQtFunViewer::point3D &TQtFunViewer::GetPoint(int x, int y) const
{
   // return point for cell [x,y]
   int indx = x*(fNpy+2) + y;
   return fMeshpoints[indx];
}

//______________________________________________________________________________
void    TQtFunViewer::InitFun(TF2 *fun)
{
   fFun = fun;
   QString title = "ROOT Function Viewer - NO function";
   if (fFun) {
     fGLListReady = false;
     CalculateMinMax();
     title   = fFun->GetTitle();
     if (fColZ) title += ": Colored";
     Int_t ncolors = gStyle->GetNumberOfColors();
     fZColorScale  = ncolors/TMath::Abs(fFunMax - fFunMin);
  } else {
     fZColorScale = 0;
  }
  setWindowTitle(title);
}
//______________________________________________________________________________
void     TQtFunViewer::CalculateMinMax()
{
   // TF2::GetMinimum(); TF2::GetMaximum() - provdes the wrong result
   // we have to calculate outselves
  Double_t xmin, ymin, xmax, ymax;
  fFun->GetRange(xmin, ymin, xmax, ymax) ;
  Int_t npx = fNpx = fFun->GetNpx();
  Int_t npy = fNpy = fFun->GetNpy();
  fMeshpoints.clear(); fMeshpoints.reserve((npx+2)*(npy+2));
  Double_t dx = fDx = (xmax - xmin)/Double_t(npx);
  Double_t dy = fDy = (ymax - ymin)/Double_t(npy); 
//  fFun->InitArgs(xv,fParams);
   int counter  =0;
   Double_t x0 = xmin-dx;
   for (int i=0;i<=npx+1;i++,x0+=dx) {
      Double_t y0 = ymin-dy;
      for (int j=0;j<=npy+1;j++,y0+=dy,counter++) {
         Double_t z0 = Eval(x0,y0);
         if (!counter) fFunMax = fFunMin = z0;
         if      (z0 > fFunMax)  fFunMax  = z0;
         else if (z0 < fFunMin)  fFunMin  = z0;
         point3D vertex = {x0,y0,z0};
         fMeshpoints.push_back(vertex);
      }
   }
   setSceneRadius (0.5 *
        TMath::Sqrt((fFunMax-fFunMin)*(fFunMax-fFunMin) 
         + (ymax-ymin)*(ymax-ymin) + (xmax-xmin)*(xmax-xmin)) 
        );
}

//______________________________________________________________________________
void TQtFunViewer::SetFun(TF2 *fun)
{
   InitFun(fun);
   update();
}
//_____________________________________________________________________________
inline Int_t TQtFunViewer::GetZColor(Double_t zc)
{
   Int_t theColor;
   Double_t wlmin  = fFunMin;
   theColor =Int_t(fZColorScale*TMath::Abs(zc-wlmin));
   return gStyle->GetColorPalette(theColor);
}
// Calculate the normal at the point
//________________________________________________________________________
inline const TQtFunViewer::point3D &TQtFunViewer::Normal(int x,int y, Float_t *normal) const
{
  Float_t nor[3] = {0,0,0};
  const point3D &v = GetPoint(x,y);
  const point3D *vxu[4]= {  &GetPoint(x+1,y  )
                          , &GetPoint(x,  y+1)
                          , &GetPoint(x-1,y  )
                          , &GetPoint(x,  y-1)
                         };
  memset(normal,0,sizeof(nor));
  for (int i=0; i<4;i++ ) {
    int k = i<3 ? i+1 : 0;
    TMath::Normal2Plane(&v.x,&(*vxu)[k].x,&(*vxu)[i].x,nor);
    TMath::Normalize(nor);
    for (int j =0; j<3;j++) normal[i] = (normal[i]+nor[i])/2;
  }
  TMath::Normalize(normal);
  if (fTop < 0) for (int j =0; j<3;j++) normal[j] = -normal[j];
  return v;
}
// Calculate the normal at the point
//________________________________________________________________________
inline void TQtFunViewer::Normal(Float_t x,Float_t y, Float_t *normal) const
{
  Float_t nor[3] = {0,0,0};
  Float_t v[3] =         {x,    y,    Eval(x,    y    )};
  Float_t vxu[4][3] = {  {x+fDx,y,    Eval(x+fDx,y    )}
                       , {x,    y+fDy,Eval(x,    y+fDy)}
                       , {x-fDx,y,    Eval(x-fDx,y    )}
                       , {x,    y-fDy,Eval(x,    y-fDy)}
                      };
  memset(normal,0,sizeof(nor));
  for (int i=0; i<4;i++ ) {
    int k = i<3 ? i+1 : 0;
    TMath::Normal2Plane(v,vxu[k],vxu[i],nor);
    TMath::Normalize(nor);
    for (int j =0; j<3;j++) normal[i] = (normal[i]+nor[i])/2;
  }
  TMath::Normalize(normal);
  if (fTop < 0) for (int j =0; j<3;j++) normal[j] = -normal[j];
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
   if (!fFun) return;
   if (!fGLListReady) {
      // Prepare the OpenGL lost
      fGLListReady = !fGLListReady;
      if (!glIsList(fFunList)) fFunList = glGenLists(1);
      glNewList(fFunList,GL_COMPILE_AND_EXECUTE);

      int stepx = fast ? 3:1;
      int stepy = fast ? 3:1;
      float red,green,blue;
      if (!fColZ) {
         Color_t rootColor = fFun->GetFillColor();
         gROOT->GetColor(rootColor)->GetRGB(red,green,blue);
         glColor3f(red,green,blue);
      }
      Float_t normal[3];
      for (int i=1;i<fNpx;i+=stepx) {
         Int_t currentColor=-1;
         glBegin(GL_QUAD_STRIP);
         for (int j=1;j<fNpy;j+=stepy) {
            const point3D &x0 = GetPoint(i,j);
            Normal(i, j, normal);
            if (fColZ) {
               int thisColor = GetZColor(x0.z);
               if (thisColor != currentColor) {
                  currentColor = GetZColor(x0.z);
                  gROOT->GetColor(currentColor)->GetRGB(red,green,blue);
                  glColor3f(red,green,blue);
               }
            }
            glNormal3f(normal[0], normal[1], normal[2]);
            glVertex3f(x0.x,x0.y,x0.z);
            Normal(i+1, j, normal);
            const point3D &x1 = GetPoint(i+1,j);
            if (fColZ) {
               int thisColor = GetZColor(x1.z);
               if (thisColor != currentColor) {
                  currentColor = thisColor;
                  gROOT->GetColor(currentColor)->GetRGB(red,green,blue);
                  glColor3f(red,green,blue);
               }
            }
            glNormal3f(normal[0], normal[1], normal[2]);
            glVertex3f(x1.x,x1.y,x1.z);
         }
         glEnd();
      }
      glEndList();
   } else {
      glCallList(fFunList);
   }
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
void TQtFunViewer::SetColZ(Bool_t on)
{ 
   // Use the ROOT palette to show the Z-values [slot]
   if ((fColZ != on) && fFun) {
      // Change the title 
      QString title =  fFun->GetTitle();
      if (fColZ) title += ": Colored";
      setWindowTitle(title);
   }
   fColZ = on; 
   update(); 
} 
//________________________________________________________________________
void TQtFunViewer::SetTop(Bool_t on) 
{
   // [slot]
   fTop = on ? -1: +1; 
   update(); 
}
