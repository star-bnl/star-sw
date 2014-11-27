#include "TGLFun.h"
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
Float_t TGLFun::Eval(Float_t x, Float_t y) const 
{
   // you may want to reimplemnt this method to be able  to define
   // the function differently
   return fFun ? fFun->Eval(x,y) : 0; 
}
//______________________________________________________________________________
TGLFun::TGLFun(TF2 *fun): fFun(fun), fDx(0),fDy(0),fColZ(kFALSE),fTop(1)
, fZColorScale(0),fFunMin(-1956),fFunMax(1956),fGLListReady(false),fFunList(-1)
, fRadius(0)
{
   InitFun(fun);
}
//______________________________________________________________________________
TGLFun::~TGLFun() {
   // delete fFun;fFun = 0;
    // Free GL list
    if (glIsList(fFunList)) glDeleteLists(fFunList,1);
}

//______________________________________________________________________________
const TGLFun::point3D &TGLFun::GetPoint(int x, int y) const
{
   // return point for cell [x,y]
   int indx = x*(fNpy+2) + y;
   return fMeshpoints[indx];
}

//______________________________________________________________________________
void    TGLFun::InitFun(TF2 *fun)
{
   fFun = fun;
   if (fFun) {
     fGLListReady = false;
     CalculateMinMax();
     Int_t ncolors = gStyle->GetNumberOfColors();
     fZColorScale  = ncolors/TMath::Abs(fFunMax - fFunMin);
  } else {
     fZColorScale = 0;
  }
}
//______________________________________________________________________________
void   TGLFun::CalculateMinMax()
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
   fRadius = (0.5 *
               TMath::Sqrt((fFunMax-fFunMin)*(fFunMax-fFunMin) 
               + (ymax-ymin)*(ymax-ymin) + (xmax-xmin)*(xmax-xmin)) 
              );
}
//______________________________________________________________________________
void TGLFun::SetRange(double xmin,double ymin,double xmax,double ymax)
{
    // reset the function range
   if (fFun) {
      fFun->SetRange(xmin,ymin,xmax,ymax);
      InitFun(fFun);
   }
}

//______________________________________________________________________________
void TGLFun::SetFun(TF2 *fun)
{
   InitFun(fun);
}
//_____________________________________________________________________________
inline Int_t TGLFun::GetZColor(Double_t zc)
{
   Int_t theColor;
   Double_t wlmin  = fFunMin;
   theColor =Int_t(fZColorScale*TMath::Abs(zc-wlmin));
   return gStyle->GetColorPalette(theColor);
}
// Calculate the normal at the point
//________________________________________________________________________
inline const TGLFun::point3D &TGLFun::Normal(int x,int y, Float_t *normal) const
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
inline void TGLFun::Normal(Float_t x,Float_t y, Float_t *normal) const
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
void TGLFun::draw()
{
  drawFunction(false);
}
//________________________________________________________________________
void TGLFun::fastDraw()
{
  drawFunction(true);
}

// Create GL "definmtion of the function
//________________________________________________________________________
void  TGLFun::CreatGLShape(bool fast)
{
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
}

// Draws a function
//________________________________________________________________________
void TGLFun::drawFunction(bool fast)
{
   if (!fFun) return;
   if (!fGLListReady) {
      // Prepare the OpenGL lost
      fGLListReady = !fGLListReady;
      if (!glIsList(fFunList)) fFunList = glGenLists(1);
      glNewList(fFunList,GL_COMPILE_AND_EXECUTE);
      CreatGLShape(fast);
#if 0
      // test this later !!!
      // invert the property and redraw the internal portion
      int top = fTop;   fTop = -fTop;
      bool col = fColZ; fColZ = !fColZ;
      CreatGLShape(fast);
      // restore the properties
      fTop = top; fColZ = col;
#endif
      glEndList();
   } else {
      glCallList(fFunList);
   }
}

//________________________________________________________________________
void TGLFun::SetColZ(Bool_t on)
{ 
   // Use the ROOT palette to show the Z-values [slot]
   if ((fColZ != on) && fFun) {
      // Change the title 
      QString title =  fFun->GetTitle();
      if (fColZ) title += ": Colored";
   }
   fColZ = on; 
} 
//________________________________________________________________________
void TGLFun::SetTop(Bool_t on) 
{
   // [slot]
   fTop = on ? -1: +1; 
}

//________________________________________________________________________
const char* TGLFun::GetTitle() const
{
    return (fFun) ? fFun->GetTitle() : "no function";
}
