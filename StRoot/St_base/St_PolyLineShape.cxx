#include "St_PolyLineShape.h"

#include "TPoints3DABC.h"
#include "St_PolyLine3D.h"
#include <TPolyLine3D.h>
#include <TPolyMarker3D.h>
#include "St_Node.h"
#include "St_NodePosition.h"
#include <TTUBE.h>
#include <TBRIK.h>
#include <TSPHE.h>
#include <TView.h>
#include <TPad.h>
#include <TPadView3D.h>
#include <TPoint.h>
#include <TPostScript.h>

#define MyInputPoints TPoints3DABC

ClassImp(St_PolyLineShape)

//______________________________________________________________________________
St_PolyLineShape::St_PolyLineShape()
{
   m_Shape = 0;
   m_Smooth = kFALSE;
   m_Connection= 0;
   m_Points=0;
   SetWidthFactor();
   m_HasDrawn = kFALSE;
   m_ShapeType = kNULL;
   m_SizeX3D   = 0;
   m_PointFlag = kFALSE;
   m_LineFlag  = kFALSE;
}

//______________________________________________________________________________
St_PolyLineShape::St_PolyLineShape(MyInputPoints  *points,Option_t* option)
{
   m_Shape       = new TTUBE("tube","tube","void",0.5,0.5);
   m_ShapeType   = kNULL;
   m_Smooth      = kFALSE;
   m_Connection  = 0;
   m_Points      = points;
   m_HasDrawn    = kFALSE;
   m_SizeX3D     = 0;
   // Take in account the current node if any   
   if (!m_Points) { 
     Error("St_PolyLineShape","No polyline is defined");
     return;
   }
   m_PointFlag = strchr(option,'P')?kTRUE:kFALSE;
   m_LineFlag  = strchr(option,'L')?kTRUE:kFALSE;

   SetWidthFactor();
   Create();
}

//______________________________________________________________________________
St_PolyLineShape::~St_PolyLineShape()
{
  SafeDelete(m_Shape);
  SafeDelete(m_SizeX3D);
}
//______________________________________________________________________________
void St_PolyLineShape::Axis(TVirtualPad *p, Float_t width)
{
#if 0
   TVirtualPad *pad = p;
   TVirtualPad *savpad = 0;
   if (pad && pad != gPad) {
      savpad = gPad;
      pad->cd();      
   }
   else 
     pad = gPad;
   
   TView *view = pad->GetView();
   if (view) {
      Float_t min[3];
      Float_t max[3];
      view->GetRange(min,max);
      MyInputPoints *lx = new MyInputPoints(2);
      lx->SetLineColor(6);
      lx->SetLineWidth(5);
      lx->SetPoint(0,0,0,0);
      lx->SetPoint(1,max[0],0,0);

      MyInputPoints *ly = new MyInputPoints(2);
      ly->SetLineColor(4);
      ly->SetLineWidth(5);
      ly->SetPoint(0,0,0,0);
      ly->SetPoint(1,0,max[1],0);

      MyInputPoints *lz = new MyInputPoints(2);
      lz->SetLineColor(2);
      lz->SetLineWidth(5);
      lz->SetPoint(0,0,0,0);
      lz->SetPoint(1,0,0,max[2]);

      St_PolyLineShape *lxview = new St_PolyLineShape(lx);
      St_PolyLineShape *lyview = new St_PolyLineShape(ly);
      St_PolyLineShape *lzview = new St_PolyLineShape(lz);

      lxview->SetWidthFactor(width);
      lyview->SetWidthFactor(width);
      lzview->SetWidthFactor(width);

      lxview->Draw();
      lyview->Draw();
      lzview->Draw();
   }
   if (savpad) savpad->cd();
#endif
 }   

//______________________________________________________________________________
void St_PolyLineShape::Create()
{
    if (!m_Connection) SetConnection(kBrik);
}

//______________________________________________________________________________
Size3D *St_PolyLineShape::CreateX3DSize(Bool_t marker)
{  
  if (!m_SizeX3D) m_SizeX3D = new Size3D; 
  m_SizeX3D->numPoints = 0;
  m_SizeX3D->numSegs   = 0;
  m_SizeX3D->numPolys  = 0;         //NOTE: Because of different structure, our
  if (m_Points) {
    Int_t size = m_Points->Size();
    if (marker) {
      Int_t mode;

      if (size > 10000) mode = 1;         // One line marker    '-'
      else if (size > 3000) mode = 2;     // Two lines marker   '+'
      else mode = 3;                      // Three lines marker '*'
 
      m_SizeX3D->numSegs   = size*mode;
      m_SizeX3D->numPoints = size*mode*2;
      m_SizeX3D->numPolys  = 0;
    }
    else {
      m_SizeX3D->numSegs   = size-1;
      m_SizeX3D->numPoints = size;
    }
    m_SizeX3D->numPolys  = 0;         //NOTE: Because of different structure, our
  }
  return m_SizeX3D;
}
//______________________________________________________________________________
Int_t St_PolyLineShape::SetConnection(EShapeTypes connection)
{
 Float_t size = 0.5*GetWidthFactor()*GetLineWidth();

 if (m_ShapeType != connection) {
   SafeDelete(m_Connection);
   m_ShapeType = connection;
   switch (m_ShapeType) {
     case  kSphere:
          SetConnection(new TSPHE("connection","sphere","void",0,size,0,90,0,360));
          break;
     default: 
          SetConnection(new TBRIK("connection","brik","void",size,size,size));
          break;
     };
  }
  return 0;
}

//______________________________________________________________________________
Int_t St_PolyLineShape::DistancetoPrimitive(Int_t px, Int_t py)
{
 if (m_Points) return m_Points->DistancetoPrimitive(px,py);
 return 999999;
}

//______________________________________________________________________________
void St_PolyLineShape::Draw(Option_t *opt)
{
  Create();
  AppendPad();
}

//______________________________________________________________________________
void St_PolyLineShape::ExecuteEvent(Int_t event, Int_t px, Int_t py)
{
 if (m_Points) m_Points->ExecuteEvent(event,px, py);
}

//______________________________________________________________________________
Color_t St_PolyLineShape::GetColorAttribute(){
  return GetLineColor();
}
//______________________________________________________________________________
Width_t St_PolyLineShape::GetSizeAttribute(){
  return GetLineWidth();
}

//______________________________________________________________________________
void St_PolyLineShape::PaintNode(Float_t *start,Float_t *end,Option_t *option)
{
  // Double_t *start - coordinate of the start point of the current segment
  // Double_t *end   - coordinate of the end   point of the current segment

  // Calculate the vector
   const Int_t kDimension = 3;
   Double_t vector[kDimension];
   Double_t nodeposition[kDimension]; 
   Int_t i=0;
   for (i=0;i<kDimension;i++) {
      vector[i]=end[i]-start[i];
      nodeposition[i]=0.5*(start[i]+end[i]);
   }
   Double_t length = TMath::Normalize(vector);

  // Calculate the rotation axis for Axis Oz

    Double_t Oz[3]={0,0,1};
    Double_t rotate[3];

    Double_t sina = TMath::Normalize(TMath::Cross(vector,Oz,rotate));
    Double_t cosa = Product(vector,Oz);
    Double_t mrot[3][3];
 
    TShape *shape = m_Shape;
    if (!shape) shape = m_Connection;

    gyrot(rotate,cosa,sina,mrot);
#ifdef LINES  
    Float_t width = GetWidthFactor()*(m_Points->GetLineWidth());
#else
//    Float_t width = GetWidthFactor()*(m_Points->GetMarkerSize());
      Float_t width = GetWidthFactor()*GetLineWidth();
//==      width  *= m_Points->GetSizeAttribute();
#endif

    mrot[0][0] *= width;
    mrot[0][1] *= width;
    mrot[0][2] *= width;

    mrot[1][0] *= width;
    mrot[1][1] *= width;
    mrot[1][2] *= width;

    mrot[2][0] *= length;
    mrot[2][1] *= length;
    mrot[2][2] *= length;

#ifdef LINES  
    Color_t color = m_Points->GetLineColor();
#else
    Color_t color = GetLineColor();
//    color = m_Points->GetColorAttribute();
//    Color_t color = m_Points->GetMarkerColor();
#endif

    St_Node node("SegmentNode","SegmentNode", shape);
    node.SetLineColor(color);
    if (!m_Shape) node.SetVisibility(2);
    node.SetLineColor(color);

    TRotMatrix matrix ("rotate","rotate",&mrot[0][0]);
    St_NodePosition position(&node,nodeposition[0],nodeposition[1]
                                  ,nodeposition[2],&matrix);

    if (!(m_Smooth || m_Connection))  {
         node.PaintNodePosition(option, &position);
         return;
    }

    // Add the connection

    memset(mrot,0,9*sizeof(Double_t));

    length = width/length;
    mrot[2][2] = length;
    mrot[0][0] = 1;
    mrot[1][1] = 1;

    TRotMatrix kneeMatrix("knee","knee",&mrot[0][0]);
    St_Node knee("ConnectionNode","ConnectionNode", m_Connection);
    St_NodePosition kneePosition(&knee, 0, 0, 0.5, &kneeMatrix);
    knee.SetLineColor(color);
    node.Add(&knee,&kneePosition);

    node.PaintNodePosition(option, &position);
}

//______________________________________________________________________________
void St_PolyLineShape::Paint(Option_t *opt)
{
  if (!m_Points) return;

  Bool_t rangeView = opt && opt[0] && strcmp(opt,"range")==0 ? kTRUE : kFALSE;
  TPadView3D *view3D = 0;
  if (!rangeView  && (view3D = gPad->GetView3D()) ) {
    TString mode;
    mode="";
    view3D->SetLineAttr(GetColorAttribute(), GetSizeAttribute());
    if (m_LineFlag)  mode  = "L";
    if (m_PointFlag) mode += "P";    
    view3D->PaintPoints3D(GetPoints(), mode.Data());
  }
  if (!strstr(opt, "x3d")) {
    if (m_PointFlag) {
         SetMarkerColor(GetColorAttribute());
         SetMarkerSize(GetSizeAttribute());
         PaintPolyMarker(m_Points->Size());
    }
    if (m_LineFlag) {
         SetLineColor(GetColorAttribute());
         SetLineWidth(GetSizeAttribute());
         PaintPoints(m_Points->Size());
    }

  }
  else {
    if (m_LineFlag) {
       CreateX3DSize(kFALSE); PaintX3DLine(opt);
    } else {
       CreateX3DSize(kTRUE);  PaintX3DMarker(opt);
    }
//     Paint3d(opt);
  }
}

//______________________________________________________________________________
void  St_PolyLineShape::PaintPoints(Int_t n, Float_t *, Option_t *)
{
//*-*-*-*-*-*-*-*-*Draw this 3-D polyline with new coordinates*-*-*-*-*-*-*-*-*-*
//*-*              ===========================================
   if (n < 2) return;
 
   TAttLine::Modify();  //Change line attributes only if necessary
 
//*-*- Loop on each individual line
 
   for (Int_t i=1;i<n;i++) {
      Float_t xyz[6];
      m_Points->GetXYZ(xyz,i-1,2);
      gPad->PaintLine3D(xyz, &xyz[3]);
   }
}

//______________________________________________________________________________
void St_PolyLineShape::PaintPolyMarker(Int_t n, Float_t *, Marker_t, Option_t *)
{
//*-*-*-*-*-*-*-*-*Paint polymarker in CurrentPad World coordinates*-*-*-*-*-*-*-*
//*-*              ================================================
 
   if (n <= 0) return;
 
   //Create temorary storage
   TPoint *pxy = new TPoint[n];
   Float_t *x  = new Float_t[n];
   Float_t *y  = new Float_t[n];
   Float_t xndc[3], ptr[3];
   
 
   TView *view = gPad->GetView();      //Get current 3-D view
   if(!view) return;                   //Check if `view` is valid
 
//*-*- convert points from world to pixel coordinates
   Int_t nin = 0;
   for (Int_t i = 0; i < n; i++) {
      m_Points->GetXYZ(ptr,i);
      view->WCtoNDC(ptr, xndc);
      if (xndc[0] < gPad->GetX1() || xndc[0] > gPad->GetX2()) continue;
      if (xndc[1] < gPad->GetY1() || xndc[1] > gPad->GetY2()) continue;
      x[nin] = xndc[0];
      y[nin] = xndc[1];
      pxy[nin].fX = gPad->XtoPixel(x[nin]);
      pxy[nin].fY = gPad->YtoPixel(y[nin]);
      nin++;
   }
 
   TAttMarker::Modify();  //Change marker attributes only if necessary
 
//*-*- invoke the graphics subsystem
   if (!gPad->IsBatch()) gGXW->DrawPolyMarker(nin, pxy);
 
 
   if (gCurrentPS) {
      gCurrentPS->DrawPolyMarker(nin, x, y);
   }
   delete [] x;
   delete [] y;
 
   delete [] pxy;
}

//______________________________________________________________________________
void St_PolyLineShape::Paint3d(Option_t *opt)
{
 if (!m_Points) return;

 Create();

 struct XYZ { Float_t xyz[3]; } *points;
 points  = (XYZ *)(m_Points->GetP());
 Int_t size      = m_Points->GetN()-1;
  
 for (Int_t i=0;i<size;i++) 
      PaintNode((Float_t *)(points+i+1),(Float_t *)(points+i),opt);      
 m_HasDrawn = kTRUE;
}

//______________________________________________________________________________
void St_PolyLineShape::PaintX3DLine(Option_t *opt)
{
#ifndef WIN32
   X3DBuffer *buff = new X3DBuffer;
   if (!buff) return;

   Int_t size = 0;
   if (m_Points) size = m_Points->Size();
   if (!size) return;

   m_SizeX3D->numPoints = buff->numPoints = size;
   m_SizeX3D->numSegs   = buff->numSegs   = size-1;
   m_SizeX3D->numPolys  = buff->numPolys  = 0;        //NOTE: Because of different structure, our

   buff->polys     = NULL;     //      St_PolyLine3D can't use polygons
   St_Points3D x3dPoints(size);
   buff->points    = m_Points->GetXYZ(x3dPoints.GetP(),0,size);
 
//        Int_t c = (((fAttributes?fAttributes->GetColorAttribute():0) % 8) - 1) * 4;     // Basic colors: 0, 1, ... 8
   Int_t c = ((GetColorAttribute() % 8) - 1) * 4;     // Basic colors: 0, 1, ... 8
   if (c < 0) c = 0;
 
    //*-* Allocate memory for segments *-*
    buff->segs = new Int_t[buff->numSegs*3];
    if (buff->segs) {
         for (Int_t i = 0; i < buff->numSegs; i++) {
             buff->segs[3*i  ] = c;
             buff->segs[3*i+1] = i;
             buff->segs[3*i+2] = i+1;
         }
     }
 
 
     if (buff && buff->points && buff->segs) //If everything seems to be OK ...
         FillX3DBuffer(buff);
     else {                            // ... something very bad was happened
         gSize3D.numPoints -= buff->numPoints;
         gSize3D.numSegs   -= buff->numSegs;
         gSize3D.numPolys  -= buff->numPolys;
     }
 
     if (buff->segs)     delete [] buff->segs;
     if (buff->polys)    delete [] buff->polys;
     if (buff)           delete    buff;
#endif
}
//______________________________________________________________________________
void St_PolyLineShape::PaintX3DMarker(Option_t *opt)
{
#ifndef WIN32
   Int_t size = 0;
   if (m_Points) size = m_Points->Size();
   if (!size) return;
   Int_t mode;
   Int_t i, j, k, n;
 
   X3DBuffer *buff = new X3DBuffer;
   if(!buff) return;
 
   if (size > 10000) mode = 1;         // One line marker    '-'
   else if (size > 3000) mode = 2;     // Two lines marker   '+'
   else mode = 3;                      // Three lines marker '*'

   m_SizeX3D->numSegs   = buff->numSegs   = size*mode;
   m_SizeX3D->numPoints = buff->numPoints = buff->numSegs*2;
   m_SizeX3D->numPolys  = buff->numPolys  = 0;         //NOTE: Because of different structure, our

   buff->polys     = NULL;      //      TPolyMarker3D can't use polygons
 
 
    //*-* Allocate memory for points *-*
   Float_t delta = 0.002;
 
   buff->points = new Float_t[buff->numPoints*3];
   if (buff->points) {
       for (i = 0; i < size; i++) {
           for (j = 0; j < mode; j++) {
               for (k = 0; k < 2; k++) {
                   delta *= -1;
                   for (n = 0; n < 3; n++) {
                       Float_t xyz[3];
                       m_Points->GetXYZ(xyz,i);
                       buff->points[mode*6*i+6*j+3*k+n] = 
                           xyz[n] * (1 + (j == n ? delta : 0));
                   }
               }
           }
       }
   }
 
   Int_t c = ((GetColorAttribute() % 8) - 1) * 4;     // Basic colors: 0, 1, ... 8
   if (c < 0) c = 0;
 
    //*-* Allocate memory for segments *-*
   buff->segs = new Int_t[buff->numSegs*3];
   if (buff->segs) {
       for (i = 0; i < buff->numSegs; i++) {
           buff->segs[3*i  ] = c;
           buff->segs[3*i+1] = 2*i;
           buff->segs[3*i+2] = 2*i+1;
       }
   }
 
   if (buff->points && buff->segs)    //If everything seems to be OK ...
       FillX3DBuffer(buff);
   else {                            // ... something very bad was happened
       gSize3D.numPoints -= buff->numPoints;
       gSize3D.numSegs   -= buff->numSegs;
       gSize3D.numPolys  -= buff->numPolys;
   }
 
   if (buff->points)   delete [] buff->points;
   if (buff->segs)     delete [] buff->segs;
   if (buff->polys)    delete [] buff->polys;
   if (buff)           delete    buff;
#endif
}

//______________________________________________________________________________
Float_t St_PolyLineShape::Product(Float_t *v1, Float_t *v2,Int_t ndim)
{
  Float_t p = 0;
  if (v1 && v2 && ndim > 0) 
    for (Int_t i=0; i<ndim; i++) p+= v1[i]*v2[i];
  return p;
}

//______________________________________________________________________________
Double_t St_PolyLineShape::Product(Double_t *v1, Double_t *v2,Int_t ndim)
{
  Double_t p = 0;
  if (v1 && v2 && ndim > 0) 
    for (Int_t i=0;i<ndim;i++) p+= v1[i]*v2[i];
  return p;
}

//______________________________________________________________________________
Double_t *St_PolyLineShape::gyrot(Double_t *dirc, Double_t cosang, Double_t sinang, Double_t trans[3][3])
{
//************************************************************************
//*                                                                      *
//*   call gyrot(dirc,angp,trans,ntrans)                       vp 880722 *
//*                                       revised              vp 921009 *
//*                                       revised (f->c++)     vf 981006 *
//*       routine for filling rotation transformation matrix             *
//*       from axis and rotation angle around                            *
//*                                                                      *
//*   arguments:                                                         *
//*       dirc    direct cosinuses (may be not normalised)               *
//*       cosang, sinang - cos and sin of the rotation angle             *
//*       tranz   rotation & shift matrix 3*3  (input/output)            *
//*                                                                      *
//************************************************************************

      Double_t ax[3];
 
      memcpy(ax,dirc,3*sizeof(Double_t));
      TMath::Normalize(ax);      
     
      Double_t ca  = cosang;
      Double_t sa  = sinang;
      Double_t ca1;

      if (ca < 0.5)
        ca1 = 1. - ca ;
      else
        ca1 = (sa*sa)/(1.+ca) ;

      Int_t j1 = 0;
      Int_t j2 = 0;
      for(j1 = 0; j1 < 3; j1++) {
        for(j2 = 0; j2 < 3; j2++)
              trans[j1][j2] = ca1*ax[j1]*ax[j2];
        trans[j1][j1]   += ca;
      }
 
      trans[0][1] = trans[0][1] - sa*ax[2];
      trans[1][0] = trans[1][0] + sa*ax[2];
      trans[0][2] = trans[0][2] + sa*ax[1];
      trans[2][0] = trans[2][0] - sa*ax[1];
      trans[1][2] = trans[1][2] - sa*ax[0];
      trans[2][1] = trans[2][1] + sa*ax[0];
     
      return (Double_t *)trans;

}

//______________________________________________________________________________
Color_t St_PolyLineShape::SetColorAttribute(Color_t color)
{
  Color_t currentColor = GetColorAttribute();
  if (color != currentColor) 
  {
    SetLineColor(color);
    SetMarkerColor(color);
  }
  return currentColor;
}
//______________________________________________________________________________
Width_t St_PolyLineShape::SetSizeAttribute(Width_t size)
{
  Width_t currentSize = GetSizeAttribute();
  if (size != currentSize) 
  {
    SetLineWidth(size);
    SetMarkerSize(size);
  }
  return currentSize;
}

//______________________________________________________________________________
void St_PolyLineShape::SetShape(TShape *shape)
{ 
   SafeDelete(m_Shape)
   m_Shape = shape; 
}

//______________________________________________________________________________
void St_PolyLineShape::Sizeof3D() const
{
//*-*-*-*-*-*-*Return total X3D size of this shape with its attributes*-*-*-*-*-*
//*-*          =======================================================
  St_PolyLineShape *line = (St_PolyLineShape *)this;
  if (m_LineFlag )
    line->CreateX3DSize(kFALSE); 
  else
    line->CreateX3DSize(kTRUE); 
  if (m_SizeX3D) {
     gSize3D.numPoints += m_SizeX3D->numPoints;
     gSize3D.numSegs   += m_SizeX3D->numSegs;
     gSize3D.numPolys  += m_SizeX3D->numPolys;
  }
  else Error("Sizeof3D()","buffer size has not been defined yet");
}
