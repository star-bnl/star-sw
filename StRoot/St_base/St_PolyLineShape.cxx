#include <TPolyLine3D.h>
#include "St_Node.h"
#include "St_NodePosition.h"
#include <TTUBE.h>
#include <TBRIK.h>
#include <TSPHE.h>
#include <TView.h>
#include <TPad.h>

#include "St_PolyLineShape.h"

ClassImp(St_PolyLineShape)

//______________________________________________________________________________
St_PolyLineShape::St_PolyLineShape()
{
   m_Shape = 0;
   m_Smooth = kFALSE;
   m_Connection= 0;
   m_Line=0;
   SetWidthFactor();
   m_HasDrawn = kFALSE;
   m_ShapeType = kNULL;
}
//______________________________________________________________________________
St_PolyLineShape::St_PolyLineShape(TPolyLine3D  *line,Option_t* option)
{
   m_Shape       = new TTUBE("tube","tube","void",0.5,0.5);
   m_ShapeType   = kNULL;
   m_Smooth      = kFALSE;
   m_Connection  = 0;
   m_Line        = line;
   m_HasDrawn    = kFALSE;
   // Take in account the current node if any   
   if (!m_Line) { 
     Error("St_PolyLineShape","No polyline is defined");
     return;
   }
   SetWidthFactor();
   Create();
}

//______________________________________________________________________________
St_PolyLineShape::~St_PolyLineShape()
{
  SafeDelete(m_Shape);
}
//______________________________________________________________________________
void St_PolyLineShape::Axis(TVirtualPad *p, Float_t width)
{
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
      TPolyLine3D *lx = new TPolyLine3D(2);
      lx->SetLineColor(6);
      lx->SetLineWidth(5);
      lx->SetPoint(0,0,0,0);
      lx->SetPoint(1,max[0],0,0);

      TPolyLine3D *ly = new TPolyLine3D(2);
      ly->SetLineColor(4);
      ly->SetLineWidth(5);
      ly->SetPoint(0,0,0,0);
      ly->SetPoint(1,0,max[1],0);

      TPolyLine3D *lz = new TPolyLine3D(2);
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
 }   

//______________________________________________________________________________
void St_PolyLineShape::Create()
{
    if (!m_Connection) SetConnection(kBrik);
}

//______________________________________________________________________________
Int_t St_PolyLineShape::SetConnection(EShapeTypes connection)
{
// Float_t size = 0.5*GetWidthFactor()*(m_Line->GetLineWidth());
 Float_t size = 0.5;

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
 if (m_Line) return m_Line->DistancetoPrimitive(px,py);
 return 999999;
}

//______________________________________________________________________________
void St_PolyLineShape::Draw(Option_t *opt)
{

  if (m_HasDrawn) return;

  Create();

  if (m_Line) {
//   m_Line->Draw();
    AppendPad();
    m_HasDrawn = kTRUE;
  }  
}

//______________________________________________________________________________
void St_PolyLineShape::ExecuteEvent(Int_t event, Int_t px, Int_t py)
{
 if (m_Line) m_Line->ExecuteEvent(event,px, py);
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
  
    Float_t width = GetWidthFactor()*(m_Line->GetLineWidth());

    mrot[0][0] *= width;
    mrot[0][1] *= width;
    mrot[0][2] *= width;

    mrot[1][0] *= width;
    mrot[1][1] *= width;
    mrot[1][2] *= width;

    mrot[2][0] *= length;
    mrot[2][1] *= length;
    mrot[2][2] *= length;


    St_Node node("SegmentNode","SegmentNode", shape);
    node.SetLineColor(m_Line->GetLineColor());
    if (!m_Shape) node.SetVisibility(2);
    node.SetLineColor(m_Line->GetLineColor());

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
    knee.SetLineColor(m_Line->GetLineColor());
    node.Add(&knee,&kneePosition);

    node.PaintNodePosition(option, &position);
}

//______________________________________________________________________________
void St_PolyLineShape::Paint(Option_t *opt)
{
  if (!m_Line) return;
  if (!strstr(opt, "x3d"))
     m_Line->Paint(opt);
  else
     m_Line->Paint(opt);
//     Paint3d(opt);
}

//______________________________________________________________________________
void St_PolyLineShape::Paint3d(Option_t *opt)
{
 if (!m_Line) return;

 Create();

 struct XYZ { Float_t xyz[3]; } *points;
 points  = (XYZ *)(m_Line->GetP());
 Int_t size      = m_Line->GetN()-1;
  
 for (Int_t i=0;i<size;i++) 
      PaintNode((Float_t *)(points+i+1),(Float_t *)(points+i),opt);      
 m_HasDrawn = kTRUE;
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
  if (m_Line)  m_Line->Sizeof3D();
}
