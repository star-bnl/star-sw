//*-- Author :    Valery Fine(fine@bnl.gov)   25/12/98  
// $Id: St_NodePosition.cxx,v 1.2 1998/12/26 21:40:40 fisyak Exp $
// $Log: St_NodePosition.cxx,v $
// Revision 1.2  1998/12/26 21:40:40  fisyak
// Add Id and Log
// 
 
//*KEEP,CopyRight,T=C.
/*************************************************************************
 * Copyright(c) 1998, FineSoft, All rights reserved.                     *
 * Authors: Valery Fine                                                  *
 *                                                                       *
 * Permission to use, copy, modify and distribute this software and its  *
 * documentation for non-commercial purposes is hereby granted without   *
 * fee, provided that the above copyright notice appears in all copies   *
 * and that both the copyright notice and this permission notice appear  *
 * in the supporting documentation. The authors make no claims about the *
 * suitability of this software for any purpose.                         *
 * It is provided "as is" without express or implied warranty.           *
 *************************************************************************/
//*KEND.
 
#include <iostream.h>

#include "St_NodePosition.h" 
#include "St_Node.h" 

//*KEEP,TROOT.
#include "TROOT.h"
//*KEEP,TClass.
#include "TClass.h"
//*KEEP,TVirtualPad.
#include "TVirtualPad.h"
//*KEEP,TView.
#include "TView.h"
//*KEEP,TGeometry.
#include "TGeometry.h"
//*KEEP,TRotMatrix.
#include "TRotMatrix.h"
//*KEEP,TBrowser.
#include "TBrowser.h"
//*KEEP,X3DBuffer,T=C.
#include "X3DBuffer.h"
//*KEND.
 
//*KEEP,TPadView3D,T=C++.
#include "TPadView3D.h"
//*KEND.
 
#if 0
const Int_t kMAXLEVELS = 20;
const Int_t kVectorSize = 3;
const Int_t kMatrixSize = kVectorSize*kVectorSize;
const Int_t kSonsInvisible = BIT(17);
 
static Double_t gTranslation[kMAXLEVELS][kVectorSize];
static Double_t gRotMatrix[kMAXLEVELS][kMatrixSize];
static Int_t gGeomLevel = 0;
 
St_NodePosition *gNode;
#endif
 
R__EXTERN  Size3D gSize3D;
 
ClassImp(St_NodePosition)
 
//______________________________________________________________________________
//*-*-*-*-*-*-*-*-* S T N O D E P O S I T I O N   description *-*-*-*-*-*-*-*-*-
//*-*               ===========================
//*-*
//*-*    A St_NodePosition object is used to build the geometry hierarchy (see TGeometry).
//*-*    A node may contain other nodes.
//*-*
//*-*    A geometry node has attributes:
//*-*      - name and title
//*-*      - pointer to the referenced shape (see TShape).
//*-*      - x,y,z offset with respect to the mother node.
//*-*      - pointer to the rotation matrix (see TRotMatrix).
//*-*
//*-*    A node can be drawn.
//*-*
//*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
 
 
//______________________________________________________________________________
St_NodePosition::St_NodePosition(St_Node *node,Double_t x, Double_t y, Double_t z, const Text_t *matrixname, Option_t *option)
: fNode(node),fX(x),fY(y),fZ(z)
{
//*-*-*-*-*-*-*-*-*-*-*Node normal constructor*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*                  ======================
//*-*
//*-*    name    is the name of the node
//*-*    title   is title
//*-*    x,y,z   are the offsets of the volume with respect to his mother
//*-*    matrixname  is the name of the rotation matrix
//*-*
//*-*    This new node is added into the list of sons of the current node
//*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
 
   static Int_t counter = 0;
   counter++;
   if(!(counter%1000))cout<<"St_NodePosition count="<<counter<<" name="<<name<<endl;
 
   if (strlen(matrixname)) fMatrix = gGeometry->GetRotMatrix(matrixname);
   else {
     fMatrix = gGeometry->GetRotMatrix("Identity");
     if (!fMatrix) {
        new TRotMatrix("Identity","Identity matrix",90,0,90,90,0,0);
        fMatrix  = gGeometry->GetRotMatrix("Identity");
     }
   }
}
 
 
//______________________________________________________________________________
St_NodePosition::St_NodePosition(St_Node *node,Double_t x, Double_t y, Double_t z, TRotMatrix *matrix, Option_t *option)
               : fNode(node),fX(x),fY(y),fZ(z)
{
//*-*-*-*-*-*-*-*-*-*-*Node normal constructor*-*-*-*-*-*-*-*-*-*-*
//*-*                  ================================
//*-*
//*-*    name    is the name of the node
//*-*    title   is title
//*-*    x,y,z   are the offsets of the volume with respect to his mother
//*-*    matrix  is the pointer to the rotation matrix
//*-*
//*-*    This new node is added into the list of sons of the current node
//*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
 
   fMatrix = matrix;
   if(!fMatrix) {
     fMatrix =gGeometry->GetRotMatrix("Identity");
     if (!fMatrix) {
        new TRotMatrix("Identity","Identity matrix",90,0,90,90,0,0);
        fMatrix  = gGeometry->GetRotMatrix("Identity");
     }
   }
}
  
//______________________________________________________________________________
Int_t St_NodePosition::DistancetoPrimitive(Int_t px, Int_t py)
{
//*-*-*-*-*-*-*-*-*-*-*Compute distance from point px,py to a Node*-*-*-*-*-*
//*-*                  ===========================================
//*-*  Compute the closest distance of approach from point px,py to this node.
//*-*  The distance is computed in pixels units.
//*-*
//*-*
//*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
 
   const Int_t big = 9999;
   const Int_t inaxis = 7;
   const Int_t maxdist = 5;
 
   Int_t i;
 
   Int_t puxmin = gPad->XtoAbsPixel(gPad->GetUxmin());
   Int_t puymin = gPad->YtoAbsPixel(gPad->GetUymin());
   Int_t puxmax = gPad->XtoAbsPixel(gPad->GetUxmax());
   Int_t puymax = gPad->YtoAbsPixel(gPad->GetUymax());
 
//*-*- return if point is not in the user area
   if (px < puxmin - inaxis) return big;
   if (py > puymin + inaxis) return big;
   if (px > puxmax + inaxis) return big;
   if (py < puymax - inaxis) return big;
 
   TView *view =gPad->GetView();
   if (!view) return big;
 
//*-*- Update translation vector and rotation matrix for new level
   if (gGeomLevel) {
      Int_t ig = gGeomLevel;
      UpdateTempMatrix(&(gTranslation[ig-1][0]),&gRotMatrix[ig-1][0]
                      ,fX,fY,fZ,fMatrix->GetMatrix()
                      ,&gTranslation[ig][0],&gRotMatrix[ig][0]);
 
   } else {
      for (i=0;i<kVectorSize;i++) gTranslation[0][i] = 0;
      for (i=0;i<kMatrixSize;i++) gRotMatrix[0][i] = 0;
      gRotMatrix[0][0] = 1;   gRotMatrix[0][4] = 1;   gRotMatrix[0][8] = 1;
   }
 
//*-*- Paint Referenced shape
   Int_t dist = big;
   if (fVisibility && fShape->GetVisibility()) {
      gNode = this;
      dist = fShape->DistancetoPrimitive(px,py);
      if (dist < maxdist) {
         gPad->SetSelected(this);
         return 0;
      }
   }
   if ( TestBit(kSonsInvisible) ) return dist;
 
//*-*- Loop on all sons
   Int_t nsons = 0;
   if (fNodes) nsons = fNodes->GetSize();
   Int_t dnode = dist;
   if (nsons) {
 
      Int_t levelsave = gGeomLevel;
      gGeomLevel++;
      St_NodePosition *node;
      TObject *obj;
      TIter  next(fNodes);
      while ((obj = next())) {
         node = (St_NodePosition*)obj;
         dnode = node->DistancetoPrimitive(px,py);
         if (dnode <= 0) break;
         if (dnode < dist) dist = dnode;
      }
      gGeomLevel = levelsave;
   }
 
   if (gGeomLevel==0 && dnode > maxdist) {
      gPad->SetSelected(view);
      return 0;
   } else
      return dnode;
}
 
//______________________________________________________________________________
void St_NodePosition::Draw(Option_t *option)
{
//*-*-*-*-*-*-*-*-*-*-*-*Draw Referenced node with current parameters*-*-*-*
//*-*                   =============================================
 
   Int_t i;
   TString opt = option;
   opt.ToLower();
//*-*- Clear pad if option "same" not given
   if (!gPad) {
      if (!gROOT->GetMakeDefCanvas()) return;
      (gROOT->GetMakeDefCanvas())();
   }
   if (!opt.Contains("same")) gPad->Clear();
 
//*-*- Draw Referenced node
   gGeomLevel = 0;
   for (i=0;i<kVectorSize;i++) gTranslation[0][i] = 0;
   for (i=0;i<kMatrixSize;i++) gRotMatrix[0][i] = 0;
   gRotMatrix[0][0] = 1;   gRotMatrix[0][4] = 1;   gRotMatrix[0][8] = 1;
 
 
   AppendPad(option);
 
//*-*- Create a 3-D View
   TView *view = gPad->GetView();
   if (!view) {
      view = new TView(1);
      view->SetAutoRange(kTRUE);
      Paint();
      view->SetAutoRange(kFALSE);
   }
}
 
//______________________________________________________________________________
void St_NodePosition::DrawOnly(Option_t *option)
{
//*-*-*-*-*-*-*-*-*-*Draw only Sons of this node*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*                ===========================
 
   SetVisibility(2);
   Draw(option);
}
 
 
//______________________________________________________________________________
void St_NodePosition::ExecuteEvent(Int_t, Int_t, Int_t)
{
//*-*-*-*-*-*-*-*-*-*-*Execute action corresponding to one event*-*-*-*
//*-*                  =========================================
//*-*  This member function must be implemented to realize the action
//*-*  corresponding to the mouse click on the object in the window
//*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
 
//   if (gPad->GetView())
//             gPad->GetView()->ExecuteRotateView(event, px, py);
 
//   if (!gPad->GetListOfPrimitives()->FindObject(this)) gPad->SetCursor(kCross);
   gPad->SetCursor(kHand);
}
  
//______________________________________________________________________________
Text_t *St_NodePosition::GetObjectInfo(Int_t, Int_t)
{
   if (!gPad) return "";
   if (!GetNode()) return "";
   static char info[64];   
   sprintf(info,"%s/%s, shape=%s/%s",GetNode()->GetName(),GetNode()->GetTitle(),GetNode()->GetShape()->GetName(),GetNode()->GetShape()->ClassName());
   return info;
}
 
//______________________________________________________________________________
Bool_t St_NodePosition::IsFolder()
{
//*-*-*-*-*Return TRUE if node contains nodes, FALSE otherwise*-*
//*-*      ======================================================
 
   if (fNodes) return kTRUE;
   else        return kFALSE;
}
 
//______________________________________________________________________________
void St_NodePosition::Local2Master(Double_t *local, Double_t *master)
{
//*-*-*-*-*Convert one point from local system to master reference system*-*-*
//*-*      ==============================================================
//
//  Note that before invoking this function, the global rotation matrix
//  and translation vector for this node must have been computed.
//  This is automatically done by the Paint functions.
//  Otherwise St_NodePosition::UpdateMatrix should be called before.
 
   Double_t x,y,z;
   Float_t bomb = gGeometry->GetBomb();
 
   Double_t *matrix      = &gRotMatrix[gGeomLevel][0];
   Double_t *translation = &gTranslation[gGeomLevel][0];
 
   x = bomb*translation[0]
     + local[0]*matrix[0]
     + local[1]*matrix[3]
     + local[2]*matrix[6];
 
   y = bomb*translation[1]
     + local[0]*matrix[1]
     + local[1]*matrix[4]
     + local[2]*matrix[7];
 
   z = bomb*translation[2]
     + local[0]*matrix[2]
     + local[1]*matrix[5]
     + local[2]*matrix[8];
 
   master[0] = x; master[1] = y; master[2] = z;
}
 
//______________________________________________________________________________
void St_NodePosition::Local2Master(Float_t *local, Float_t *master)
{
//*-*-*-*-*Convert one point from local system to master reference system*-*-*
//*-*      ==============================================================
//
//  Note that before invoking this function, the global rotation matrix
//  and translation vector for this node must have been computed.
//  This is automatically done by the Paint functions.
//  Otherwise St_NodePosition::UpdateMatrix should be called before.
 
   Float_t x,y,z;
   Float_t bomb = gGeometry->GetBomb();
 
   Double_t *matrix      = &gRotMatrix[gGeomLevel][0];
   Double_t *translation = &gTranslation[gGeomLevel][0];
 
   x = bomb*translation[0]
     + local[0]*matrix[0]
     + local[1]*matrix[3]
     + local[2]*matrix[6];
 
   y = bomb*translation[1]
     + local[0]*matrix[1]
     + local[1]*matrix[4]
     + local[2]*matrix[7];
 
   z = bomb*translation[2]
     + local[0]*matrix[2]
     + local[1]*matrix[5]
     + local[2]*matrix[8];
 
   master[0] = x; master[1] = y; master[2] = z;
 
}
  
//______________________________________________________________________________
void St_NodePosition::Paint(Option_t *option)
{
//*-*-*-*-*-*-*-*-*-*-*-*Paint Referenced node with current parameters*-*-*-*
//*-*                   ==============================================
//*-*
//*-*  vis = 1  (default) shape is drawn
//*-*  vis = 0  shape is not drawn but its sons may be not drawn
//*-*  vis = -1 shape is not drawn. Its sons are not drawn
//*-*  vis = -2 shape is drawn. Its sons are not drawn
//*-*
//*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
#if 0 
   TPadView3D *view3D=gPad->GetView3D();
 
//*-*- Update translation vector and rotation matrix for new level
   if (gGeomLevel)
   {
 
      UpdateTempMatrix(&(gTranslation[gGeomLevel-1][0]),&gRotMatrix[gGeomLevel-1][0]
                      ,fX,fY,fZ,fMatrix->GetMatrix()
                      ,&gTranslation[gGeomLevel][0],&gRotMatrix[gGeomLevel][0]);
      if (view3D)
          view3D->UpdateNodeMatrix(this,option);
   }
 
//*-*- Paint Referenced shape
//   Int_t vis = fShape->GetVisibility();
//   if ( vis == -1) return;
   Int_t nsons = 0;
   if (fNodes) nsons = fNodes->GetSize();
//   if (vis == -3) {
//     if (nsons == 0) vis = 1;
//     else            vis = 0;
//   }
 
   TAttLine::Modify();
   TAttFill::Modify();
   if (fVisibility && fShape->GetVisibility()) {
      gNode = this;
      fShape->SetLineColor(GetLineColor());
      fShape->SetLineStyle(GetLineStyle());
      fShape->SetLineWidth(GetLineWidth());
      fShape->SetFillColor(GetFillColor());
      fShape->SetFillStyle(GetFillStyle());
      if (view3D)
          view3D->SetAtSt_NodePosition(this,option);
      fShape->Paint(option);
   }
   if ( TestBit(kSonsInvisible) ) return;
 
//*-*- Paint all sons
   if(!nsons) return;
 
   gGeomLevel++;
   St_NodePosition *node;
   TObject *obj;
   TIter  next(fNodes);
   while ((obj = next())) {
      if (view3D)
          view3D->PushMatrix();
 
      node = (St_NodePosition*)obj;
      node->Paint(option);
      if (view3D)
          view3D->PopMatrix();
   }
   gGeomLevel--;
#endif 
}
   
//______________________________________________________________________________
void St_NodePosition::SetVisibility(Int_t vis)
{
//*-*-*-*-*-*-*Set visibility for this node and its sons*-*-*-*-*--*-*-*-*-*-*
//*-*          =========================================
//*-*  vis = 3  node is drawn and its sons are drawn
//*-*  vis = 2  node is not drawn but its sons are drawn
//*-*  vis = 1  (default) node is drawn
//*-*  vis = 0  node is not drawn
//*-*  vis = -1 node is not drawn. Its sons are not drawn
//*-*  vis = -2 node is drawn. Its sons are not drawn
//*-*  vis = -3 Only node leaves are drawn
//*-*  vis = -4 Node is not drawn. Its immediate sons are drawn
//*-*
//*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
 
   ResetBit(kSonsInvisible);
   TIter  next(fNodes);
   St_NodePosition *node;
   if (vis == -4 ) {         //Node is not drawn. Its immediate sons are drawn
      fVisibility = 0;
      if (!fNodes) { fVisibility = 1; return;}
      while ((node = (St_NodePosition*)next())) { node->SetVisibility(-2); }
   } else if (vis == -3 ) {  //Only node leaves are drawn
      fVisibility = 0;
      if (!fNodes) { fVisibility = 1; return;}
      while ((node = (St_NodePosition*)next())) { node->SetVisibility(-3); }
 
   } else if (vis == -2) {  //node is drawn. Its sons are not drawn
      fVisibility = 1; SetBit(kSonsInvisible); if (!fNodes) return;
      while ((node = (St_NodePosition*)next())) { node->SetVisibility(-1); }
 
   } else if (vis == -1) {  //node is not drawn. Its sons are not drawn
      fVisibility = 0; SetBit(kSonsInvisible); if (!fNodes) return;
      while ((node = (St_NodePosition*)next())) { node->SetVisibility(-1); }
 
   } else if (vis ==  0) {  //node is not drawn
      fVisibility = 0;
 
   } else if (vis ==  1) {  //node is drawn
      fVisibility = 1;
 
   } else if (vis ==  2) {  //node is not drawn but its sons are drawn
      fVisibility = 0; if (!fNodes) return;
      while ((node = (St_NodePosition*)next())) { node->SetVisibility(3); }
 
   } else if (vis ==  3) {  //node is drawn and its sons are drawn
      fVisibility = 1; if (!fNodes) return;
      while ((node = (St_NodePosition*)next())) { node->SetVisibility(3); }
   }
}
 
//______________________________________________________________________________
void St_NodePosition::Sizeof3D() const
{
//*-*-*-*-*-*-*Return total size of this 3-D Node with its attributes*-*-*
//*-*          ==========================================================
 
   if (fVisibility && fShape->GetVisibility()) {
      fShape->Sizeof3D();
   }
   if ( TestBit(kSonsInvisible) ) return;
 
   if (!fNodes) return;
   St_NodePosition *node;
   TObject *obj;
   TIter  next(fNodes);
   while ((obj = next())) {
      node = (St_NodePosition*)obj;
      node->Sizeof3D();
   }
}
 
//_______________________________________________________________________
void St_NodePosition::Streamer(TBuffer &b)
{
//*-*-*-*-*-*-*-*-*Stream a class object*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*              =========================================
   if (b.IsReading()) {
      Version_t v = b.ReadVersion();
      TNamed::Streamer(b);
      TAttLine::Streamer(b);
      TAttFill::Streamer(b);
      b >> fX;
      b >> fY;
      b >> fZ;
      b >> fMatrix;
      b >> fShape;
      b >> fParent;
      b >> fNodes;
      fOption.Streamer(b);
      if (v > 1) b >> fVisibility;
      else  fVisibility = fShape->GetVisibility();
   } else {
      b.WriteVersion(St_NodePosition::IsA());
      TNamed::Streamer(b);
      TAttLine::Streamer(b);
      TAttFill::Streamer(b);
      b << fX;
      b << fY;
      b << fZ;
      b << fMatrix;
      b << fShape;
      b << fParent;
      b << fNodes;
      fOption.Streamer(b);
      b << fVisibility;
   }
}
 
 
//______________________________________________________________________________
void St_NodePosition::UpdateMatrix()
{
//    Update global rotation matrix/translation vector for this node
//   this function must be called before invoking Local2Master
 
   St_NodePosition *nodes[kMAXLEVELS], *node;
   Int_t i;
   for (i=0;i<kVectorSize;i++) gTranslation[0][i] = 0;
   for (i=0;i<kMatrixSize;i++) gRotMatrix[0][i] = 0;
   gRotMatrix[0][0] = 1;   gRotMatrix[0][4] = 1;   gRotMatrix[0][8] = 1;
 
   node     = this;
   gGeomLevel  = 0;
   //build array of parent nodes
   while (node) {
      nodes[gGeomLevel] = node;
      node = node->GetParent();
      gGeomLevel++;
   }
   gGeomLevel--;
   //Update matrices in the hierarchy
   for (i=1;i<=gGeomLevel;i++) {
      node = nodes[gGeomLevel-i];
      UpdateTempMatrix(&(gTranslation[i-1][0]),&gRotMatrix[i-1][0]
                      ,node->GetX(),node->GetY(),node->GetZ(),node->GetMatrix()->GetMatrix()
                      ,&gTranslation[i][0],&gRotMatrix[i][0]);
   }
}
 
//______________________________________________________________________________
void St_NodePosition::UpdateTempMatrix(Double_t *dx,Double_t *rmat
                         , Double_t x, Double_t y, Double_t z, Double_t *matrix
                         , Double_t *dxnew, Double_t *rmatnew)
{
//*-*-*-*-*-*-*Compute new translation vector and global matrix*-*-*-*-*-*-*-*
//*-*          ================================================
//*-*
//*-*  dx      old translation vector
//*-*  rmat    old global matrix
//*-*  x,y,z   offset of new local system with respect to mother
//*-*  dxnew   new translation vector
//*-*  rmatnew new global rotation matrix
//*-*
//*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
 
 
   dxnew[0] = dx[0] + x*rmat[0] + y*rmat[3] + z*rmat[6];
   dxnew[1] = dx[1] + x*rmat[1] + y*rmat[4] + z*rmat[7];
   dxnew[2] = dx[2] + x*rmat[2] + y*rmat[5] + z*rmat[8];
 
 
   rmatnew[0] = rmat[0]*matrix[0] + rmat[3]*matrix[1] + rmat[6]*matrix[2];
   rmatnew[1] = rmat[1]*matrix[0] + rmat[4]*matrix[1] + rmat[7]*matrix[2];
   rmatnew[2] = rmat[2]*matrix[0] + rmat[5]*matrix[1] + rmat[8]*matrix[2];
   rmatnew[3] = rmat[0]*matrix[3] + rmat[3]*matrix[4] + rmat[6]*matrix[5];
   rmatnew[4] = rmat[1]*matrix[3] + rmat[4]*matrix[4] + rmat[7]*matrix[5];
   rmatnew[5] = rmat[2]*matrix[3] + rmat[5]*matrix[4] + rmat[8]*matrix[5];
   rmatnew[6] = rmat[0]*matrix[6] + rmat[3]*matrix[7] + rmat[6]*matrix[8];
   rmatnew[7] = rmat[1]*matrix[6] + rmat[4]*matrix[7] + rmat[7]*matrix[8];
   rmatnew[8] = rmat[2]*matrix[6] + rmat[5]*matrix[7] + rmat[8]*matrix[8];
}
