//*-- Author :    Valery Fine(fine@bnl.gov)   25/12/98  
// $Id: St_NodePosition.cxx,v 1.12 1999/04/02 23:36:04 fine Exp $
// $Log: St_NodePosition.cxx,v $
// Revision 1.12  1999/04/02 23:36:04  fine
// Collapsed geometry structures has been implemeted
//
// Revision 1.11  1999/03/30 22:30:14  fine
//  Visibility test has been added for Paint method
//
// Revision 1.10  1999/03/27 22:44:59  fine
// Working 3D St_node with X3d and OpenGL
//
// Revision 1.9  1999/02/05 16:32:30  fine
// St_NodePostion::Draw() method has been implemented
//
// Revision 1.8  1999/02/04 19:22:23  fine
// Severak drawing method have been added to draw STAR nodes
//
// Revision 1.7  1999/01/31 02:03:07  fine
// St_DataSetIter::Notify - new method + clean up
//
// Revision 1.6  1999/01/30 04:24:21  fine
// St_Table: Print memory leak fixed
//
// Revision 1.5  1998/12/30 22:30:17  fine
// St_Table::PrintHrader method has been introduced
//
// Revision 1.4  1998/12/27 03:16:52  fine
// Flag WIN32 has been introduced for St_Node / St_NodePosition against of crashes
//
// Revision 1.3  1998/12/27 02:33:16  fine
// St_Node, St_NodePosition - first working versions have been introduced *see macros/STAR_shapes.C for an example)
//
// Revision 1.2  1998/12/26 21:40:40  fisyak
// Add Id and Log
// 
 
//*KEEP,CopyRight,T=C.
/*************************************************************************
 * Copyright(c) 1998, FineSoft, All rights reserved.                     *
 * Authors: Valery Fine                                                  *
 *************************************************************************/
//*KEND.
 
#include <iostream.h>
#include <iomanip.h>

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
St_NodePosition::St_NodePosition(St_Node *node,Double_t x, Double_t y, Double_t z, const Text_t *matrixname)
: fNode(node),fX(x),fY(y),fZ(z),fMatrix(0)
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
   if (!node) return;
   static Int_t counter = 0;
   counter++;
   if(!(counter%1000))cout<<"St_NodePosition count="<<counter<<" name="<<node->GetName()<<endl;
 
   if (matrixname && strlen(matrixname)) fMatrix = gGeometry->GetRotMatrix(matrixname);
   if (!fMatrix) fMatrix = St_Node::GetIdentity();
}
 
 
//______________________________________________________________________________
St_NodePosition::St_NodePosition(St_Node *node,Double_t x, Double_t y, Double_t z, TRotMatrix *matrix)
               : fNode(node),fX(x),fY(y),fZ(z),fMatrix(matrix)
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
   if (!fMatrix) fMatrix = St_Node::GetIdentity();
}
//______________________________________________________________________________
void St_NodePosition::Browse(TBrowser *b)
{
#ifndef WIN32
   Inspect();
#endif
   if (GetNode()) {
        TShape *shape = GetNode()->GetShape();
        b->Add(GetNode(),shape?shape->GetName():GetNode()->GetName());
   }
//    if( GetList() ) {
//       GetList()->Browse( b );
//    }
   else {
       Draw();
       gPad->Update();
    }
} 
//______________________________________________________________________________
Int_t St_NodePosition::DistancetoPrimitive(Int_t, Int_t)
{
//*-*-*-*-*-*-*-*-*-*-*Compute distance from point px,py to a Node*-*-*-*-*-*
//*-*                  ===========================================
//*-*  Compute the closest distance of approach from point px,py to this node.
//*-*  The distance is computed in pixels units.
//*-*
//*-*
//*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
   return 99999;      
}
 
//______________________________________________________________________________
void St_NodePosition::Draw(Option_t *option)
{
//*-*-*-*-*-*-*-*-*-*-*-*Draw Referenced node with current parameters*-*-*-*
//*-*                   =============================================
  St_Node *node = GetNode();
  if (node) node->Draw(option);
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
void St_NodePosition::Local2Master(Double_t *local, Double_t *master)
{
//*-*-*-*-*Convert one point from local system to master reference system*-*-*
//*-*      ==============================================================
//
//  Note that before invoking this function, the global rotation matrix
//  and translation vector for this node must have been computed.
//  This is automatically done by the Paint functions.
//  Otherwise St_NodePosition::UpdateMatrix should be called before.
 
#if 0 
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
#endif   
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
 
#if 0 
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
#endif 
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
//_______________________________________________________________________
void St_NodePosition::Print(Option_t *option)
{
  St_Node *myNode = GetNode();
  cout << " Node: " <<   GetNode()->GetName() << endl;
  cout << " Position: x=" <<
          GetX() << " : y=" << 
          GetY() << " : z=" <<
          GetZ() << endl;
 
  if (fMatrix){
      fMatrix->Print();
      Double_t *matrix = fMatrix->GetMatrix();
      Int_t i = 0;
      cout << setw(4) <<" " ; 
      for (i=0;i<3;i++) cout << setw(3) << i+1 << setw(3) << ":" ;
      cout << endl;
      for (i=0;i<3;i++) {      
        cout << i+1 << ". ";
        for (Int_t j=0;j<3;j++)
           cout << setw(6) << *matrix++ << " : " ;
        cout << endl;
      }
  }
}
 
//_______________________________________________________________________
void St_NodePosition::SavePrimitive(ofstream &out, Option_t *option)
{
#if 0
  out << "St_NodePosition *CreatePosition() { " << endl;
  out << "  St_NodePosition *myPosition = 0;    " << endl;
  Double_t x = GetX();
  Double_t y = GetY();
  Double_t z = GetZ();
  TRotMatrix *matrix = 
   myPosition =  new St_NodePosition(St_Node *node,Double_t x, Double_t y, Double_t z, const Text_t *matrixname)
: fNode(node),fX(x),fY(y),fZ(z),fMatrix(0)
{
/
  out << "  return myPosition; "                << endl;
  out << "} "                                   << endl;
#endif

}
   
//_______________________________________________________________________
void St_NodePosition::UpdatePosition(Option_t *)
{
  TPadView3D *view3D=gPad->GetView3D();
//*-*- Update translation vector and rotation matrix for new level
  if (gGeometry->GeomLevel() && fMatrix) {
     gGeometry->UpdateTempMatrix(fX,fY,fZ
                                ,fMatrix->GetMatrix()
                                ,fMatrix->IsReflection());
     if (view3D)
        view3D->UpdatePosition(fX,fY,fZ,fMatrix);
  }
}

