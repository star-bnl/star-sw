//*-- Author :    Valery Fine   10/12/98
// $Id: St_Node.cxx,v 1.24 1999/05/13 19:57:26 fine Exp $
// $Log: St_Node.cxx,v $
// Revision 1.24  1999/05/13 19:57:26  fine
// St_Node  the TShape list has been introduced, St_Node file format has been changed
//
// Revision 1.23  1999/05/12 15:46:00  fine
// some cosmetic change fro Browse method
//
// Revision 1.22  1999/04/23 22:47:33  fine
// Node family has been adjusted for St_PolyLineShape class
//
// Revision 1.21  1999/04/15 19:44:46  fine
//  St_DataSetIter::FindObject bug has been fixed. aliases FindByName and FindByPath  introduced
//
// Revision 1.20  1999/04/13 14:26:39  fine
// Geometry-based dataset implementation, next step
//
// Revision 1.19  1999/04/08 16:44:09  fine
// Working version of the NodeView family
//
// Revision 1.18  1999/04/02 23:56:17  fine
//  Clean up
//
// Revision 1.17  1999/03/30 22:30:13  fine
//  Visibility test has been added for Paint method
//
// Revision 1.16  1999/03/29 19:25:23  fine
// Visibility flag have been activated. Some working correction
//
// Revision 1.15  1999/03/27 22:44:59  fine
// Working 3D St_node with X3d and OpenGL
//
// Revision 1.14  1999/03/21 22:38:53  fine
// StDataSetIter make up + new NextDataSet method introced
//
// Revision 1.13  1999/02/04 19:22:22  fine
// Severak drawing method have been added to draw STAR nodes
//
// Revision 1.12  1999/02/04 16:26:11  fine
// St_NodeView::Paint method has been introduced
//
// Revision 1.11  1999/01/31 02:03:06  fine
// St_DataSetIter::Notify - new method + clean up
//
// Revision 1.10  1999/01/30 04:24:20  fine
// St_Table: Print memory leak fixed
//
// Revision 1.9  1999/01/29 18:41:53  fine
// St_TableSorter clean up for HP machine
//
// Revision 1.8  1999/01/25 14:22:46  fine
// St_Node Identity matrix improvement
//
// Revision 1.7  1999/01/23 18:40:53  fisyak
// Cleanup for SL98l
//
// Revision 1.6  1999/01/13 20:29:14  fine
// St_DataSet::Pass() method - the option kUp has been introduced
//
// Revision 1.5  1998/12/27 04:16:44  fine
// *** empty log message ***
//
// Revision 1.3  1998/12/27 02:33:16  fine
// St_Node, St_NodePosition - first working versions have been introduced *see macros/STAR_shapes.C for an example)
//
// Revision 1.2  1998/12/26 21:40:40  fisyak
// Add Id and Log
// 
/*************************************************************************
 * Copyright(c) 1998, FineSoft, All rights reserved. Valery Fine (Faine) *
 *************************************************************************/
//*KEND.
 
#include <iostream.h>
 
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
//*KEEP,TShape.
#include "TShape.h"
//*KEEP,St_Node.
#include "St_Node.h"
//*KEEP,TBrowser.
#include "TBrowser.h"
//*KEEP,X3DBuffer,T=C.
#include "X3DBuffer.h"
//*KEND.
 
//*KEEP,TPadView3D,T=C++.
#include "TPadView3D.h"
//*KEND.

#include "TRotMatrix.h"
#include "St_NodePosition.h"

//const Int_t kMAXLEVELS = 20;
const Int_t kSonsInvisible = BIT(17);

#if 0
const Int_t kVectorSize = 3;
const Int_t kMatrixSize = kVectorSize*kVectorSize;
 
static Double_t gTranslation[kMAXLEVELS][kVectorSize];
static Double_t gRotMatrix[kMAXLEVELS][kMatrixSize];
static Int_t gGeomLevel = 0;
 
St_Node *gNode;
#endif 
R__EXTERN  Size3D gSize3D;
static TRotMatrix *gIdentity = 0; 
ClassImp(St_Node)
 
//______________________________________________________________________________
//*-*-*-*-*-*-*-*-*-*-*-* S T N O D E  description *-*-*-*-*-*-*-*-*-*-*-*-*
//*-*                     ========================
//*-*
//*-*    A St_Node object is used to build the geometry hierarchy (see TGeometry).
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
St_Node::St_Node()
{
//*-*-*-*-*-*-*-*-*-*-*Node default constructor*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*                  ========================
 
   fShape       = 0;
   fListOfShapes = 0;
   fVisibility  = 1;
}
 
//______________________________________________________________________________
St_Node::St_Node(const Text_t *name, const Text_t *title, const Text_t *shapename, Option_t *option)
       :St_ObjectSet(name),TAttLine(), TAttFill(),fListOfShapes(0),fShape(0)
{
//*-*-*-*-*-*-*-*-*-*-*Node normal constructor*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*                  ======================
//*-*
//*-*    name    is the name of the node
//*-*    title   is title
//*-*    shapename is the name of the referenced shape
//*-*
//*-*    This new node is added into the list of sons of the current node
//*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
 
#ifdef WIN32
//*-* The color "1" - default produces a very bad 3D image with OpenGL
   Color_t lcolor = 16;
   SetLineColor(lcolor);
#endif
   static Int_t counter = 0;
   counter++;
   SetTitle(title);
   if(!(counter%1000))cout<<"St_Node count="<<counter<<" name="<<name<<endl;
   Add(gGeometry->GetShape(shapename),kTRUE);
//   fParent = gGeometry->GetCurrenSt_Node();
   fOption = option;
   fVisibility = 1;

   if(!fShape) {Printf("Illegal referenced shape"); return;}
   ImportShapeAttributes();

#if 0
   if (GetParent()) {
//      fParent->BuildListOfNodes();
//      fParent->GetListOfNodes()->Add(this);
   } else {
//      gGeometry->GetListOfNodes()->Add(this);
//      cd();
   }
#endif
}
 
 
//______________________________________________________________________________
St_Node::St_Node(const Text_t *name, const Text_t *title, TShape *shape, Option_t *option)
                :St_ObjectSet(name),TAttLine(),TAttFill(),fListOfShapes(0),fShape(0)
{
//*-*-*-*-*-*-*-*-*-*-*Node normal constructor*-*-*-*-*-*-*-*-*-*-*
//*-*                  ================================
//*-*
//*-*    name    is the name of the node
//*-*    title   is title
//*-*    shape   is the pointer to the shape definition
//*-*
//*-*    This new node is added into the list of sons of the current node
//*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
#ifdef WIN32
//*-* The color "1" - default produces a very bad 3D image with OpenGL
   Color_t lcolor = 16;
   SetLineColor(lcolor);
#endif

   Add(shape,kTRUE);
   fOption = option;
   fVisibility = 1;
   SetTitle(title);
   if(!shape) {Printf("Illegal referenced shape"); return;} 
   ImportShapeAttributes();
#if 0
   if (GetParent()) {
//      fParent->BuildListOfNodes();
//      fParent->GetListOfNodes()->Add(this);
      ImportShapeAttributes();
   } else {
//      gGeometry->GetListOfNodes()->Add(this);
//      cd();
   }
#endif

}
//______________________________________________________________________________
St_Node::St_Node(TNode &rootNode):fListOfShapes(0),fShape(0)
{
  // Convert the ROOT TNode object into STAR St_Node

  SetName(rootNode.GetName());
  SetTitle(rootNode.GetTitle());
  fVisibility = rootNode.GetVisibility();
  fOption     = rootNode.GetOption();
  Add(rootNode.GetShape(),kTRUE);

  SetLineColor(rootNode.GetLineColor());
  SetLineStyle(rootNode.GetLineStyle());
  SetLineWidth(rootNode.GetLineWidth());
  SetFillColor(rootNode.GetFillColor());
  SetFillStyle(rootNode.GetFillStyle());

  TList *nodes = rootNode.GetListOfNodes();
  if (nodes) {
    TIter next(nodes);
    TNode *node = 0;
    while (node = (TNode *) next()){
      St_Node *nextNode = new St_Node(*node);
      Add(nextNode,node->GetX(),node->GetY(),node->GetZ(),node->GetMatrix());
    }
  }
}

//______________________________________________________________________________
void St_Node::Add(TShape *shape, Bool_t IsMaster)
{
  if (!shape) return;
  if (!fListOfShapes) fListOfShapes = new TList;
  fListOfShapes->Add(shape);
  if (IsMaster) fShape = shape;
}
//______________________________________________________________________________
TNode *St_Node::CreateTNode(const St_NodePosition *position)
{
  // Convert the  STAR St_Node into ROOT TNode object

  Double_t x=0; 
  Double_t y=0;
  Double_t z=0;
  TRotMatrix* matrix = 0;
  if (position) {
     x=position->GetX(); 
     y=position->GetY();
     z=position->GetZ();
     matrix = position->GetMatrix();
  }
//  const Char_t  *path = Path();
//  printf("%s: %s/%s, shape=%s/%s\n",path,GetName(),GetTitle(),GetShape()->GetName(),GetShape()->ClassName());
  TNode *newNode  = new TNode(GetName(),GetTitle(),GetShape(),x,y,z,matrix,GetOption());
  newNode->SetVisibility(GetVisibility());

  newNode->SetLineColor(GetLineColor());
  newNode->SetLineStyle(GetLineStyle());
  newNode->SetLineWidth(GetLineWidth());
  newNode->SetFillColor(GetFillColor());
  newNode->SetFillStyle(GetFillStyle());

  TList *positions = GetListOfPositions();
  if (positions) {
    TIter next(positions);
    St_NodePosition *pos = 0;
    while (pos = (St_NodePosition *) next()){
      St_Node *node = pos->GetNode();
      if (node) {
          newNode->cd();
          TNode *nextNode = node->CreateTNode(pos);
      }
    }
  }
  newNode->ImportShapeAttributes();
  return newNode;
}

//______________________________________________________________________________
St_Node::~St_Node()
{
//*-*-*-*-*-*-*-*-*-*-*Node default destructor*-*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*                  ======================
 
   // Hmm, here we are in the troubles, in theory we have to find all
   // place where this node is sitting but we don't (yet :-()

   if (GetListOfPositions()) {
     GetListOfPositions()->Delete();
     delete GetListOfPositions();
     SetPositionsList();
   }
   SafeDelete(fListOfShapes);
#if 0
   if (fParent)     fParent->GetListOfNodes()->Remove(this);
   else    gGeometry->GetListOfNodes()->Remove(this);
   if (gGeometry->GetCurrenSt_Node() == this) gGeometry->SetCurrenSt_Node(0);
#endif
}
//______________________________________________________________________________
void St_Node::Add(St_NodePosition *position)
{
  if (!GetListOfPositions()) SetPositionsList(new TList);
  if ( GetListOfPositions()) GetListOfPositions()->Add(position);
  else Error("Add","Can not create list of positions for the current node <%s>:<%s>",GetName(),GetTitle());
}
//______________________________________________________________________________
St_NodePosition *St_Node::Add(St_Node *node, St_NodePosition *nodePosition)
{  
  St_NodePosition *position = nodePosition;
  if (!node) return 0;
  if (!position) position = new St_NodePosition(node);  // Create default position
  St_DataSet::Add(node);
  Add(position);
  return position;
}
//______________________________________________________________________________
St_NodePosition *St_Node::Add(St_Node *node, Double_t x, Double_t y, Double_t z,
                              TRotMatrix *matrix,  UInt_t id, Option_t *)
{
//*-*
//*-*    node    the pointer to the node to be placed
//*-*    x,y,z   are the offsets of the volume with respect to his mother
//*-*    matrix  is the pointer to the rotation matrix
//*-*     id     is a unique position id 
//*-*
 if (!node) return 0;
 TRotMatrix *rotation = matrix;
 if(!rotation) rotation = GetIdentity();
 St_NodePosition *position = new St_NodePosition(node,x,y,z,rotation);
 position->SetId(id);
 return Add(node,position);
}

//______________________________________________________________________________
St_NodePosition *St_Node::Add(St_Node *node, Double_t x, Double_t y, Double_t z, 
                              const Text_t *matrixname,  UInt_t id, Option_t *)
{
//*-*
//*-*    node        the pointer to the node to be placed
//*-*    x,y,z       are the offsets of the volume with respect to his mother
//*-*    matrixname  is the name of the rotation matrix
//*-*     id     is a unique position id 
//*-*
 if (!node) return 0;
 TRotMatrix *rotation = 0;
 if (matrixname && strlen(matrixname)) rotation = gGeometry->GetRotMatrix(matrixname);
 if (!rotation)                        rotation = GetIdentity();
 St_NodePosition *position = new St_NodePosition(node,x,y,z,rotation);
 position->SetId(id);
 return Add(node,position);
}

//______________________________________________________________________________
void St_Node::Browse(TBrowser *b)
{
   if (GetListOfPositions()){
       St_NodePosition *nodePosition = 0;
       TIter next(GetListOfPositions());
       Int_t posNumber = 0;
       while (nodePosition = (St_NodePosition *)next()) {
         posNumber       = nodePosition->GetId();
         TString posName = "*";
         posName += nodePosition->GetNode()->GetTitle();
         char num[10];
         posName += ";";
         sprintf(num,"%d",posNumber);
         posName += num;
         b->Add(nodePosition,posName.Data());       
       }
//       GetListOfPositions()->Browse(b);
   }
//    if( GetList() ) {
//       GetList()->Browse( b );
//    }
   else {
#ifndef WIN32
      Inspect();
#endif
 //      Draw();
 //      gPad->Update();
    }
} 

//______________________________________________________________________________
Int_t St_Node::DistancetoPrimitive(Int_t px, Int_t py)
{
  return DistancetoNodePrimitive(px,py);
}

//______________________________________________________________________________
Int_t St_Node::DistancetoNodePrimitive(Int_t px, Int_t py,St_NodePosition *pos)
{
//*-*-*-*-*-*-*-*-*Compute distance from point px,py to a St_NodeView*-*-*-*-*-*
//*-*                  ===========================================
//*-*  Compute the closest distance of approach from point px,py to the position of 
//*-*  this node.
//*-*  The distance is computed in pixels units.
//*-*
//*-*  It is restricted by 2 levels of St_Nodes
//*-*
//*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
 
   const Int_t big = 9999;
   const Int_t inaxis = 7;
   const Int_t maxdist = 5;
 
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
 
   static St_NodePosition nullPosition;
   St_NodePosition *position = pos;
   if (!position) position = &nullPosition;
   if (pos) position->UpdatePosition();      
   Int_t dist = big;
   if (GetVisibility()) {
     TShape  *shape = 0;
     TIter nextShape(fListOfShapes);
     while (shape = (TShape *)nextShape()) {
      //*-*- Distnance to the next referenced shape  if visible
      if (shape->GetVisibility()) {
        Int_t dshape = shape->DistancetoPrimitive(px,py);
        if (dshape < maxdist) {
           gPad->SetSelected(this);
           return 0;
        }
        if (dshape < dist) dist = dshape;
      }
    }
  }

//   if ( TestBit(kSonsInvisible) ) return dist;
 
//*-*- Loop on all sons
   TList *posList = GetListOfPositions();
   Int_t dnode = dist;
   if (posList && posList->GetSize()) {
      gGeometry->PushLevel();
      St_NodePosition *thisPosition;
      TObject *obj;
      TIter  next(posList);
      while ((obj = next())) {
         thisPosition = (St_NodePosition*)obj;
         St_Node *node = thisPosition->GetNode();
         dnode = node->DistancetoNodePrimitive(px,py,thisPosition);
         if (dnode <= 0)  break;
         if (dnode < dist) dist = dnode;
         if (gGeometry->GeomLevel() > 2) break;
      }
      gGeometry->PopLevel();
   }
 
   if (gGeometry->GeomLevel()==0 && dnode > maxdist) {
      gPad->SetSelected(view);
      return 0;
   } else
      return dnode;
}
 
//______________________________________________________________________________
void St_Node::Draw(Option_t *option)
{
//*-*-*-*-*-*-*-*-*-*-*-*Draw Referenced node with current parameters*-*-*-*
//*-*                   =============================================
 
   TString opt = option;
   opt.ToLower();
//*-*- Clear pad if option "same" not given
   if (!gPad) {
      if (!gROOT->GetMakeDefCanvas()) return;
      (gROOT->GetMakeDefCanvas())();
   }
   if (!opt.Contains("same")) gPad->Clear();
 
//*-*- Draw Referenced node
   gGeometry->SetGeomLevel();
   gGeometry->UpdateTempMatrix();
 
   AppendPad(option);
 
//*-*- Create a 3-D View
   TView *view = gPad->GetView();
   if (!view) {
      view = new TView(1);
      view->SetAutoRange(kTRUE);
      Paint("range");
      view->SetAutoRange(kFALSE);
   }
}

 
//______________________________________________________________________________
void St_Node::DrawOnly(Option_t *option)
{
//*-*-*-*-*-*-*-*-*-*Draw only Sons of this node*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*                ===========================
 
   SetVisibility(2);
   Draw(option);
}
 
 
//______________________________________________________________________________
void St_Node::ExecuteEvent(Int_t, Int_t, Int_t)
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
TRotMatrix *St_Node::GetIdentity()
{
 Double_t *IdentityMatrix = 0;
 // Return a pointer the "identity" matrix
 if (!gIdentity) {
      gIdentity = gGeometry->GetRotMatrix("Identity");
      if (!gIdentity) {
         gIdentity  =  new TRotMatrix();
         gIdentity->SetName("Identity");
         gIdentity->SetTitle("Identity matrix");
         gIdentity->SetMatrix(IdentityMatrix);
         gGeometry->GetListOfMatrices()->AddFirst(gIdentity);
      }
 }
 return gIdentity;
}
//______________________________________________________________________________
Text_t *St_Node::GetObjectInfo(Int_t, Int_t)
{
   if (!gPad) return "";
   static char info[512];
   sprintf(info,"%s/%s, shape=%s/%s",GetName(),GetTitle());
   TIter nextShape(fListOfShapes);
   TShape *shape = 0;
   while(shape = (TShape *)nextShape()) 
      sprintf(&info[strlen(info)]," shape=%s/%s",shape->GetName(),shape->ClassName());
   
   return info;
}
 
//______________________________________________________________________________
void St_Node::ImportShapeAttributes()
{
//*-*-*-*-*-*-*Copy shape attributes as node attributes*-*-*-*-*--*-*-*-*-*-*
//*-*          ========================================
 
   if (fShape) {
     SetLineColor(fShape->GetLineColor());
     SetLineStyle(fShape->GetLineStyle());
     SetLineWidth(fShape->GetLineWidth());
     SetFillColor(fShape->GetFillColor());
     SetFillStyle(fShape->GetFillStyle());
   }
 
   if (!GetList()) return;
   St_Node *node;
   TIter  next(GetList());
   while (node = (St_Node *)next()) 
      node->ImportShapeAttributes();
}
   
#if 0
//______________________________________________________________________________
void St_Node::ls(Option_t *option)
{
//*-*-*-*-*-*-*-*-*-*-*-*List Referenced object with current parameters*-*-*-*
//*-*                   ===============================================
 
   Int_t sizeX3D = 0;
   TString opt = option;
   opt.ToLower();
 
   Int_t maxlevel = 15;
   if (opt.Contains("1")) maxlevel = 1;
   if (opt.Contains("2")) maxlevel = 2;
   if (opt.Contains("3")) maxlevel = 3;
   if (opt.Contains("4")) maxlevel = 4;
   if (opt.Contains("5")) maxlevel = 5;
   if (opt.Contains("x")) sizeX3D  = 1;
 
   IndentLevel();
 
   Int_t nsons = 0;
   if (Nodes()) nsons = Nodes()->GetSize();
   const Text_t *shapename, *matrixname;
   if (fShape) shapename = fShape->IsA()->GetName();
   else        shapename = "????";
   cout<<GetName()<<":"<<GetTitle()<<" is a "<<shapename;
   if (sizeX3D) {
      gSize3D.numPoints = 0;
      gSize3D.numSegs   = 0;
      gSize3D.numPolys  = 0;
      Sizeof3D();
      cout<<" NumPoints="<<gSize3D.numPoints;
      cout<<" NumSegs  ="<<gSize3D.numSegs;
      cout<<" NumPolys ="<<gSize3D.numPolys;
   } else {
      cout<<" X="<<fX<<" Y="<<fY<<" Z="<<fZ;
      if (nsons) cout<<" Sons="<<nsons;
      if (fMatrix) matrixname   = fMatrix->GetName();
      else         matrixname   = "Identity";
      if(strcmp(matrixname,"Identity")) cout<<" Rot="<<matrixname;
   }
   cout<<endl;
   if(!nsons) return;
   if (gGeomLevel >= maxlevel) return;
 
   TObject::IncreaseDirLevel();
   gGeomLevel++;
   Nodes()->ls(option);
   gGeomLevel--;
   TObject::DecreaseDirLevel();
 
}
#endif
 
//______________________________________________________________________________
void St_Node::Paint(Option_t *opt)
{
  PaintNodePosition(opt);
  return; 
}

//______________________________________________________________________________
void St_Node::PaintNodePosition(Option_t *option,St_NodePosition *pos)
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
  static St_NodePosition nullPosition;

// restrict the levels for "range" option
  Int_t level = gGeometry->GeomLevel();
//  if (option && option[0]=='r' && level > 3 && strcmp(option,"range") == 0) return;
  if (GetVisibility() && option && option[0]=='r' && level > 3 ) return;

  TPadView3D *view3D=gPad->GetView3D();

  St_NodePosition *position = pos;
  if (!position)   position   = &nullPosition;

  // PaintPosition does change the current matrix and it MUST be callled FIRST !!!

  position->UpdatePosition(option);      

  PaintShape(option);  
  St_Node *n =  position->GetNode();
  if ( n && n->TestBit(kSonsInvisible) ) return;
 
//*-*- Paint all sons
  TList *posList = GetListOfPositions();
  if (posList && posList->GetSize()) {  
    gGeometry->PushLevel();
    St_NodePosition *thisPosition;
    TIter  next(posList);
    while ((thisPosition = (St_NodePosition *)next())) {
       if (view3D)  view3D->PushMatrix();

       St_Node *node = thisPosition->GetNode();
       if (node) node->PaintNodePosition(option,thisPosition);

       if (view3D) view3D->PopMatrix();
    }
    gGeometry->PopLevel();
  }
}
 
//______________________________________________________________________________
void St_Node::PaintShape(Option_t *option)
{
  // Paint shape of the node
  // To be called from the TObject::Paint method only
  TAttLine::Modify();
  TAttFill::Modify();

  if (!GetVisibility()) return;

  TIter nextShape(fListOfShapes);
  TShape *shape = 0;
  while(shape = (TShape *)nextShape()) {
    if (!shape->GetVisibility())   continue;
    shape->SetLineColor(GetLineColor());
    shape->SetLineStyle(GetLineStyle());
    shape->SetLineWidth(GetLineWidth());
    shape->SetFillColor(GetFillColor());
    shape->SetFillStyle(GetFillStyle());
    TPadView3D *view3D=gPad->GetView3D();
    if (view3D)
          view3D->SetLineAttr(GetLineColor(),GetLineWidth(),option);
    shape->Paint(option);
  }
}
//______________________________________________________________________________
void St_Node::DeletePosition(St_NodePosition *position)
{
  // DeletePosition deletes the position of the St_Node *node from this St_Node
  // and removes that node from the list of the nodes of this St_Node

  if (!position) return;

  if (GetListOfPositions()) {
    TObjLink *lnk = GetListOfPositions()->FirstLink();         
    while (lnk) {                                               
       St_NodePosition *nextPosition = (St_NodePosition *)(lnk->GetObject());
       if (nextPosition && nextPosition == position) {
          St_Node *node = nextPosition->GetNode();
          GetListOfPositions()->Remove(lnk);
          delete nextPosition;
          Remove(node);
          break;
       }
       lnk = lnk->Next();
    }                                                            
  }
}   
 
//______________________________________________________________________________
void St_Node::SetVisibility(Int_t vis)
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
   TIter  next(Nodes());
   St_Node *node;
   if (vis == -4 ) {         //Node is not drawn. Its immediate sons are drawn
      fVisibility = 0;
      if (!Nodes()) { fVisibility = 1; return;}
      while ((node = (St_Node*)next())) { node->SetVisibility(-2); }
   } else if (vis == -3 ) {  //Only node leaves are drawn
      fVisibility = 0;
      if (!Nodes()) { fVisibility = 1; return;}
      while ((node = (St_Node*)next())) { node->SetVisibility(-3); }
 
   } else if (vis == -2) {  //node is drawn. Its sons are not drawn
      fVisibility = 1; SetBit(kSonsInvisible); if (!Nodes()) return;
      while ((node = (St_Node*)next())) { node->SetVisibility(-1); }
 
   } else if (vis == -1) {  //node is not drawn. Its sons are not drawn
      fVisibility = 0; SetBit(kSonsInvisible); if (!Nodes()) return;
      while ((node = (St_Node*)next())) { node->SetVisibility(-1); }
 
   } else if (vis ==  0) {  //node is not drawn
      fVisibility = 0;
 
   } else if (vis ==  1) {  //node is drawn
      fVisibility = 1;
 
   } else if (vis ==  2) {  //node is not drawn but its sons are drawn
      fVisibility = 0; if (!Nodes()) return;
      while ((node = (St_Node*)next())) { node->SetVisibility(3); }
 
   } else if (vis ==  3) {  //node is drawn and its sons are drawn
      fVisibility = 1; if (!Nodes()) return;
      while ((node = (St_Node*)next())) { node->SetVisibility(3); }
   }
}
 
//______________________________________________________________________________
void St_Node::Sizeof3D() const
{
//*-*-*-*-*-*-*Return total size of this 3-D Node with its attributes*-*-*
//*-*          ==========================================================
 
   if (GetVisibility()) {
     TIter nextShape(fListOfShapes);
     TShape *shape = 0;
     while(shape = (TShape *)nextShape()) {
        if (shape->GetVisibility())  shape->Sizeof3D();
     }
   }
//   if ( TestBit(kSonsInvisible) ) return;
 
   if (!Nodes()) return;
   St_Node *node;
   TObject *obj;
   TIter  next(Nodes());
   while ((obj = next())) {
      node = (St_Node*)obj;
      node->Sizeof3D();
   }
}

