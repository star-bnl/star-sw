//*-- Author :    Valery Fine   10/12/98
// $Id: St_Node.cxx,v 1.9 1999/01/29 18:41:53 fine Exp $
// $Log: St_Node.cxx,v $
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
 
   fShape  = 0;
   fVisibility = 1;
}
 
//______________________________________________________________________________
St_Node::St_Node(const Text_t *name, const Text_t *title, const Text_t *shapename, Option_t *option)
       :St_ObjectSet(name),TAttLine(), TAttFill()
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
   fShape  = gGeometry->GetShape(shapename);
//   fParent = gGeometry->GetCurrenSt_Node();
   fOption = option;
   fVisibility = 1;

   if(!fShape) {Printf("Illegal referenced shape"); return;}

#if 0 
   if (fParent) {
      fParent->BuildListOfNodes();
      fParent->GetListOfNodes()->Add(this);
      ImportShapeAttributes();
   } else {
      gGeometry->GetListOfNodes()->Add(this);
      cd();
   }
#endif
}
 
 
//______________________________________________________________________________
St_Node::St_Node(const Text_t *name, const Text_t *title, TShape *shape, Option_t *option)
                :St_ObjectSet(name),TAttLine(),TAttFill()
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

   fShape  = shape;
   fOption = option;
   fVisibility = 1;
   SetTitle(title);
   if(!shape) {Printf("Illegal referenced shape"); return;} 

}
//______________________________________________________________________________
St_Node::St_Node(TNode &rootNode)
{
  // Convert the ROOT TNode object into STAR St_Node

  SetName(rootNode.GetName());
  SetTitle(rootNode.GetTitle());
  fVisibility = rootNode.GetVisibility();
  fOption     = rootNode.GetOption();
  fShape      = rootNode.GetShape();

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
                              TRotMatrix *matrix, Option_t *option)
{
//*-*
//*-*    node    the pointer to the node to be placed
//*-*    x,y,z   are the offsets of the volume with respect to his mother
//*-*    matrix  is the pointer to the rotation matrix
//*-*
 if (!node) return 0;
 TRotMatrix *rotation = matrix;
 if(!rotation) {
    if (!gIdentity) {
       gIdentity = gGeometry->GetRotMatrix("Identity");
       if (!gIdentity) 
          gIdentity  =  new TRotMatrix("Identity","Identity matrix",90,0,90,90,0,0);
   }
   rotation = gIdentity;
 }
 St_NodePosition *position = new St_NodePosition(node,x,y,z,rotation);
 return Add(node,position);
}

//______________________________________________________________________________
St_NodePosition *St_Node::Add(St_Node *node, Double_t x, Double_t y, Double_t z,  const Text_t *matrixname, Option_t *option)
{
//*-*
//*-*    node        the pointer to the node to be placed
//*-*    x,y,z       are the offsets of the volume with respect to his mother
//*-*    matrixname  is the name of the rotation matrix
//*-*
 if (!node) return 0;
 TRotMatrix *rotation = gIdentity;
 if (strlen(matrixname)) rotation = gGeometry->GetRotMatrix(matrixname);
 else if (!gIdentity) {
    gIdentity = gGeometry->GetRotMatrix("Identity");
    if (!gIdentity) 
          gIdentity  =  new TRotMatrix("Identity","Identity matrix",90,0,90,90,0,0);
    rotation = gIdentity;
 }
 St_NodePosition *position = new St_NodePosition(node,x,y,z,rotation);
 return Add(node,position);
}

//______________________________________________________________________________
void St_Node::Browse(TBrowser *b)
{
   if (GetListOfPositions()){
       St_NodePosition *nodePosition = 0;
       TIter next(GetListOfPositions());
       while (nodePosition = (St_NodePosition *)next()){
//         b->Add(nodePosition->GetNode());
         b->Add(nodePosition,nodePosition->GetNode()->GetName());
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
//*-*-*-*-*-*-*-*-*-*-*Compute distance from point px,py to a Node*-*-*-*-*-*
//*-*                  ===========================================
//*-*  Compute the closest distance of approach from point px,py to this node.
//*-*  The distance is computed in pixels units.
//*-*
//*-*
//*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
#if 0 
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
   if (Nodes()) nsons = Nodes()->GetSize();
   Int_t dnode = dist;
   if (nsons) {
 
      Int_t levelsave = gGeomLevel;
      gGeomLevel++;
      St_Node *node;
      TObject *obj;
      TIter  next(Nodes());
      while ((obj = next())) {
         node = (St_Node*)obj;
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
#else
   return 0;
#endif
}
 
//______________________________________________________________________________
void St_Node::Draw(Option_t *option)
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
#if 0
   St_NodeStruct *nodeToDraw = new St_NodeStruct(this);
   nodeToDraw->Draw(option);
#endif

//*-*- Create a 3-D View
   TView *view = gPad->GetView();
   if (!view) {
      view = new TView(1);
      view->SetAutoRange(kTRUE);
#if 0
      nodeToDraw->Paint();
#endif
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
Text_t *St_Node::GetObjectInfo(Int_t, Int_t)
{
   if (!gPad) return "";
   static char info[64];
   sprintf(info,"%s/%s, shape=%s/%s",GetName(),GetTitle(),fShape->GetName(),fShape->ClassName());
   return info;
}
 
//______________________________________________________________________________
void St_Node::ImportShapeAttributes()
{
//*-*-*-*-*-*-*Copy shape attributes as node attributes*-*-*-*-*--*-*-*-*-*-*
//*-*          ========================================
 
   SetLineColor(fShape->GetLineColor());
   SetLineStyle(fShape->GetLineStyle());
   SetLineWidth(fShape->GetLineWidth());
   SetFillColor(fShape->GetFillColor());
   SetFillStyle(fShape->GetFillStyle());
 
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
void St_Node::Paint(Option_t *option)
{
  Error("Paint","Bug, this method  should not be called");
  return; 
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
 
   if (fVisibility && fShape->GetVisibility()) {
      fShape->Sizeof3D();
   }
   if ( TestBit(kSonsInvisible) ) return;
 
   if (!Nodes()) return;
   St_Node *node;
   TObject *obj;
   TIter  next(Nodes());
   while ((obj = next())) {
      node = (St_Node*)obj;
      node->Sizeof3D();
   }
}
 
#if 0
//_______________________________________________________________________
void St_Node::Streamer(TBuffer &b)
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
      b.WriteVersion(St_Node::IsA());
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
#endif

