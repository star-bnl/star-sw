//*-- Author :    Valery Fine(fine@bnl.gov)   25/12/98 
// 
// 

#include <fstream.h>
#include <iostream.h>
#include <iomanip.h>

#include "TBrowser.h"
#include "St_NodeView.h"
#include "St_NodeViewIter.h"
#include "St_NodePosition.h"
#include "TROOT.h"
#include "TView.h"
#include "TPadView3D.h"
#include "TGeometry.h"
#include "TVirtualPad.h"
#include "TObjArray.h"
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_NodeView                                                          //
//                                                                      //
// St_NodeView class is a special kind of St_DataSet with one extra     //
// pointer to wrap any TObject onto St_DataSet object                   //
//                                                                      //
//  BE CAREFUL !!!                                                      //
//  One has to use it carefully no conrol over that extra object        //
//  is performed. This means: the onject m_Obj data-member points to can//
//  be destroyed with no this kbject notifying.                         //
//  There is no tool /protection to check whether m_Obj is till alive.  //
//  It is one's  code responsilitiy                                     //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(St_NodeView)
//_____________________________________________________________________________
St_NodeView::St_NodeView(St_NodeView *viewNode,St_NodePosition *nodePosition)
            : St_ObjectSet(viewNode->GetName(),(TObject *)nodePosition)
{
  //
  // This ctor creates a St_NodeView structure from the "marked" nodes
  // of the "viewNode" input structure
  // It re-calculates all positions according of the new topology
  //
  if (viewNode) 
  {
     SetTitle(viewNode->GetTitle());
     EDataSetPass mode = kContinue;
     St_NodeViewIter next(viewNode,0);
     St_NodeView *nextView = 0;
     while (nextView = (St_NodeView *)next(mode)){     
       mode = kContinue;
       if (nextView->IsMarked()) {
         St_NodePosition *position = next[0];
         if (!position->GetNode()) {
             Error("St_NodeView ctor","%s %s ",GetName(),nextView->GetName());
         }
         Add(new St_NodeView(nextView,position));
         mode = kPrune;
       }
    }
  }
}

//_____________________________________________________________________________
St_NodeView::St_NodeView(St_Node &pattern,const St_NodePosition *nodePosition,EDataSetPass iopt,Int_t level)
            : St_ObjectSet(pattern.GetName(),(TObject *)nodePosition)
{
  //
  // Creates St_NodeView (view) with a topology similar with St_Node *pattern
  //
  //  Parameters:
  //  -----------
  //  pattern        - the pattern dataset
  //  iopt = kStruct - clone only my structural links
  //         kAll    - clone all links
  //         kRefs   - clone only refs
  //         kMarked - clone marked (not implemented yet) only
  //
  //   All new-created sets become the structural ones anyway.
  //

  //  cout << "ctor for " << GetName() << " - " << GetTitle() << endl;
  SetTitle(pattern.GetTitle());
  if ( pattern.IsMarked() ) Mark(); 
  St_NodePosition *position = 0;
  const TList *list = pattern.GetListOfPositions();
  if (!list) return;

  TIter next(list);
  Bool_t optSel    = (iopt == kStruct);
  Bool_t optAll    = (iopt == kAll);
  Bool_t optMarked = (iopt == kMarked);
  Int_t thisLevel = level + 1;
  while (position = (St_NodePosition *)next()) {
    // define the the related St_Node
     St_Node *node = position->GetNode(); 
     if (node) {
       if (optMarked && !node->IsMarked()) continue;

       if (optSel) {
            St_DataSet *parent = node->GetParent(); 
            if ( parent && (parent != (St_DataSet *)&pattern) ) continue;
       }

       Add(new St_NodeView(*node,position,iopt,thisLevel));       
     }
     else 
       Error("St_NodeView ctor","Position with NO node attached has been supplied");
     
  }
}

//_____________________________________________________________________________
St_NodeView::St_NodeView(Double_t *translate, Double_t *rotate, UInt_t positionId, St_Node *topNode,
                         const Char_t *thisNodePath, const Char_t *matrixName, const Int_t matrixType)
{
  // Special ctor to back St_NodeView::SavePrimitive() method
  St_Node *thisNode = 0;
  Double_t thisX  = translate[0]; 
  Double_t thisY  = translate[1];
  Double_t thisZ  = translate[2];

  // Find St_Node by path; 
  if (topNode) { 
    thisNode =  (St_Node *)topNode->Find(thisNodePath);     
    if (!thisNode->InheritsFrom("St_Node")) { 
           thisNode = 0; 
           fprintf(stderr,"Error wrong node <%s> on path: \"%s\"\n",thisNode->GetName(),thisNodePath); 
    } 
  } 

  TRotMatrix *thisRotMatrix =  0;
  if (matrixName && strlen(matrixName)) thisRotMatrix = gGeometry->GetRotMatrix(matrixName); 
  St_NodePosition *thisPosition = 0; 
  if (thisRotMatrix)  
      thisPosition = new St_NodePosition(thisNode,thisX, thisY, thisZ, matrixName); 
  else if (matrixType==2)  
      thisPosition = new St_NodePosition(thisNode,thisX, thisY, thisZ); 
  else if (rotate) {
       const Char_t *title = "rotation";
      thisRotMatrix = new TRotMatrix((Text_t *)matrixName,(Text_t *)title,rotate);
      thisPosition  = new St_NodePosition(thisNode,thisX, thisY, thisZ, thisRotMatrix); 
  }
  else
       Error("St_NodeView"," No rotation matrix is defined");
  thisPosition->SetId(positionId);
  SetObject(thisPosition);
  if (thisNode) {
    SetName(thisNode->GetName());
    SetTitle(thisNode->GetTitle());
  }
}

//_____________________________________________________________________________
St_NodeView::St_NodeView(St_Node *thisNode,St_NodePosition *nodePosition)
            : St_ObjectSet(thisNode->GetName(),(TObject *)nodePosition)
{
  if (thisNode) 
     SetTitle(thisNode->GetTitle());
}

//______________________________________________________________________________
St_NodeView::~St_NodeView()
{
// default dtor (empty for this class)
}

//_____________________________________________________________________________
void St_NodeView::Browse(TBrowser *b){
  St_ObjectSet::Browse(b);
//  St_NodePosition *pos = GetPosition();
//  if (pos) pos->Browse(b);
//    b->Add(pos); 
}

//______________________________________________________________________________
Int_t St_NodeView::DistancetoPrimitive(Int_t px, Int_t py)
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

   Int_t dist = big;
 
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
 
   St_NodePosition *position = GetPosition();
   St_Node *thisNode  = 0;
   TShape  *thisShape = 0;
   if (position) {
     thisNode = position->GetNode();     
     position->UpdatePosition(); 
     if (thisNode) {     
        thisShape    = thisNode->GetShape();
        if (thisNode->GetVisibility() && thisShape && thisShape->GetVisibility()) {
          dist = thisShape->DistancetoPrimitive(px,py);
          if (dist < maxdist) {
             gPad->SetSelected(this);
             return 0;
          }
       }
     }
   }
//   if ( TestBit(kSonsInvisible) ) return dist;
 
//*-*- Loop on all sons
   TList *fNodes =  GetList();
   Int_t nsons = fNodes?fNodes->GetSize():0;
   Int_t dnode = dist;
   if (nsons) {
      gGeometry->PushLevel();
      St_Node *node;
      TIter  next(fNodes);
      while ((node  = (St_Node *)next())) {
         dnode = node->DistancetoPrimitive(px,py);
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
void St_NodeView::Draw(Option_t *option)
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

//_____________________________________________________________________________
St_Node *St_NodeView::GetNode() const { 
  St_NodePosition *pos = GetPosition();
  if (pos)
    return pos->GetNode();
  return 0;
}

//______________________________________________________________________________
void St_NodeView::Paint(Option_t *option)
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

// restrict the levels for "range" option
  Int_t level = gGeometry->GeomLevel();
//  if (option && option[0]=='r' && level > 3 && strcmp(option,"range") == 0) return;
//  if (option && option[0]=='r' && level > 3 ) return;

  TPadView3D *view3D=gPad->GetView3D();

  St_Node *thisNode  = 0;
  St_NodePosition *position = GetPosition();
  
  // UpdatePosition does change the current matrix and it MUST be called FIRST !!!
  if (position) {
     thisNode  = position->GetNode();
     position->UpdatePosition(option);      
  }

  if (thisNode)  thisNode->PaintShape(option);  
////---   if ( thisNode->TestBit(kSonsInvisible) ) return;
 
//*-*- Paint all sons
  TList *fNodes =  GetList();
  Int_t nsons = fNodes?fNodes->GetSize():0;

  if(!nsons) return;
 
  gGeometry->PushLevel();
  St_NodeView *node;
  TIter  next(fNodes);
  while ((node = (St_NodeView *)next())) {
     if (view3D)  view3D->PushMatrix();

     node->Paint(option);

     if (view3D)  view3D->PopMatrix();
  }
  gGeometry->PopLevel();
}
//______________________________________________________________________________
TString St_NodeView::PathP() const
{
 // return the full path of this data set 
   TString str;
   St_NodeView *parent = (St_NodeView *)GetParent();
   if (parent) { 
       str = parent->PathP();
       str += "/";
   }
   str +=  GetName();
   UInt_t positionId = 0;
   St_NodePosition *p = GetPosition();
   if (p) {
      char buffer[10];
      positionId = p->GetId();
      sprintf(buffer,"%d",p->GetId());
      str +=  buffer;
   }
   return str;
}
//_______________________________________________________________________
void St_NodeView::SavePrimitive(ofstream &out, Option_t *option)
{
const Char_t *sceleton[] = {
   "St_NodeView *CreateNodeView(St_Node *topNode) {"
  ,"  TString     thisNodePath   = "
  ,"  UInt_t      thisPositionId = "
  ,"  Double_t thisTranslate[3]  = "
  ," "
  ,"  TString        matrixName  = " 
  ,"  Int_t          matrixType  = "
  ,"  Double_t     thisMatrix[]  = {  "
  ,"                                  "
  ,"                                  "
  ,"                               };"
  ,"  return = new St_NodeView(thisTranslate, thisMatrix, thisPositionId, topNode,"
  ,"                          thisNodePath.Data(),matrixName.Data(), matrixType);"
  ,"}"
  };
//------------------- end of sceleton ---------------------
  Int_t sceletonSize = sizeof(sceleton)/4;
  St_NodePosition *thisPosition = GetPosition();
  St_Node *thisFullNode = GetNode();
  TString thisNodePath = thisFullNode ? thisFullNode->Path() : TString("");
  // Define position
  UInt_t thisPositionId = thisPosition ? thisPosition->GetId():0;
  Double_t thisX  = thisPosition ? thisPosition->GetX():0;
  Double_t thisY  = thisPosition ? thisPosition->GetY():0;
  Double_t thisZ  = thisPosition ? thisPosition->GetZ():0;

  TRotMatrix *matrix = thisPosition ? thisPosition->GetMatrix():0;
  Int_t matrixType = 2;
  TString matrixName = " ";
  Double_t thisMatrix[] = { 0,0,0, 0,0,0, 0,0,0 };
  if (matrix) {
     matrixName = matrix->GetName();
     memcpy(thisMatrix,matrix->GetMatrix(),9*sizeof(Double_t));
     matrixType = matrix->GetType();
  }
  Int_t im = 0;
  for (Int_t lineNumber =0; lineNumber < sceletonSize; lineNumber++) {
    out << sceleton[lineNumber];                             // cout << lineNumber << ". " << sceleton[lineNumber];
    switch (lineNumber) {
    case  1:  out  << "\"" << thisNodePath.Data() << "\";" ; // cout  << "\"" << thisNodePath.Data() << "\";" ;
       break;
    case  2:  out   << thisPositionId << ";" ; // cout  << "\"" << thisNodePath.Data() << "\";" ;
       break;
    case  3:  out << "{" << thisX << ", " << thisY << ", "<< thisZ << "};";  // cout << thisX << ";" ;
       break;
    case  5:  out << "\"" << matrixName << "\";" ;           // cout << "\"" << matrixName << "\";" ;
       break;
    case  6:  out <<  matrixType << ";" ;                    // cout <<  matrixType << ";" ;
       break;
    case  7:  out << thisMatrix[im++] << ", " << thisMatrix[im++] << ", " << thisMatrix[im++]  << ", " ;
       break; 
    case  8:  out << thisMatrix[im++] << ", " << thisMatrix[im++] << ", " << thisMatrix[im++]  << ", " ;
       break;
    case  9:  out << thisMatrix[im++] << ", " << thisMatrix[im++] << ", " << thisMatrix[im++] ;
       break;
    default:
       break;
   };
//   cout << " " << endl;
   out << " " << endl;
 }
}
