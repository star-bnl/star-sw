//*-- Author :    Valery Fine(fine@bnl.gov)   25/12/98 
// 
// 

#include "TBrowser.h"
#include "St_NodeView.h"
#include "St_NodePosition.h"
#include "TPadView3D.h"
#include "TGeometry.h"
#include "TVirtualPad.h"
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
St_NodeView::St_NodeView(St_Node &pattern,const St_NodePosition *nodePosition,EDataSetPass iopt)
            : St_ObjectSet(pattern.GetName(),(TObject *)nodePosition)
{
  //
  // Creates St_DataSet (clone) with a topology similar with St_DataSet *pattern
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
  
  St_NodePosition *position = 0;
  TIter next(pattern.GetListOfPositions());
  Bool_t optsel = (iopt == kStruct);
  Bool_t optall = (iopt == kAll);
  while (position = (St_NodePosition *)next()) {
    // define the the related St_Node
     St_Node *node = position->GetNode(); 
     if (node) {
       St_DataSet *parent = node->GetParent(); 
       if ( optall || (optsel && parent == (St_DataSet *)&pattern) )
                                  Add(new St_NodeView(*node,position));
     }
  }
}
//_____________________________________________________________________________
void St_NodeView::Browse(TBrowser *b){
  St_ObjectSet::Browse(b);
  St_NodePosition *pos = GetPosition();
  if (pos) pos->Browse(b);
//    b->Add(pos); 
}

//_____________________________________________________________________________
St_Node *St_NodeView::GetNode(){ 
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
 
  TPadView3D *view3D=gPad->GetView3D();

//  while (nextView = (St_NodeView *) next()) {
   St_Node *thisNode  = GetNode();
   St_NodePosition *position = 0;
   TShape  *shape = 0;
   if (thisNode) {
       shape    = thisNode->GetShape();
       position = GetPosition();
   }
    
//*-*- Update translation vector and rotation matrix for new level
   if (gGeometry->GeomLevel()) {
      gGeometry->UpdateTempMatrix(position->GetX(),position->GetY(),position->GetZ()
                                 ,position->GetMatrix()->GetMatrix()
                                 ,position->GetMatrix()->IsReflection());
      if (view3D)
         view3D->UpdatePosition(position->GetX(),position->GetY(),position->GetZ(),position->GetMatrix());
   }
 
   Int_t nsons = 0;
   TList *fNodes =  GetList();
   if (fNodes) nsons = fNodes->GetSize();
 
   thisNode->TAttLine::Modify();
   thisNode->TAttFill::Modify();
   if (thisNode->GetVisibility() && shape->GetVisibility()) {
//      gNode = thisNode;
      shape->SetLineColor(thisNode->GetLineColor());
      shape->SetLineStyle(thisNode->GetLineStyle());
      shape->SetLineWidth(thisNode->GetLineWidth());
      shape->SetFillColor(thisNode->GetFillColor());
      shape->SetFillStyle(thisNode->GetFillStyle());
      if (view3D)
           view3D->SetLineAttr(thisNode->GetLineColor(),thisNode->GetLineWidth(),option);
      shape->Paint(option);
   }
////---   if ( thisNode->TestBit(kSonsInvisible) ) return;
 
//*-*- Paint all sons
   if(!nsons) return;
 
   gGeometry->PushLevel();
   St_Node *node;
   TObject *obj;
   TIter  next(fNodes);
   while ((obj = next())) {
      if (view3D)
          view3D->PushMatrix();
 
      node = (St_Node*)obj;
      node->Paint(option);
      if (view3D)
          view3D->PopMatrix();
   }
   gGeometry->PopLevel();
 
}

