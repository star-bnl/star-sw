//*-- Author :    Valery Fine(fine@bnl.gov)   25/01/99  
// $Id: St_NodeViewIter.cxx,v 1.5 1999/04/05 03:18:28 fine Exp $
// $Log: St_NodeViewIter.cxx,v $
// Revision 1.5  1999/04/05 03:18:28  fine
// St_Node family steps
//
// Revision 1.4  1999/04/02 23:36:05  fine
// Collapsed geometry structures has been implemeted
//
// Revision 1.3  1999/03/29 19:25:26  fine
// Visibility flag have been activated. Some working correction
//

#include "St_NodeViewIter.h"
// #include "TClonesArray.h"
#include "TObjArray.h"
#include "St_NodeView.h"
#include "St_DataSetIter.h"
#include "TGeometry.h"

ClassImp(St_NodeViewIter)
//______________________________________________________________________________
St_NodeViewIter::St_NodeViewIter(St_NodeView *view, Int_t depth, Bool_t dir):
           St_DataSetIter(view,depth,dir),m_Positions(0)
{ }

//______________________________________________________________________________
St_NodeViewIter::~St_NodeViewIter()
{
  if (m_Positions) { m_Positions->Delete(); delete m_Positions; }
}
//______________________________________________________________________________
St_NodePosition *St_NodeViewIter::GetPosition(Int_t level)
{
  St_NodePosition *pos = 0;
  if (m_Positions) {
    Int_t thisLevel = level;
    if (!thisLevel) thisLevel = fDepth;
    pos=(St_NodePosition *)m_Positions->At(thisLevel);
  }
  return pos;
}

//______________________________________________________________________________
St_NodePosition St_NodeViewIter::operator[](Int_t level)
{
  Int_t thisLevel = level;
  if (!thisLevel) thisLevel = fDepth;
  St_NodePosition *pos = GetPosition(thisLevel);
  St_NodePosition  null;
  if (pos) return *pos;
  else {
     printf(" GetPostion: %d \n", thisLevel);
     return null;
  }
}

//______________________________________________________________________________
void St_NodeViewIter::Notify(St_DataSet *set)
{
  St_NodeView     *view         = (St_NodeView *) set;
  St_NodePosition *position     = view->GetPosition();
  St_NodePosition *newPosition=UpdateTempMatrix(position);
}

//______________________________________________________________________________
St_NodePosition *St_NodeViewIter::UpdateTempMatrix(St_NodePosition *curPosition)
{
  // Pick the "old" position by pieces
  St_NodePosition *newPosition = 0;
  St_Node *curNode = 0;
  if (curPosition) curNode = curPosition->GetNode();
  if (fDepth-1) {
    St_NodePosition *oldPosition = 0;
    TRotMatrix *oldMatrix = 0;
    oldPosition = m_Positions ? (St_NodePosition *)m_Positions->At(fDepth-1):0;
    Double_t oldTranslation[] = { 0, 0, 0 };
    if (oldPosition) 
    {
      oldMatrix         = oldPosition->GetMatrix();
 
      oldTranslation[0] = oldPosition->GetX();
      oldTranslation[1] = oldPosition->GetY();
      oldTranslation[2] = oldPosition->GetZ();
    }

    // Pick the "current" position by pieces
    TRotMatrix *curMatrix        = curPosition->GetMatrix();
 
    // Create a new position
    Double_t newTranslation[3];
    Double_t newMatrix[9];

    if(oldMatrix)
    {
      TGeometry::UpdateTempMatrix(oldTranslation,oldMatrix->GetMatrix()
                       ,curPosition->GetX(),curPosition->GetY(),curPosition->GetZ(),curMatrix->GetMatrix()
                       ,newTranslation,newMatrix);
      Int_t num = gGeometry->GetListOfMatrices()->GetSize();
      Char_t anum[100];
      sprintf(anum,"%d",num+1);
      newPosition = SetPositionAt(curNode
                                ,newTranslation[0],newTranslation[1],newTranslation[2]
                                ,new TRotMatrix(anum,"NodeView",newMatrix));
    }
    else {
       newTranslation[0] = oldTranslation[0] + curPosition->GetX();
       newTranslation[1] = oldTranslation[1] + curPosition->GetY();
       newTranslation[2] = oldTranslation[2] + curPosition->GetZ();
       newPosition = SetPositionAt(curNode,newTranslation[0],newTranslation[1],newTranslation[2]);
    }
  }
  else
       newPosition =  SetPositionAt(curNode);
  return newPosition;
}

//______________________________________________________________________________
void St_NodeViewIter::ResetPosition(Int_t level, St_NodePosition *newPosition)
{
  Int_t thisLevel = level;
  if (!thisLevel) thisLevel = fDepth;
  St_NodePosition *thisPosition  =  GetPosition(level);
  if (thisPosition) delete thisPosition;
  thisPosition = 0;
  if (newPosition) 
     thisPosition =  
#ifdef Clone
                new ((*m_Positions)[thisLevel]) St_NodePosition(*newPosition);
#else
//               delete (*m_Positions)[thisLevel]
                   new  St_NodePosition(*newPosition);
#endif
}
//______________________________________________________________________________
void St_NodeViewIter::Reset(St_DataSet *l,Int_t depth)
{
  St_DataSetIter::Reset(l,depth);
}

//______________________________________________________________________________
St_NodePosition *St_NodeViewIter::SetPositionAt(St_Node *node,Double_t x, Double_t y, Double_t z, TRotMatrix *matrix)
{
   if (!m_Positions)  m_Positions = new TObjArray(100);
   St_NodePosition *position =  (St_NodePosition *) m_Positions->At(fDepth);
   if (position) position->Reset(node,x,y,z,matrix);
   else {
      position = new St_NodePosition(node,x,y,z,matrix);
      m_Positions->AddAtAndExpand(position,fDepth);
    }
   return position;
}



