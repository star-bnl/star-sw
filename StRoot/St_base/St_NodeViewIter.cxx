//*-- Author :    Valery Fine(fine@bnl.gov)   25/01/99  
// $Id: St_NodeViewIter.cxx,v 1.8 1999/04/23 00:09:20 fine Exp $
// $Log: St_NodeViewIter.cxx,v $
// Revision 1.8  1999/04/23 00:09:20  fine
// Working verion of PolyLineShape. Some correction for St_Node family as well
//
// Revision 1.7  1999/04/13 14:26:41  fine
// Geometry-based dataset implementation, next step
//
// Revision 1.6  1999/04/08 16:44:11  fine
// Working version of the NodeView family
//
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

/////////////////////////////////////////////////////////////////////////////////
//
//   St_NodeViewIter is a special class-iterator to
//   iterate over GEANT geometry dataset St_NodeView.
//   Class should provide a "standard" well-known 
//   "St_DataSetIter" interface to navigate and access 
//   the geometry information supplied by St_geant_Maker
//   as the St_Node object. Apart of the the base
//   St_DataSetIter this special class may supply
//   not only pointer to the selected dataset but some
//   "position" information (like translate vectors and
//   rotation matrice).
//
/////////////////////////////////////////////////////////////////////////////////

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
const St_NodePosition *St_NodeViewIter::GetPosition(Int_t level) const
{
  const St_NodePosition *pos = 0;
  if (m_Positions) {
    Int_t thisLevel = level;
    if (!thisLevel) thisLevel = fDepth;
    pos=(St_NodePosition *)m_Positions->At(thisLevel);
  }
  return pos;
}

//______________________________________________________________________________
St_NodePosition *St_NodeViewIter::operator[](Int_t level) const 
{
  const St_NodePosition *pos = GetPosition(level);
  if (pos) return new St_NodePosition(*pos);
  else {
     Error("operator[]"," GetPosition: %d %d %x", level,fDepth, m_Positions);
     return 0;
  }
}

//______________________________________________________________________________
void St_NodeViewIter::Notify(St_DataSet *set)
{
  if (!set) return;
  St_NodeView     *view         = (St_NodeView *) set;
  St_NodePosition *position     = 0;
  position = view->GetPosition();
  St_NodePosition *newPosition=UpdateTempMatrix(position);
}

//______________________________________________________________________________
St_NodePosition *St_NodeViewIter::UpdateTempMatrix(St_NodePosition *curPosition)
{
  // Pick the "old" position by pieces
  St_NodePosition *newPosition = 0;
  St_Node *curNode = 0;
  UInt_t curPositionId    = 0;
  if (curPosition) {
      curNode       = curPosition->GetNode();
      curPositionId = curPosition->GetId();
  }
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
  else if (curPosition)  {
         newPosition =  SetPositionAt(*curPosition);
//         printf(" new level %d %s\n",fDepth, curNode->GetName());
       }
       else 
         Error("UpdateTempMatrix","No position has been defined");
  if (newPosition) newPosition->SetId(curPositionId);
  return newPosition;
}

//______________________________________________________________________________
void St_NodeViewIter::ResetPosition(Int_t level, St_NodePosition *newPosition)
{
  Int_t thisLevel = level;
  if (!thisLevel) thisLevel = fDepth;
  St_NodePosition *thisPosition  =  (St_NodePosition *) GetPosition(level);
  if (newPosition) 
     *thisPosition =  *newPosition;
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

//______________________________________________________________________________
St_NodePosition *St_NodeViewIter::SetPositionAt(St_NodePosition &curPosition)
{
   if (!m_Positions)  m_Positions = new TObjArray(100);
   St_NodePosition *position =  (St_NodePosition *) m_Positions->At(fDepth);
   if (position) *position = curPosition;
   else {
      position = new St_NodePosition(curPosition);
      m_Positions->AddAtAndExpand(position,fDepth);
    }
   return position;
}


