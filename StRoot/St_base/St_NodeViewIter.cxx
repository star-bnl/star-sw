//*-- Author :    Valery Fine(fine@bnl.gov)   25/01/99  
//  
// 

#include "St_NodeViewIter.h"
#include "TClonesArray.h"
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
  Int_t l = level;
  if (!l) l = fDepth;
  St_NodePosition *pos = 0;
  St_NodePosition  null;
  if (m_Positions) pos=(St_NodePosition *)m_Positions->At(l);
  return pos;
}

//______________________________________________________________________________
St_NodePosition St_NodeViewIter::operator[](Int_t level)
{
  Int_t l = level;
  if (!l) l = fDepth;
  St_NodePosition *pos = GetPosition(l);
  St_NodePosition  null;
  if (pos) return *pos;
  else     return null;
}

//______________________________________________________________________________
void St_NodeViewIter::Notify(St_DataSet *set)
{
  St_NodeView     *view       = (St_NodeView *) set;
  St_NodePosition *position   = view->GetPosition();
  if (!m_Positions) m_Positions = new TClonesArray("St_NodePosition",100);
  St_NodePosition *newPosition=UpdateTempMatrix(position);
//  m_Positions->AddAtAndExpand(newPosition,fDepth);
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
    oldPosition = (St_NodePosition *)m_Positions->At(fDepth-1);
    oldMatrix = oldPosition->GetMatrix();
 
    Double_t oldTranslation[3];
    oldTranslation[0] = oldPosition->GetX();
    oldTranslation[1] = oldPosition->GetY();
    oldTranslation[2] = oldPosition->GetZ();

    // Pick the "current" position by pieces
    TRotMatrix *curMatrix        = curPosition->GetMatrix();
 
    // Create a new postion
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
      newPosition = new ((*m_Positions)[fDepth]) St_NodePosition(curNode
                                   ,newTranslation[0],newTranslation[1],newTranslation[2]
                                   ,new TRotMatrix(anum,"NodeView",newMatrix));
    }
    else {
       newTranslation[0] = oldTranslation[0] + curPosition->GetX();
       newTranslation[1] = oldTranslation[1] + curPosition->GetY();
       newTranslation[2] = oldTranslation[2] + curPosition->GetZ();
       newPosition = new ((*m_Positions)[fDepth])  St_NodePosition(curNode,newTranslation[0],newTranslation[1],newTranslation[2]);
    }
  }
  else 
      newPosition = new ((*m_Positions)[fDepth]) St_NodePosition(curNode);
  return newPosition;
}

//______________________________________________________________________________
void St_NodeViewIter::Reset(St_DataSet *l,Int_t depth)
{
  St_DataSetIter::Reset(l,depth);
}
