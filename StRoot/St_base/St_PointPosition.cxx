// Class St_PointPosition converts St_NodePostion objects
// into St_vector3 table one to be saved with MySQL data-base
// $Id: St_PointPosition.cxx,v 1.2 2000/01/12 18:07:24 fine Exp $
#include "St_DataSetIter.h"
#include "St_PointPosition.h"
#include "TRotMatrix.h"

ClassImp(St_PointPosition)

//_____________________________________________________________________________
St_PointPosition::St_PointPosition(const St_NodeView &viewNode)
{
  //
  // This ctor creates a St_PointPosition structure from the St_NodeView 
  // dataset
   St_NodePosition *pos = viewNode.GetPosition();
   if (pos)  MakePosition(*pos);
   St_DataSetIter next((St_DataSet *)&viewNode);
   St_NodeView *nextView = 0;
   while ( (nextView = (St_NodeView *)next()) )
         Add(new St_PointPosition(*nextView));
}

//_____________________________________________________________________________
St_PointPosition::St_PointPosition(const St_NodePosition &pos)
{
       MakePosition(pos);
}
//_____________________________________________________________________________
void St_PointPosition::MakePosition(const St_NodePosition &pos)
{
  St_Node *node = pos.GetNode();
  if (node) {
     Char_t buffer[10];
     TString nodeName = node->GetName();    
     sprintf(buffer,"#%d",pos.GetId());
     nodeName += buffer;
     SetName(nodeName);
  }
  TRotMatrix *matrix = pos.GetMatrix();
  Double_t *marray = 0;
  if (matrix) marray = matrix->GetMatrix();
  Int_t i = 0;
  if (marray) {
      ReAllocate(3);
      for (i=0;i<3;i++,marray += 3) AddAt(marray,i);
  }
  Double_t translation[3];
  if ( (translation[0] = pos.GetX()) || (translation[1] = pos.GetY()) ||
                                        (translation[2] = pos.GetZ()) )  
  {  ReAllocate();    AddAt(translation,i);  }
};

//_____________________________________________________________________________
void St_PointPosition::UpdatePosition(St_NodePosition *pos) {
 if (!pos) return;
 Int_t nRow = GetNRows();
 if (!nRow) return;
 TRotMatrix *m = 0;
 Int_t indx = 0;
 if (nRow >=3) { m = new TRotMatrix(" "," ",(Double_t *)GetTable(indx)); indx += 3; }
 pos->SetMatrix(m); 
 if (nRow - indx) {
   point3_st *translation = GetTable(indx);
   pos->SetPosition(translation->point[0],translation->point[1],translation->point[2]);
 }
}

//_____________________________________________________________________________
// $Log: St_PointPosition.cxx,v $
// Revision 1.2  2000/01/12 18:07:24  fine
// cvs symbols have been added and copyright class introduced
//
//_____________________________________________________________________________

