#ifndef STAF_St_PointPosition
#define STAF_St_PointPosition

#include "St_point3_Table.h"
#include "St_NodePosition.h"
#include "St_NodeView.h"
// $Id: St_PointPosition.h,v 1.1 1999/12/29 20:44:19 fine Exp $
class St_PointPosition : public St_point3 {
 protected:
 public:
   St_PointPosition(){;}
   St_PointPosition(const St_NodeView &viewNode);
   St_PointPosition(const St_NodePosition &pos);
   void MakePosition(const St_NodePosition &pos);
   void UpdatePosition(St_NodePosition *pos);
   void Print(Int_t n = 4);   //*MENU
   ClassDef(St_PointPosition,0) // class table to define the geometry 3D positions
};

inline void St_PointPosition::Print(Int_t n){ St_Table::Print(0,n);}
// $Log: St_PointPosition.h,v $
// Revision 1.1  1999/12/29 20:44:19  fine
// New set of classes to provide update detector geometry from Db
//
#endif
