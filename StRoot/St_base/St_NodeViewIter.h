//*-- Author :    Valery Fine(fine@bnl.gov)   25/01/99  
// $Id: St_NodeViewIter.h,v 1.8 1999/09/23 18:31:32 fine Exp $
// $Log: St_NodeViewIter.h,v $
// Revision 1.8  1999/09/23 18:31:32  fine
// CVS id adjusted
//

#ifndef STAR_St_NodeViewIter
#define STAR_St_NodeViewIter

#include "St_DataSetIter.h" 
#include "St_NodePosition.h"

class TObjArray;
class St_NodeView;

class St_NodeViewIter : public St_DataSetIter {
private:
protected:
     friend class St_NodeView;
     TObjArray    *m_Positions; // the array of the Node position in the absolute system 
     virtual const St_NodePosition *GetPosition(Int_t level=0) const;
     virtual St_NodePosition *SetPositionAt(St_Node *node,Double_t x=0, Double_t y=0, Double_t z=0, TRotMatrix *matrix=0);
     virtual St_NodePosition *SetPositionAt(St_NodePosition &curPosition);
public:
     St_NodeViewIter(St_NodeView *view, Int_t depth=1, Bool_t dir=kIterForward);
    ~St_NodeViewIter();
     virtual void            Notify(St_DataSet *set);
     virtual void            Reset(St_DataSet *l=0,Int_t depth=0);

     St_NodePosition        *operator[](Int_t level) const ;

     St_NodePosition        *UpdateTempMatrix(St_NodePosition *curPosition);
     void                    ResetPosition(Int_t level=0, St_NodePosition *newPosition=0);
     ClassDef(St_NodeViewIter,0)
};


#endif 

