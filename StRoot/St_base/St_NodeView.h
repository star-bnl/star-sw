//*-- Author :    Valery Fine(fine@bnl.gov)   25/12/98 
// $Id: St_NodeView.h,v 1.10 1999/04/13 14:26:41 fine Exp $
// $Log: St_NodeView.h,v $
// Revision 1.10  1999/04/13 14:26:41  fine
// Geometry-based dataset implementation, next step
//
// Revision 1.9  1999/04/10 15:05:05  fine
// First working version of SavePrimitive. New ctor has been introduced to back SavePrimitive
//
// Revision 1.8  1999/04/09 23:24:08  fine
// St_NodeView::SavePrimitive() - first approach
//
// Revision 1.7  1999/04/08 16:44:10  fine
// Working version of the NodeView family
//
// Revision 1.6  1999/03/30 16:24:02  fine
//  Corrections towards GEANT manager
//
// Revision 1.5  1999/03/29 19:25:26  fine
// Visibility flag have been activated. Some working correction
//
#ifndef STAR_St_NodeView
#define STAR_St_NodeView

#include "St_Node.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_NodeView                                                         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

class St_NodeView : public St_ObjectSet {

public:
  St_NodeView():St_ObjectSet(){;}
  St_NodeView(St_NodeView *viewNode,St_NodePosition *nodePosition=0);
  St_NodeView(St_Node &pattern,const St_NodePosition *nodePosition=0,EDataSetPass iopt=kAll,Int_t level=0);
  St_NodeView(Double_t *translate, Double_t *rotate, UInt_t positionId, St_Node *thisNode,
              const Char_t *thisNodePath, const Char_t *matrixName=0, const Int_t matrixType=0);
  St_NodeView(St_Node *thisNode,St_NodePosition *nodePosition);
  virtual ~St_NodeView();
  virtual void Browse(TBrowser *b);
  virtual void Draw(Option_t *option); // *MENU* 
  virtual Int_t DistancetoPrimitive(Int_t px, Int_t py);
  virtual Bool_t IsMarked();
  virtual St_NodePosition *GetPosition() const { return (St_NodePosition *)GetObject();}
  virtual St_Node         *GetNode() const ;
  virtual void             Paint(Option_t *option="");
  virtual TString          PathP() const;
  virtual void             SavePrimitive(ofstream &out, Option_t *option="");
  ClassDef(St_NodeView,1)
};

inline Bool_t St_NodeView::IsMarked(){ return TestBit(kMark); }
#endif

