//*-- Author :    Valery Fine(fine@bnl.gov)   25/12/98 
// $Id: St_NodeView.h,v 1.19 1999/07/09 17:52:56 fine Exp $
// $Log: St_NodeView.h,v $
// Revision 1.19  1999/07/09 17:52:56  fine
// New ctors to create sub-view between two nodes
//
// Revision 1.18  1999/07/09 01:56:39  fine
// New method to contrsuct sub views and manage visibilities
//
// Revision 1.17  1999/06/21 22:16:55  fine
// Some clean up
//
// Revision 1.16  1999/06/14 09:30:31  fine
//   default ctor clean
//
// Revision 1.15  1999/06/14 08:45:28  fine
// List of the shapes have been introduced for St_NodeView
//
// Revision 1.14  1999/06/05 00:42:32  fine
// SetLineAttribute methods have been introduced
//
// Revision 1.13  1999/05/29 20:52:33  fine
// Several method to estimat range of 3D object were introduced
//
// Revision 1.12  1999/04/23 22:47:36  fine
// Node family has been adjusted for St_PolyLineShape class
//
// Revision 1.11  1999/04/23 00:09:19  fine
// Working verion of PolyLineShape. Some correction for St_Node family as well
//
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
protected:
  TList          *fListOfShapes;     //Pointer to the list of the "extra" shape definitions
//  TList          *fListOfAttributes;  //Pointer to the list of this node attributes
//  St_Node        *fAttributes;

  virtual void    PaintShape(Option_t *option);

public:
  St_NodeView():St_ObjectSet(),fListOfShapes(0) {;}
  St_NodeView(St_NodeView *viewNode,St_NodePosition *nodePosition=0);
  St_NodeView(St_NodeView *viewNode,const Char_t *NodeName1,const Char_t *NodeName2=0);
  St_NodeView(St_NodeView *viewNode,St_NodeView *topNode);
  St_NodeView(St_NodeView *viewNode,const St_NodeView *node1,const St_NodeView *node2);
  St_NodeView(St_Node &pattern,const St_NodePosition *nodePosition=0,EDataSetPass iopt=kAll,Int_t level=0);
  St_NodeView(Double_t *translate, Double_t *rotate, UInt_t positionId, St_Node *thisNode,
              const Char_t *thisNodePath, const Char_t *matrixName=0, const Int_t matrixType=0);
  St_NodeView(St_Node *thisNode,St_NodePosition *nodePosition);
  virtual ~St_NodeView();
  virtual St_Node *AddNode(St_Node *node);
  virtual void     Add(St_NodeView *node);
  virtual void     Add(TShape *shape, Bool_t IsMaster=kFALSE);
  virtual void     Browse(TBrowser *b);
  virtual void     Draw(Option_t *option=""); // *MENU* 
  virtual Int_t    DistancetoPrimitive(Int_t px, Int_t py);
//  virtual St_Node *GetAttributes() const;
  virtual St_NodePosition *GetPosition() const { return (St_NodePosition *)GetObject();}
  virtual St_Node *GetNode() const ; 
  virtual Int_t    GetGlobalRange(const St_NodeView *rootNode,Float_t *min, Float_t *max);
  virtual TList   *GetListOfShapes()      const;
//  virtual TList   *GetListOfAttributes()  const;
  virtual void     GetLocalRange(Float_t *min, Float_t *max);
  virtual TShape  *GetShape()  const;
//  virtual St_Node *GetThisAttributes() const;
  virtual Int_t    GetVisibility() const;
  virtual Bool_t   IsMarked();
  virtual Bool_t   Is3D()  {return kTRUE;}
  virtual TList   *Nodes(){ return GetList();}
  virtual void     Paint(Option_t *option="");
  virtual TString  PathP() const;
  virtual void     SetLineAttributes(); // *MENU*
  virtual void     SavePrimitive(ofstream &out, Option_t *option="");
  virtual void     SetVisibility(Int_t vis=1); // *MENU*
  virtual void     Sizeof3D() const;
  ClassDef(St_NodeView,1)
};

inline void    St_NodeView::Add(St_NodeView *node){ St_DataSet::Add(node);}
inline Bool_t  St_NodeView::IsMarked(){ return TestBit(kMark); }
inline TList  *St_NodeView::GetListOfShapes() const {return fListOfShapes;}
// inline TList  *St_NodeView::GetListOfAttributes() const {return fListOfAttributes;}
inline TShape *St_NodeView::GetShape()  const 
       {return fListOfShapes ? (TShape *)fListOfShapes->First():0;}
inline Int_t   St_NodeView::GetVisibility() const {return GetNode() ? GetNode()->GetVisibility():0;}
// inline St_Node *St_NodeView::GetThisAttributes() const {return fAttributes;}

#endif

