//*-- Author :    Valery Fine(fine@bnl.gov)   25/12/98  
// $Id: St_Node.h,v 1.13 1999/05/13 19:57:27 fine Exp $
// $Log: St_Node.h,v $
// Revision 1.13  1999/05/13 19:57:27  fine
// St_Node  the TShape list has been introduced, St_Node file format has been changed
//
// Revision 1.12  1999/04/19 00:05:15  fine
// New class St_PolylineShape has been introduced
//
// Revision 1.11  1999/04/13 14:26:40  fine
// Geometry-based dataset implementation, next step
//
// Revision 1.10  1999/04/08 16:44:09  fine
// Working version of the NodeView family
//
// Revision 1.9  1999/04/05 03:18:26  fine
// St_Node family steps
//
// Revision 1.8  1999/02/04 19:22:23  fine
// Severak drawing method have been added to draw STAR nodes
//
// Revision 1.7  1999/01/31 02:03:07  fine
// St_DataSetIter::Notify - new method + clean up
//
// Revision 1.6  1999/01/30 04:24:21  fine
// St_Table: Print memory leak fixed
//
// Revision 1.5  1999/01/25 23:36:41  fine
// St_DataSet fixed, St_DataSetIter::operator[] introduced, St_Node with own Hash methdo
//
// Revision 1.4  1998/12/27 04:16:44  fine
// *** empty log message ***
//
// Revision 1.2  1998/12/26 21:40:40  fisyak
// Add Id and Log
//
//Copyright (C) 1998. Brookhaven National Laboratory. Valery Fine (Faine). All right reserved.
 
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_Node                                                              //
//                                                                      //
// Description of parameters to position a 3-D geometry object          //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
 
#ifndef ROOT_St_Node
#define ROOT_St_Node

#include "St_ObjectSet.h" 

#include <TNode.h>

#ifndef ROOT_TShape
//*KEEP,TShape.
#include "TShape.h"
//*KEND.
#endif
#ifndef ROOT_TAttLine
//*KEEP,TAttLine.
#include "TAttLine.h"
//*KEND.
#endif
#ifndef ROOT_TAttFill
//*KEEP,TAttFill.
#include "TAttFill.h"
//*KEND.
#endif
 
class TBrowser;
class St_NodePosition;
class TRotMatrix;
class TList;

class St_Node  : public St_ObjectSet, public TAttLine, public TAttFill { 
 protected:
   TShape         *fShape;         //Pointer to "master" shape definition
   TList          *fListOfShapes;  //Pointer to the list of the shape definitions
   TString         fOption;        //List of options if any
   Int_t           fVisibility;    //Visibility flag

   virtual void             Add(St_NodePosition *position);
   virtual St_NodePosition *Add(St_Node *node, St_NodePosition *nodePosition);
   virtual Int_t            DistancetoNodePrimitive(Int_t px, Int_t py,St_NodePosition *position=0);
           void             SetPositionsList(TList *list=0){AddObject((TObject *)list);}
   virtual void             PaintNodePosition(Option_t *option="",St_NodePosition *postion=0);
   friend class St_PolyLineShape;
 public:
        St_Node();
        St_Node(const Text_t *name, const Text_t *title, const Text_t *shapename, Option_t *option="");
        St_Node(const Text_t *name, const Text_t *title, TShape *shape, Option_t *option="");
        St_Node(TNode &node);
        virtual ~St_Node();
        virtual St_NodePosition *Add(St_Node *node, Double_t x=0, Double_t y=0, Double_t z=0, TRotMatrix *matrix=0, UInt_t id=0, Option_t *option="");
        virtual St_NodePosition *Add(St_Node *node, Double_t x, Double_t y, Double_t z,  const Text_t *matrixname,  UInt_t id=0, Option_t *option="");
        virtual void        Add(TShape *shape, Bool_t IsMaster=kFALSE);
        virtual void        Browse(TBrowser *b);
        virtual TNode      *CreateTNode(const St_NodePosition *position=0);
        virtual void        DeletePosition(St_NodePosition *position);
        virtual Int_t       DistancetoPrimitive(Int_t px, Int_t py);
        virtual void        Draw(Option_t *option=""); // *MENU*
        virtual void        DrawOnly(Option_t *option="");
        virtual void        ExecuteEvent(Int_t event, Int_t px, Int_t py);
        static  TRotMatrix *GetIdentity();
        virtual Text_t     *GetObjectInfo(Int_t px, Int_t py);
        const   Option_t   *GetOption() const { return fOption.Data();}
                TShape     *GetShape()  const {return fShape;}
                TList      *GetListOfShapes()  const {return fListOfShapes;}
        Int_t               GetVisibility() const {return fVisibility;}
        virtual TList      *GetListOfPositions() { return (TList *)(GetObject());}
        virtual ULong_t     Hash() { return TObject::Hash();}
        virtual void        ImportShapeAttributes();
        virtual Bool_t      IsMarked();
        virtual Bool_t      Is3D()  {return kTRUE;}
        virtual TList      *Nodes() const { return GetList(); }
        virtual void        Paint(Option_t *option="");
        virtual void        PaintShape(Option_t *option="");
        virtual void        SetVisibility(Int_t vis=1); // *MENU*
        virtual void        Sizeof3D() const;
 
        ClassDef(St_Node,1)  //Description of parameters to position a 3-D geometry object
};

inline Bool_t St_Node::IsMarked(){ return TestBit(kMark); }
 
#endif
