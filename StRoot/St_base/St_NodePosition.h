//*-- Author :    Valery Fine(fine@bnl.gov)   25/12/98   
// $Id: St_NodePosition.h,v 1.5 1999/02/04 19:22:23 fine Exp $
// $Log: St_NodePosition.h,v $
// Revision 1.5  1999/02/04 19:22:23  fine
// Severak drawing method have been added to draw STAR nodes
//
// Revision 1.4  1999/01/31 02:03:07  fine
// St_DataSetIter::Notify - new method + clean up
//
// Revision 1.3  1998/12/27 02:33:17  fine
// St_Node, St_NodePosition - first working versions have been introduced *see macros/STAR_shapes.C for an example)
//
// Revision 1.2  1998/12/26 21:40:40  fisyak
// Add Id and Log
// 
//+SEQ,CopyRight,T=NOINCLUDE.
 
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_NodePosition                                                       //
//                                                                      //
// Description of parameters to position a 3-D geometry object          //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
 
#ifndef ROOT_St_NodePosition
#define ROOT_St_NodePosition

#include <TClass.h>
#include "St_Node.h"
  
class TBrowser;
class TRotMatrix;
  
class St_NodePosition  : public TObject {
 protected:
   Double_t        fX;           //X offset with respect to parent object
   Double_t        fY;           //Y offset with respect to parent object
   Double_t        fZ;           //Z offset with respect to parent object
   TRotMatrix     *fMatrix;      //Pointer to rotation matrix
   St_Node        *fNode;        //Refs pointer to the node defined
 
 public:
        St_NodePosition(St_Node *node=0,Double_t x=0, Double_t y=0, Double_t z=0, TRotMatrix *matrix=0);
        St_NodePosition(St_Node *node,Double_t x, Double_t y, Double_t z, const Text_t *matrixname);
        St_NodePosition(const St_NodePosition&pos):fNode(pos.GetNode()),fX(pos.GetX()),fY(pos.GetY()),fZ(pos.GetZ()),fMatrix(pos.GetMatrix()){;}
        virtual ~St_NodePosition(){;}
        virtual void        Browse(TBrowser *b);
        virtual Int_t       DistancetoPrimitive(Int_t px, Int_t py);
        virtual void        Draw(Option_t *option=""); // *MENU*
        virtual void        ExecuteEvent(Int_t event, Int_t px, Int_t py);
        virtual TRotMatrix  *GetMatrix() const {return fMatrix;}
        virtual St_Node     *GetNode() const {return fNode;}
        virtual Text_t      *GetObjectInfo(Int_t px, Int_t py);
	const   Option_t    *GetOption() const { return GetNode()?GetNode()->GetOption():0;}
        virtual const Char_t *GetName() const { return GetNode() ? GetNode()->GetName():IsA()->GetName();}
	Int_t               GetVisibility() const {return GetNode()?GetNode()->GetVisibility():0;}
        virtual Double_t    GetX() const {return fX;}
        virtual Double_t    GetY() const {return fY;}
        virtual Double_t    GetZ() const {return fZ;}
        Bool_t              IsFolder() {return GetNode()?kTRUE:kFALSE;}
//        Bool_t              IsFolder() {return kFALSE;}
        virtual Bool_t      Is3D()  {return kTRUE;}
        virtual void        Local2Master(Double_t *local, Double_t *master);
        virtual void        Local2Master(Float_t *local, Float_t *master);
        virtual void        Paint(Option_t *option="");
        virtual void        UpdatePosition(Option_t *option="");
        virtual void        SetMatrix(TRotMatrix *matrix=0) {fMatrix = matrix;}
        virtual void        SetPosition( Double_t x=0, Double_t y=0, Double_t z=0) {fX=x; fY=y; fZ=z;}
//        virtual void        UpdateMatrix();
//        virtual void        UpdateTempMatrix(Double_t *dx1,Double_t *rmat1
//                             , Double_t x, Double_t y, Double_t z, Double_t *matrix
//                             , Double_t *dxnew, Double_t *rmatnew);
 
        ClassDef(St_NodePosition,1)  //Description of parameters to position a 3-D geometry object
};
 
#endif
