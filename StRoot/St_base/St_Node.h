//*-- Author :    Valery Fine(fine@bnl.gov)   25/12/98  
//Copyright (C) 1998. Brookhaven National Laboratory. Valery Fine (Faine). All right reserved.
 
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_Node                                                                //
//                                                                      //
// Description of parameters to position a 3-D geometry object          //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
 
#ifndef ROOT_St_Node
#define ROOT_St_Node

#include "St_ObjectSet.h" 

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
   TShape         *fShape;         //Pointer to shape definition
   TString         fOption;        //List of options if any
   Int_t           fVisibility;    //Visibility flag

   virtual void    Add(St_NodePosition *position);
   virtual void    Add(St_Node *node, St_NodePosition *nodePosition);
   void SetPositionsList(TList *list=0){AddObject((TObject *)list);}

 public:
        St_Node();
        St_Node(const Text_t *name, const Text_t *title, const Text_t *shapename, Option_t *option="");
        St_Node(const Text_t *name, const Text_t *title, TShape *shape, Option_t *option="");
        virtual ~St_Node();
        virtual void        Add(St_Node *node, Double_t x, Double_t y=0, Double_t z=0, TRotMatrix *matrix=0, Option_t *option="");
        virtual void        Add(St_Node *node, Double_t x, Double_t y, Double_t z,  const Text_t *matrixname, Option_t *option="");
        virtual void        DeletePosition(St_NodePosition *position);
        virtual Int_t       DistancetoPrimitive(Int_t px, Int_t py);
        virtual void        Draw(Option_t *option=""); // *MENU*
        virtual void        DrawOnly(Option_t *option="");
        virtual void        ExecuteEvent(Int_t event, Int_t px, Int_t py);
        virtual Text_t     *GetObjectInfo(Int_t px, Int_t py);
        const   Option_t   *GetOption() const { return fOption.Data();}
                TShape     *GetShape()  const {return fShape;}
        Int_t               GetVisibility() const {return fVisibility;}
        virtual TList      *GetListOfPositions() { return (TList *)(GetObject());}
        virtual void        ImportShapeAttributes();
        virtual Bool_t      Is3D()  {return kTRUE;}
        virtual TList      *Nodes() const { return GetList(); }
        virtual void        Paint(Option_t *option="");
        virtual void        RecursiveRemove(TObject *obj);
        virtual void        SetVisibility(Int_t vis=1); // *MENU*
        virtual void        Sizeof3D() const;
 
        ClassDef(St_Node,1)  //Description of parameters to position a 3-D geometry object
};
 
#endif
