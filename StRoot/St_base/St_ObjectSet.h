//*-- Author :    Valery Fine(fine@bnl.gov)   25/12/98 
// $Id: St_ObjectSet.h,v 1.6 1999/05/07 17:53:18 fine Exp $
// $Log: St_ObjectSet.h,v $
// Revision 1.6  1999/05/07 17:53:18  fine
// owner bit has been introduced to deal with the embedded objects
//
#ifndef STAR_St_ObjectSet
#define STAR_St_ObjectSet

#include "St_DataSet.h"

//////////////////////////////////////////////////////////////////////////////////////
//                                                                                  //
//  St_ObjectSet  - is a container St_DataSet                                       //
//                  This means this object has an extra pointer to an embedded      //
//                  TObject.                                                        //
//  Terminology:    This St_OvjectSet may be an OWNER of the embeded TObject        //
//                  If the container is the owner it can delete the embeded object  //
//                  otherwsie it leaves that object "as is"                         //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

class St_ObjectSet : public St_DataSet {
protected:
  enum EOwnerBits { kIsOwner         = BIT(23) };
  TObject *fObj;                              // TObject to be inserted
  virtual Bool_t DoOwner(Bool_t done=kTRUE);

public:
  St_ObjectSet(const Char_t *name, TObject *obj=0,Bool_t makeOwner=kTRUE);
  St_ObjectSet(TObject *obj=0,Bool_t makeOwner=kTRUE);
  virtual ~St_ObjectSet();
  virtual void     Browse(TBrowser *b);
  virtual void     Delete(Option_t *opt="");
  virtual TObject *GetObject() const {return fObj;};
  virtual TObject *SetObject(TObject *obj,Bool_t makeOwner=kTRUE);
  virtual TObject *AddObject(TObject *obj,Bool_t makeOwner=kTRUE);
  virtual Long_t   HasData() const {return fObj ? 1 : 0;} 
  virtual Bool_t   IsOwner() const {return TestBit(kIsOwner);}

  ClassDef(St_ObjectSet,1)
};

#endif

