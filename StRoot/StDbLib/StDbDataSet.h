//*-- Author :    Valery Fine(fine@bnl.gov)   25/12/98 
// $Id: StDbDataSet.h,v 1.1 1999/08/17 18:51:32 porter Exp $
// $Log: StDbDataSet.h,v $
// Revision 1.1  1999/08/17 18:51:32  porter
// Add DataSet files which should eventually go into St_base area but as
// it is currently needed by codes other than StDbMaker
//
// Revision 1.7  1999/05/07 21:35:31  fine
// Fix some first implemenation bugs
//
// Revision 1.6  1999/05/07 17:53:18  fine
// owner bit has been introduced to deal with the embedded objects
//
#ifndef STAR_StDbDataSet
#define STAR_StDbDataSet

#include "St_DataSet.h"

class StDbTableI;

//////////////////////////////////////////////////////////////////////////////////////
//                                                                                  //
//  StDbDataSet  - is a container St_DataSet                                       //
//                  This means this object has an extra pointer to an embedded      //
//                  StDbTableI.                                                        //
//  Terminology:    This St_OvjectSet may be an OWNER of the embeded StDbTableI        //
//                  If the container is the owner it can delete the embeded object  //
//                  otherwsie it leaves that object "as is"                         //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

class StDbDataSet : public St_DataSet {
protected:
  enum EOwnerBits { kIsOwner         = BIT(23) };
  StDbTableI *fObj;//!                              // StDbTableI to be inserted
  virtual Bool_t DoOwner(Bool_t done=kTRUE);

public:
  StDbDataSet(const Char_t *name, StDbTableI *obj=0,Bool_t makeOwner=kTRUE);
  StDbDataSet(StDbTableI *obj=0,Bool_t makeOwner=kTRUE);
  virtual ~StDbDataSet();
  virtual void     Browse(TBrowser *b);
  virtual void     Delete(Option_t *opt="");
  virtual StDbTableI *GetDbTable() const {return fObj;};
  virtual void     SetObject(StDbTableI *obj) { SetObject(obj,kTRUE);}
  virtual StDbTableI *SetObject(StDbTableI *obj,Bool_t makeOwner);
  virtual StDbTableI *AddObject(StDbTableI *obj,Bool_t makeOwner=kTRUE);
  virtual StDbTableI* GetDbObject(){ return fObj; }; // no longer private
  virtual Long_t   HasData() const {return fObj ? 1 : 0;} 
  virtual Bool_t   IsOwner() const {return TestBit(kIsOwner);}

  ClassDef(StDbDataSet,1)
};

#endif

