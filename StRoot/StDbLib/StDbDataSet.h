//*-- Author :    Valery Fine(fine@bnl.gov)   25/12/98 
// $Id: StDbDataSet.h,v 1.2 2000/01/10 20:37:53 porter Exp $
// $Log: StDbDataSet.h,v $
// Revision 1.2  2000/01/10 20:37:53  porter
// expanded functionality based on planned additions or feedback from Online work.
// update includes:
// 	1. basis for real transaction model with roll-back
// 	2. limited SQL access via the manager for run-log & tagDb
// 	3. balance obtained between enumerated & string access to databases
// 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
// 	5. restructured Node model for better XML support
//
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

class StDbTable;

//////////////////////////////////////////////////////////////////////////////////////
//                                                                                  //
//  StDbDataSet  - is a container St_DataSet                                       //
//                  This means this object has an extra pointer to an embedded      //
//                  StDbTable.                                                        //
//  Terminology:    This St_OvjectSet may be an OWNER of the embeded StDbTable        //
//                  If the container is the owner it can delete the embeded object  //
//                  otherwsie it leaves that object "as is"                         //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

class StDbDataSet : public St_DataSet {
protected:
  enum EOwnerBits { kIsOwner         = BIT(23) };
  StDbTable *fObj;//!                              // StDbTable to be inserted
  virtual Bool_t DoOwner(Bool_t done=kTRUE);

public:
  StDbDataSet(const Char_t *name, StDbTable *obj=0,Bool_t makeOwner=kTRUE);
  StDbDataSet(StDbTable *obj=0,Bool_t makeOwner=kTRUE);
  virtual ~StDbDataSet();
  virtual void     Browse(TBrowser *b);
  virtual void     Delete(Option_t *opt="");
  virtual StDbTable *GetDbTable() const {return fObj;};
  virtual void     SetObject(StDbTable *obj) { SetObject(obj,kTRUE);}
  virtual StDbTable *SetObject(StDbTable *obj,Bool_t makeOwner);
  virtual StDbTable *AddObject(StDbTable *obj,Bool_t makeOwner=kTRUE);
  virtual StDbTable* GetDbObject(){ return fObj; }; // no longer private
  virtual Long_t   HasData() const {return fObj ? 1 : 0;} 
  virtual Bool_t   IsOwner() const {return TestBit(kIsOwner);}

  ClassDef(StDbDataSet,1)
};

#endif


