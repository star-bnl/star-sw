//*-- Author :    Valery Fine(fine@bnl.gov)   25/12/98 
// $Id: St_VoidSet.h,v 1.2 1999/05/20 18:52:51 fine Exp $
// $Log: St_VoidSet.h,v $
// Revision 1.2  1999/05/20 18:52:51  fine
// GetObject method was removed to avoid aclash with St_DataSet
//
// Revision 1.1  1999/05/10 15:34:35  perev
// StBranch::Open & Close + new St_VoidSet class
//
// Revision 1.7  1999/05/07 21:35:31  fine
// Fix some first implemenation bugs
//
// Revision 1.6  1999/05/07 17:53:18  fine
// owner bit has been introduced to deal with the embedded objects
//
#ifndef STAR_St_VoidSet
#define STAR_St_VoidSet

#include "St_DataSet.h"

//////////////////////////////////////////////////////////////////////////////////////
//                                                                                  //
//  St_VoidSet  - is a container St_DataSet                                       //
//                  This means this object has an extra pointer to an embedded      //
//                  TObject.                                                        //
//  Terminology:    This St_OvjectSet may be an OWNER of the embeded TObject        //
//                  If the container is the owner it can delete the embeded object  //
//                  otherwsie it leaves that object "as is"                         //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

class St_VoidSet : public St_DataSet {
protected:
  void *fObj;                              // void Object to be inserted

public:
  St_VoidSet(const Char_t *name, void *obj=0);
  virtual ~St_VoidSet(){};
//  virtual void    *GetObject() const {return fObj;};
  virtual void    *SetObject(void *obj) { void *old =fObj;fObj=obj; return old;}
  ClassDef(St_VoidSet,1)
};

#endif

