//*-- Author :    Valery Fine(fine@bnl.gov)   25/12/98 
// $Id: St_ObjectSet.cxx,v 1.7 1999/05/07 21:35:32 fine Exp $
// $Log: St_ObjectSet.cxx,v $
// Revision 1.7  1999/05/07 21:35:32  fine
// Fix some first implemenation bugs
//
// Revision 1.6  1999/05/07 17:53:18  fine
// owner bit has been introduced to deal with the embedded objects
//
#include "St_ObjectSet.h"
#include "TBrowser.h"

ClassImp(St_ObjectSet)
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

//_____________________________________________________________________________
St_ObjectSet::St_ObjectSet(const Char_t *name, TObject *obj, Bool_t makeOwner):St_DataSet(name)
{ 
  SetTitle("St_ObjectSet");
  SetObject(obj,makeOwner);
}
//_____________________________________________________________________________
St_ObjectSet::St_ObjectSet(TObject *obj,Bool_t makeOwner) : St_DataSet("unknown","St_ObjectSet")
{
  SetObject(obj,makeOwner);
}

//_____________________________________________________________________________
St_ObjectSet::~St_ObjectSet()
{
// Attn.: virtual  St_Object::Delete will be called via virtual dtor of St_DataSet
}
//_____________________________________________________________________________
void St_ObjectSet::Delete(Option_t *opt)
{
   if (fObj && IsOwner()) delete fObj;
   fObj = 0;  
   St_DataSet::Delete();
}

//______________________________________________________________________________
void St_ObjectSet::Browse(TBrowser *b)
{
  // Browse this dataset (called by TBrowser).
   if (b && fObj) b->Add(fObj);
  St_DataSet::Browse(b);
}

//______________________________________________________________________________
TObject *St_ObjectSet::SetObject(TObject *obj,Bool_t makeOwner)
{ 
  //
  // - Replace the embedded object with a new supplied 
  // - Destroy the preivous embedded object if this is its owner
  // - Return the previous embedded object if any
  //
   TObject *oldObject = fObj;
   if (IsOwner()) { delete oldObject; oldObject = 0;} // the object has been killed
   fObj = obj;  
   DoOwner(makeOwner);
   return oldObject;
}
//______________________________________________________________________________
TObject *St_ObjectSet::AddObject(TObject *obj,Bool_t makeOwner)
{ 
  // Aliase for SetObject method
 return SetObject(obj,makeOwner);
}
//______________________________________________________________________________
Bool_t St_ObjectSet::DoOwner(Bool_t done)
{ 
 // Set / Reset the ownerships and returns the previous
 // status of the ownerships.

  Bool_t own = IsOwner();
  if (own != done) {
    if (done) SetBit(kIsOwner); 
    else ResetBit(kIsOwner);
  }
  return own;
}
