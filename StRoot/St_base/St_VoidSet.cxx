//*-- Author :    Valery Fine(fine@bnl.gov)   25/12/98 
// $Id: St_VoidSet.cxx,v 1.1 1999/05/10 15:34:35 perev Exp $
// $Log: St_VoidSet.cxx,v $
// Revision 1.1  1999/05/10 15:34:35  perev
// StBranch::Open & Close + new St_VoidSet class
//
// Revision 1.7  1999/05/07 21:35:32  fine
// Fix some first implemenation bugs
//
// Revision 1.6  1999/05/07 17:53:18  fine
// owner bit has been introduced to deal with the embedded objects
//
#include "St_VoidSet.h"

ClassImp(St_VoidSet)
//////////////////////////////////////////////////////////////////////////////////////
//                                                                                  //
//  St_VoidSet  - is a container St_DataSet                                       //
//                  This means this object has an extra pointer to an embedded      //
//                  void Object.                                                        //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

//_____________________________________________________________________________
St_VoidSet::St_VoidSet(const Char_t *name, void *obj):St_DataSet(name)
{ 
  SetTitle("St_VoidSet");
  SetObject(obj);
}

