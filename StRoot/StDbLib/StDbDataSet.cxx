//*-- Author :    Valery Fine(fine@bnl.gov)   25/12/98 
// $Id: StDbDataSet.cxx,v 1.3 1999/12/03 22:24:01 porter Exp $
// $Log: StDbDataSet.cxx,v $
// Revision 1.3  1999/12/03 22:24:01  porter
// expanded functionality used by online, fixed bug in
// mysqlAccessor::getElementID(char*), & update StDbDataSet to
// conform to changes in Xml reader & writer
//
// Revision 1.2  1999/08/23 16:10:13  porter
// modified XmlReader & XmlWriter to be in sinc with Xml format used by current
// perl scripts.  Also added removeTable to StDbConfigNode so that others can
// become owner of a table
//
// Revision 1.1  1999/08/17 18:51:31  porter
// Add DataSet files which should eventually go into St_base area but as
// it is currently needed by codes other than StDbMaker
//
// Revision 1.7  1999/05/07 21:35:32  fine
// Fix some first implemenation bugs
//
// Revision 1.6  1999/05/07 17:53:18  fine
// owner bit has been introduced to deal with the embedded objects
//

#include <iostream.h>
#include "StDbDataSet.h"
#include "TBrowser.h"
#include "StDbTableI.h"
#include "StDbXmlWriter.h"

ClassImp(StDbDataSet)
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

//_____________________________________________________________________________
StDbDataSet::StDbDataSet(const Char_t *name, StDbTableI *obj, Bool_t makeOwner):St_DataSet(name)
{ 
  SetTitle("StDbDataSet");
  SetObject(obj,makeOwner);
}
//_____________________________________________________________________________
StDbDataSet::StDbDataSet(StDbTableI *obj,Bool_t makeOwner) : St_DataSet("unknown","StDbDataSet")
{
  SetObject(obj,makeOwner);
}

//_____________________________________________________________________________
StDbDataSet::~StDbDataSet()
{
// Attn.: virtual  St_Object::Delete will be called via virtual dtor of St_DataSet
}
//_____________________________________________________________________________
void StDbDataSet::Delete(Option_t *opt)
{
   if (fObj && IsOwner()) delete fObj;
   fObj = 0;  
   St_DataSet::Delete();
}

//______________________________________________________________________________
void StDbDataSet::Browse(TBrowser *b)
{
  // Browse this dataset (called by TBrowser).
  //   if (b && fObj) b->Add(fObj);
  St_DataSet::Browse(b);
  //  cout << "Test This Browser " << endl;

  if(fObj){

    StDbXmlWriter* acc = new StDbXmlWriter(cout);
    char* name;
    //acc->streamHeader("dummy"); // this is for DataBase Name
    name = fObj->getTableName();
    acc->streamTableName(name); delete [] name;
    acc->streamAccessor();
    fObj->StreamAccessor((typeAcceptor*)acc,false);
    acc->endAccessor();
    fObj->dbStreamer((typeAcceptor*)acc,false);
    acc->streamEndTableName();
    //acc->streamTail();  // end DataBase
    delete acc;
  }

}

//______________________________________________________________________________
StDbTableI *StDbDataSet::SetObject(StDbTableI *obj,Bool_t makeOwner)
{ 
  //
  // - Replace the embedded object with a new supplied 
  // - Destroy the preivous embedded object if this is its owner
  // - Return the previous embedded object if any
  //
   StDbTableI *oldObject = fObj;
   if (IsOwner()) { delete oldObject; oldObject = 0;} // the object has been killed
   fObj = obj;  
   DoOwner(makeOwner);
   return oldObject;
}
//______________________________________________________________________________
StDbTableI *StDbDataSet::AddObject(StDbTableI *obj,Bool_t makeOwner)
{ 
  // Aliase for SetObject method
 return SetObject(obj,makeOwner);
}
//______________________________________________________________________________
Bool_t StDbDataSet::DoOwner(Bool_t done)
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
