//*-- Author :    Valery Fine(fine@bnl.gov)   25/12/98 
// 
// 
#include "St_ObjectSet.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_ObjectSet                                                         //
//                                                                      //
// St_ObjectSet class is a special kind of St_DataSet with one extra    //
// pointer to wrap any TObject onto St_DataSet object                   //
//                                                                      //
//  BE CAREFUL !!!                                                      //
//  One has to use it carefully no conrol over that extra object        //
//  is perfomred. This means: the onject m_Obj data-member points to can//
//  be destroyed with no this kbject notifying.                         //
//  There is no tool /protection to check whether m_Obj is till alive.  //
//  It is one's  code responsilitiy                                     //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(St_ObjectSet)
//_____________________________________________________________________________
St_ObjectSet::St_ObjectSet(const Char_t *name, TObject *obj):St_DataSet(name,obj?obj->GetTitle():"St_ObjectSet"),m_Obj(obj){;}
//_____________________________________________________________________________
St_ObjectSet::St_ObjectSet(TObject *obj) : St_DataSet(obj?obj->GetName():"unknown",obj?obj->GetTitle():"St_ObjectSet")
              ,m_Obj(obj){;}
//_____________________________________________________________________________
 St_ObjectSet::St_ObjectSet(St_ObjectSet &set,TObject *obj): St_DataSet(set),
  m_Obj(obj?obj:set.GetObject()){}
//_____________________________________________________________________________
St_ObjectSet::~St_ObjectSet(){;}
//______________________________________________________________________________
TObject *St_ObjectSet::Clone() {
   return new St_ObjectSet(*this,GetObject());
}

