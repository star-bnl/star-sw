//*-- Author :    Valery Fine(fine@bnl.gov)   25/12/98 
// 
// 
#ifndef STAR_St_ObjectSet
#define STAR_St_ObjectSet

#include "St_DataSet.h"

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

class St_ObjectSet : public St_DataSet {
protected:
  TObject *m_Obj;  // "Embedded" TObject 
public:
  St_ObjectSet(const Char_t *name, TObject *obj=0);
  St_ObjectSet(St_ObjectSet &set,TObject *obj=0);
  St_ObjectSet(TObject *obj=0);
  virtual TObject *Clone();
  virtual ~St_ObjectSet();
  virtual TObject *GetObject();
  virtual void     AddObject(TObject *obj){ m_Obj = obj;}
  virtual Long_t   HasData() const {return m_Obj ? 1 : 0;} 
  ClassDef(St_ObjectSet,1)
};

inline TObject *St_ObjectSet::GetObject() { return m_Obj;}
#endif

