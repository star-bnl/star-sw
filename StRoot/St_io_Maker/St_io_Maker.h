//*CMZ :          23/02/99  18.27.27  by  Valery Fine(fine@bnl.gov)
//*-- Author :    Valery Fine(fine@bnl.gov)   03/07/98
//
//  
//  
//
#ifndef STAR_St_io_Maker
#define STAR_St_io_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_io_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
 
class  TBranch;
class  TTree;

class St_io_Maker : public StMaker {
 private:
   Bool_t drawinit;
   Bool_t fSplit;    // flag of the "split" mode
// static Char_t  m_VersionCVS = "$Id: St_io_Maker.h,v 1.6 1999/02/25 17:14:14 fine Exp $";
// Int_t          m_mode;        // mode 1 = primaries;
   TObjArray     *m_ListOfBranches; //!

 
 public: 
            St_io_Maker(const char *name="output", const char *title="io_something",Bool_t split=kTRUE,TTree *tree=0);
   virtual  ~St_io_Maker();
   virtual TObjArray *GetListOfBranches(){return m_ListOfBranches;}
   virtual void       Add(const Char_t *dataName,const Char_t *fileName=0);
   virtual void       Add(TString &dataName,const Char_t *fileName=0);
   virtual void       Add(TBranch *branch,const Char_t *dataName=0,const Char_t *fileName=0);
   virtual void       Clear(Option_t *option="");
   virtual TObjArray *CreateBranchList() {if (!m_ListOfBranches) m_ListOfBranches = new TObjArray; return m_ListOfBranches;}
   virtual St_DataSet *DataSet(const Char_t *set);
   virtual Int_t      Finish();
   virtual Int_t      GetEvent(Int_t nevent=0);
   virtual Int_t      Init();
   virtual Int_t      Make();
   virtual TTree     *MakeTree(const char* name, const char*title);
           Int_t      NextEventPut();
           Int_t      NextEventGet(Int_t nevent);
   virtual void       PrintInfo();
   virtual Int_t      SetActive();
// virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
   ClassDef(St_io_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
