//*-- Author :    Valery Fine   27/04/98
 
#ifndef STAF_St_DataSet
#define STAF_St_DataSet 
 
 
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_DataSet                                                           //
//                                                                      //
// Staf Data Set class to implement the STAF Event data strucutre       //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
 
#include "St_Table.h"
#include "TList.h"

class St_DataSetIter;
class TBrowser;

class St_DataSet : public TNamed
{
 friend class St_DataSetIter;
 friend class St_TableIter;
 private:
    St_Table    *s_StafTable;       // The pointer to the STAF STAR tables for this data set
    TList       *s_ListOfDataSet;   // List of the STAF DataSet objects
 protected:
   virtual void SetParent(St_DataSet *parent){if (s_ListOfDataSet) /* s_ListOfDataSet->SetParent(parent)*/;}
 public:
    St_DataSet(const Char_t *name="", St_DataSet *parent=0);
    virtual ~St_DataSet();
            void        Add(St_DataSet *dataset);
    virtual void        Add(St_Table *table){s_StafTable = table;}
    virtual void        Browse(TBrowser *b);
    virtual St_DataSet *GetParent(){ return s_ListOfDataSet ? (St_DataSet *)(s_ListOfDataSet->GetParent()):0;}
            St_Table   *GetStafTable(){ return GetTableObj(); }
            St_Table   *GetTableObj(){ return s_StafTable; }
            TList      *GetListOfDataset(){ return s_ListOfDataSet; }
            Int_t       GetListSize(){ return s_ListOfDataSet ? s_ListOfDataSet->GetSize():0; }
    virtual void        Remove(St_DataSet *set);
    virtual St_DataSet *InsertTable(St_Table *table);
    virtual Bool_t      IsFolder(){ return (!s_StafTable && s_ListOfDataSet->Last());}
    virtual Bool_t      IsThisDir(const Char_t *dirname);
    virtual void        ls(Option_t *option="");  // Option "*" means print all levels
    virtual void        ls(Int_t deep);           // Print the "deep" levels of this datatset
            ClassDef(St_DataSet,1) 
};

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_DataSetIter                                                       //
//                                                                      //
// Iterator of STAF DataSet lists.                                      //
//                                                                      //
// Provides "standard" features of the TIter class for St_DataSet object//
//                             and                                      //
// allows navigating St_DataSet structure using the custom "directory"  //
//    notation (see *St_DataSet::Next(const Char *path) method)         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

class St_DataSetIter : public TObject{
private:
   TIter *s_Next;
   St_DataSet *s_RootDataSet;     // Pointer to the ROOT DataSet
   St_DataSet *s_WorkingDataSet;  // Pointer to the working DataSet
   
public:
  St_DataSetIter(St_DataSet *l=0, Bool_t dir = kIterForward);
  virtual         ~St_DataSetIter() {if (s_Next) delete s_Next; s_Next = 0;}
  virtual St_DataSet      *AddTable(St_Table *table){ return AddTable(table,(St_DataSet *)0);}
  virtual St_DataSet      *AddTable(St_Table *table, St_DataSet *dataset);
  virtual St_DataSet      *AddTable(St_Table *table, const Char_t *path);
  virtual St_DataSet      *Cd(Char_t *dirname);
  virtual St_DataSet      *operator()() { return  (St_DataSet *)(s_Next?s_Next->Next():0);}
  virtual St_DataSet      *operator()(const Char_t *path) { return Next(path); }
  virtual void            *GetTable(const Char_t *path);
  virtual St_Table        *GetTableObj(const Char_t *path);
  virtual St_DataSet      *Dir(Char_t *dirname);
  virtual St_DataSet      *Mkdir(Char_t *dirname);
  virtual St_DataSet      *Md(Char_t *dirname){return Mkdir(dirname);}
  virtual St_DataSet      *Pwd(){return s_WorkingDataSet;}
  virtual Int_t            Rmdir(St_DataSet *dataset,Option_t *option="");
  virtual Int_t            Rmdir(Char_t *dirname,Option_t *option=""){return Rmdir(Next(dirname),option);}
  virtual Int_t            Rd(Char_t *dirname,Option_t *option=""){return Rmdir(Next(dirname),option);}
  virtual St_DataSet      *Next(){ return (St_DataSet *) (s_Next ? s_Next->Next():0);}
  virtual St_DataSet      *Next(const Char_t *path, St_DataSet *rootset=0,Bool_t mkdir=kFALSE);
  const Option_t *GetOption() const { return s_Next?s_Next->GetOption():0; }
  virtual void            Reset(St_DataSet *l=0);
  ClassDef(St_DataSetIter,0)
};

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_TableIter                                                         //
//                                                                      //
// Iterator of STAF table lists.                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

class St_TableIter : public TObject {
private:
   TIter s_Next;               // TIter object to interate TList of St_DataSet
public:
  St_TableIter(const St_DataSet *l, Bool_t dir = kIterForward)
                 : s_Next(l?l->s_ListOfDataSet:0,dir){}
  virtual         ~St_TableIter() {}
  St_Table        *operator()() { return Next(); }
  St_Table        *Next() { return ((St_DataSet *)s_Next())->GetTableObj(); }
   const Option_t *GetOption() const { return s_Next.GetOption(); }
   void            Reset() {s_Next;}
  ClassDef(St_TableIter,0)

};

#endif
