//*CMZ :          13/08/98  18.27.27  by  Valery Fine(fine@bnl.gov)
//*-- Author :    Valery Fine(fine@mail.cern.ch)   13/08/98 

#ifndef ROOT_St_DataSet
#define ROOT_St_DataSet
 
 
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_DataSet                                                           //
//                                                                      //
// St_DataSet class is a base class to implement the directory-like     //
// data structures and maintain it via St_DataSetIter class iterator    //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
 
//*KEEP,TList.
#include <TList.h>
//*KEEP,TNamed.
#include <TNamed.h>
#include <TNode.h>
//*KEND.
 
class St_DataSetIter;
class TBrowser;

// The control codes to navigate the St_DataSet structure via St_DataSet::Pass method

typedef enum {
      kContinue,  // continue passing 
      kPrune,     // stop passing of the current branch but continue with the next one if any
      kStop,      // break passing
      kStruct,    // work with structural links only
      kAll,       // work with all links 
      kRefs,      // work with refs links only
      kMarked     // work with marked links only
     } EDataSetPass;

class St_DataSet : public TNamed
{
 friend class St_DataSetIter;
 friend class St_DataSetTree;
 protected: 
    TObject     *fMother; // pointer to mother of the directory
    TList       *fList;   // List of the the the objects included into this dataset
    virtual void SetParent(St_DataSet *parent=0);
    virtual void SetMother(TObject *mother) {fMother = mother;}
    St_DataSet(const Char_t *name,const Char_t *title) : TNamed(name,title){} // to support TDictionary
 
 public:
 
    St_DataSet(const Char_t *name="", St_DataSet *parent=0);
    St_DataSet(const St_DataSet &src,EDataSetPass iopt=kAll);
    St_DataSet(TNode &src); 
    virtual ~St_DataSet();
            void         Add(St_DataSet *dataset);
    virtual void         Browse(TBrowser *b);
    virtual TObject     *Clone();
    virtual St_DataSet  *Data() const { return HasData() ? (St_DataSet *)this : 0; }  // returns this pointer the derived classes if any
    virtual void         Delete(Option_t *opt="");   
            TObject     *GetMother() const { return fMother; }
    virtual St_DataSet  *GetParent() const { return (St_DataSet *)fMother;}
            TList       *GetList()   const { return fList; }
            TList       *GetListOfDataset() const {return GetList();}
            Int_t        GetListSize() const;
    virtual Long_t       HasData() const {return 0;}         // Check whether this dataset has extra "data-members"
//  virtual Bool_t       IsEmpty() const;
    virtual Bool_t       IsFolder() {return kTRUE;}
    virtual Bool_t       IsThisDir(const Char_t *dirname) const ;
    virtual void         ls(Option_t *option="");             // Option "*" means print all levels
    virtual void         ls(Int_t depth);                     // Print the "depth" levels of this datatset
    virtual void         Update();                            // Update dataset
    virtual void         Update(St_DataSet *set,UInt_t opt=0);// Update this dataset with the new one
           TString       Path() const;                        // return the "full" path of this dataset
    virtual EDataSetPass Pass(EDataSetPass ( *callback)(St_DataSet *),Int_t depth=0);
    virtual Int_t        Purge(Option_t *opt="");   
    virtual void         Remove(St_DataSet *set);
    virtual void         SetWrite();
    virtual void         Shunt(St_DataSet *dataset);
    ClassDef(St_DataSet,1)
};
 
inline Int_t     St_DataSet::GetListSize() const { return fList ? fList->GetSize():0; } 
#endif
