#ifndef STAR_St_DataSetTree
#define STAR_St_DataSetTree

#include "TObject.h"

class TTree;

class TFile;
class St_DataSet;
class St_DataSetTree : public TObject {

  private:
    TTree      *fTree;
    St_DataSet **fSet;
    TFile      *fFile;

  public:
    St_DataSetTree( const char* filename="", St_DataSet *dataset=0);
    virtual  ~St_DataSetTree();
    virtual TTree *Tree(){ return fTree; }
    virtual void Close();
    void MakeTree(St_DataSet *dataset);
    void FillTree(St_DataSet *dataset);
    ClassDef(St_DataSetTree,0)
};

#endif

