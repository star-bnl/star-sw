/*!
 * \class  StTagFilterMaker
 * \brief  Skip events using criteria in .pretags.root
 * \author G. Van Buren
 * \date   Apr 2015
 *
 * Base class to skip production of events using
 * criteria stored in a .pretags.root Tag TTree
 * 
 * Derived classes should implement SkipEvent()
 * to determine whether events should be skipped or not.
 * 
 * SetVarList() can be used to provide a colon-separated
 * list of variables (formulas) which will automatically
 * be evaluated for use in SkipEvent().
 * Individual variable values will be available as GetVal(0),GetVal(1) ...
 * Individual variable names  will be available as GetVar(0),GetVar(1) ...
 *
 * Otherwise, derived classes can access the Tag TTree
 * as they wish, without needing selection criteria, as
 * a TEventList is applied to select the run and event.
 *
 */


#ifndef StTagFilterMaker_hh     
#define StTagFilterMaker_hh

#include "StMaker.h"
#include "TString.h"

class TFile;
class TTree;
class TEntryList;

class StTagFilterMaker : public StMaker {
 public:
  
  StTagFilterMaker(const Char_t *name="TagFilter");
  ~StTagFilterMaker();
  
  virtual Int_t  Init();
  virtual Int_t  InitRun(const int runnum);
  virtual bool SkipEvent()=0;
  virtual Int_t  Make();
  virtual void  Clear(const Option_t* = "");
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StTagFilterMaker.h,v 1.2 2015/05/05 20:23:42 genevb Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
  
  void SetTagFile(const Char_t *file) { mTagFile = file; }
  void SetVarList(const Char_t *varList) { mVarList = varList; } // colon-separated variables (formulas)
  const char* GetTagFile() { return mTagFile.Data(); }
  const char* GetVarList() { return mVarList.Data(); }
  void EvalVarList(const Char_t *varList=0);
  Double_t GetVal(int i, int idx=0);
  const char* GetVar(int i);
  int GetSkippedEventCount() { return mSkippedEventCounter; }

 protected:


  TString mTagFile; ///< Tags input file name, ending in .pretags.root by default
  TString mVarList; ///< List of colon-separated variables of interest
  int mSkippedEventCounter; ///< Number of events skipped
  TFile *mFile; ///< Pointer to the .pretags.root input file
  TTree *mTree; ///< Pointer to the Tags TTree
  TEntryList* mEntryList; ///< Selection of run and event

  ClassDef(StTagFilterMaker,0)    
};
#endif

/* -------------------------------------------------------------------------
 * $Id: StTagFilterMaker.h,v 1.2 2015/05/05 20:23:42 genevb Exp $
 * $Log: StTagFilterMaker.h,v $
 * Revision 1.2  2015/05/05 20:23:42  genevb
 * pre.tags.root => pretags.root
 *
 * Revision 1.1  2015/05/01 21:25:50  jeromel
 * First version of the DataFiler + one imp: MTD. Code from GVB reviewed & closed (VP+JL))
 *
 *
 * -------------------------------------------------------------------------
 */

