// $Id: StStrangeControllerBase.h,v 2.0 2000/06/05 05:19:41 genevb Exp $
// $Log: StStrangeControllerBase.h,v $
// Revision 2.0  2000/06/05 05:19:41  genevb
// New version of Strangeness micro DST package
//
//
#ifndef STAR_StStrangeControllerBase
#define STAR_StStrangeControllerBase
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StStrangeControllerBase strangeness micro DST controller base class  //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "TNamed.h"
#include "TClonesArray.h"
#include "StMcContainers.hh"

class TTree;
class TArrayI;
class StEvent;
class StMcVertex;
class StAssociationMaker;
class StStrangeMuDst;
class StStrangeAssoc;
class StStrangeMuDstMaker;

class StStrangeControllerBase : public TNamed {
 public: 
  StStrangeControllerBase(const char* name);
  virtual ~StStrangeControllerBase();

  TClonesArray* GetDataArray();
  TClonesArray* GetMcArray();   
  TClonesArray* GetAssocArray();   

  Int_t GetN();
  Int_t GetNMc();
  Int_t GetNAssoc();

  StStrangeMuDst* Get(Int_t i=0);
  StStrangeMuDst* GetMc(Int_t i = 0);
  StStrangeAssoc* GetAssoc(Int_t i = 0);

  virtual void  Clear();
  virtual void  Finish();
  
  // Functions for sub-dsts:
  virtual void Select(Int_t i=-1);     // use i<0 to specify whole event
  
  virtual void InitReadDst();
  virtual void InitCreateDst(const char* file);
  virtual void InitCreateSubDst();
  virtual Int_t MakeReadDst() = 0;
  virtual Int_t MakeCreateDst(StEvent& event) = 0;
  virtual Int_t MakeCreateMcDst(StMcVertex* mcVert) = 0;
  virtual Int_t MakeCreateSubDst() = 0;

  virtual void PrintNumMc();
  static StStrangeMuDstMaker* currentMaker;
    
 protected:
  StStrangeControllerBase* GetDstController();
  void PrintNumCand(const char* text, Int_t num);

  Bool_t doMc;

  TClonesArray* dataArray;          //!
  TClonesArray* mcArray;            //!
  TClonesArray* assocArray;         //!
  TClonesArray* tempArray;          //!

  StStrangeMuDstMaker* masterMaker; //!
  StStrangeMuDstMaker* dstMaker;    //!
  StAssociationMaker *assocMaker;   //!

  TTree* tree;                      //!
  
  // Array of muDst indices to copy
  TArrayI* selections;              //!

  // Totals for entire set
  Int_t nEntries;

  // Totals for events
  Int_t entries;
  Int_t mcEntries;
  Int_t assocEntries;
  
  Int_t increment;
  Int_t max;
  
 private:
  ClassDef(StStrangeControllerBase,1)
};

inline TClonesArray* StStrangeControllerBase::GetDataArray()
            { return dataArray; }
inline TClonesArray* StStrangeControllerBase::GetMcArray()
            { return mcArray; }
inline TClonesArray* StStrangeControllerBase::GetAssocArray()
            { return assocArray; }
	    
inline Int_t StStrangeControllerBase::GetN()
            { return (dataArray ? dataArray->GetEntriesFast() : 0); }
inline Int_t StStrangeControllerBase::GetNMc()
            { return (mcArray ? mcArray->GetEntriesFast() : 0); }
inline Int_t StStrangeControllerBase::GetNAssoc()
            { return (assocArray ? assocArray->GetEntriesFast() : 0); }

inline StStrangeMuDst* StStrangeControllerBase::Get(Int_t i)
            { return (dataArray ? (StStrangeMuDst*) (*dataArray)[i] : 0); }
inline StStrangeMuDst* StStrangeControllerBase::GetMc(Int_t i)
            { return (mcArray ? (StStrangeMuDst*) (*mcArray)[i] : 0); }
inline StStrangeAssoc* StStrangeControllerBase::GetAssoc(Int_t i)
            { return (assocArray ? (StStrangeAssoc*) (*assocArray)[i] : 0); }

#endif
