// $Id: StStrangeControllerBase.h,v 3.6 2003/05/30 21:20:19 genevb Exp $
// $Log: StStrangeControllerBase.h,v $
// Revision 3.6  2003/05/30 21:20:19  genevb
// doxygen savvy, encoding of FTPC mults, change virtual funcs
//
// Revision 3.5  2001/09/14 21:39:02  genevb
// Adjustments to not depend on order in which maker Clear() is called
//
// Revision 3.4  2001/08/23 13:20:53  genevb
// Many bug workarounds...
//
// Revision 3.2  2000/12/18 21:35:18  genevb
// Introduced variable buffer-sizing
//
// Revision 3.1  2000/07/17 20:28:40  genevb
// File size limitation workaround, some under the hood improvements
//
// Revision 3.0  2000/07/14 12:56:48  genevb
// Revision 3 has event multiplicities and dedx information for vertex tracks
//
// Revision 2.1  2000/06/09 22:17:10  genevb
// Allow MC data to be copied between DSTs, other small improvements
//
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

class TTree;
class TArrayI;
class TArrayS;
class StEvent;
class StMcVertex;
class StAssociationMaker;
class StStrangeMuDst;
class StStrangeAssoc;
class StStrangeMuDstMaker;
class TBranch;
class TClass;

enum StrBranchType {dataBranch=0, mcBranch=1, assocBranch};

class StStrangeControllerBase : public TNamed {
 public: 
  StStrangeControllerBase(Int_t type);
  virtual ~StStrangeControllerBase();
  static StStrangeControllerBase* Instantiate(Int_t type);

  TClonesArray* GetArray(Int_t branchType);
  TClonesArray* GetDataArray();
  TClonesArray* GetMcArray();   
  TClonesArray* GetAssocArray();   

  Int_t GetN(Int_t branchType);
  Int_t GetN();
  Int_t GetNMc();
  Int_t GetNAssoc();

  StStrangeMuDst* Get(Int_t i, Int_t branchType);
  StStrangeMuDst* Get(Int_t i=0);
  StStrangeMuDst* GetMc(Int_t i = 0);
  StStrangeAssoc* GetAssoc(Int_t i = 0);

  virtual void  Clear(Option_t* opt=0);
  virtual void  Finish();
  
  // Functions for sub-dsts:
  virtual void Select(Int_t i=-1);     // use i<0 to specify whole event
  virtual void Unselect(Int_t i=-1);   // use i<0 to specify whole event
  
  virtual void InitReadDst();
  virtual void InitCreateDst();
  virtual void InitCreateSubDst();
  virtual Int_t MakeReadDst() = 0;
  virtual Int_t MakeCreateDst(StEvent& event) = 0;
  virtual Int_t MakeCreateMcDst(StMcVertex* mcVert) = 0;
  virtual Int_t MakeCreateSubDst();

  TClass* GetDataClass() const;
  TClass* GetMcClass() const;
  TClass* GetAssocClass() const;

  void PrintNumMc();
  const char* GetMcName() const;
  const char* GetAssocName() const;
  void SetBufferSize(Int_t b);
  static StStrangeMuDstMaker* currentMaker;
    
 protected:
  StStrangeControllerBase* GetDstController();
  void PrintNumCand(const char* text, Int_t num);
  TBranch* AssignBranch(const char* name, TClonesArray** address);

  Bool_t doMc;

  TClonesArray* dataArray;          //!
  TClonesArray* mcArray;            //!
  TClonesArray* assocArray;         //!
  TClonesArray* tempArray;          //!

  StStrangeMuDstMaker* masterMaker; //!
  StStrangeMuDstMaker* dstMaker;    //!
  StAssociationMaker* assocMaker;   //!

  TTree* tree;                      //!
  char* file;                       //!
  Int_t dstType;                    //!
  TClass* dataClass;                //!
  TClass* mcClass;                  //!
  TClass* assocClass;               //!

  // Array of muDst indices to copy
  TArrayI* selections;              //!
  TArrayS* keepers;                 //!

  // Totals for entire set
  Int_t nEntries;

  // Totals for events
  Int_t entries;
  Int_t mcEntries;
  Int_t assocEntries;
  
  Int_t increment;
  Int_t max;
  Int_t bsize;
  TString mcName;
  TString assocName;
  
 private:
  ClassDef(StStrangeControllerBase,3)
};

inline TClonesArray* StStrangeControllerBase::GetDataArray()
            { return dataArray; }
inline TClonesArray* StStrangeControllerBase::GetMcArray()
            { return mcArray; }
inline TClonesArray* StStrangeControllerBase::GetAssocArray()
            { return assocArray; }
	    
inline Int_t StStrangeControllerBase::GetN(Int_t branchType)
            { TClonesArray* array = GetArray(branchType);
              return (array ? array->GetEntriesFast() : 0); }
inline Int_t StStrangeControllerBase::GetN()
            { return (dataArray ? dataArray->GetEntriesFast() : 0); }
inline Int_t StStrangeControllerBase::GetNMc()
            { return (mcArray ? mcArray->GetEntriesFast() : 0); }
inline Int_t StStrangeControllerBase::GetNAssoc()
            { return (assocArray ? assocArray->GetEntriesFast() : 0); }

inline StStrangeMuDst* StStrangeControllerBase::Get(Int_t i, Int_t branchType)
            { TClonesArray* array = GetArray(branchType);
              return (array ? (StStrangeMuDst*) (*array)[i] : 0); }
inline StStrangeMuDst* StStrangeControllerBase::Get(Int_t i)
            { return (dataArray ? (StStrangeMuDst*) (*dataArray)[i] : 0); }
inline StStrangeMuDst* StStrangeControllerBase::GetMc(Int_t i)
            { return (mcArray ? (StStrangeMuDst*) (*mcArray)[i] : 0); }
inline StStrangeAssoc* StStrangeControllerBase::GetAssoc(Int_t i)
            { return (assocArray ? (StStrangeAssoc*) (*assocArray)[i] : 0); }

inline TClass* StStrangeControllerBase::GetDataClass() const
            { return dataClass; }
inline TClass* StStrangeControllerBase::GetMcClass() const
            { return mcClass; }
inline TClass* StStrangeControllerBase::GetAssocClass() const
            { return assocClass; }
	    
inline const char* StStrangeControllerBase::GetMcName() const
            { return mcName.Data(); }
inline const char* StStrangeControllerBase::GetAssocName() const
            { return assocName.Data(); }

inline void StStrangeControllerBase::SetBufferSize(Int_t b) { bsize = b; }
#endif
