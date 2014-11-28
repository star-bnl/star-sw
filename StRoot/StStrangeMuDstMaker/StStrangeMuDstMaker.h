/*!
  \class StStrangeMuDstMaker
  
  StStrangeMuDstMaker strangeness micro DST maker
  \sa http://www.star.bnl.gov/STAR/comp/pkg/dev/StRoot/StStrangeMuDstMaker/doc/

*/

/*! \file StStrangeMuDstMaker.h */

#ifndef STAR_StStrangeMuDstMaker
#define STAR_StStrangeMuDstMaker
#include "StMaker.h"
#include "TClonesArray.h"
#include "StStrangeControllerBase.h"

class TFile;
class TTree;
class TChain;
class StStrangeMuDst;
class StStrangeEvMuDst;
class StV0I;
class StV0MuDst;
class StV0Mc;
class StXiI;
class StXiMuDst;
class StXiMc;
class StKinkI;
class StKinkMuDst;
class StKinkMc;
class StStrangeAssoc;
class StFile;
class StStrangeCuts;

/// DST types
enum StrDstType  {
  /// Event
  evT = 0,
  /// V0
  v0T,
  /// Xi
  xiT,
  /// Kink
  kinkT,
  strDstT};

/// DST type names
static const char* strTypeNames[strDstT] = {"Ev","V0","Xi","Kink"};
/// I/O modes
enum StrangeEnum {StrangeNoKeep, StrangeNoFile, StrangeWrite, StrangeRead};

class StStrangeMuDstMaker : public StMaker {
 public: 
  StStrangeMuDstMaker(const char *name="strangeMuDst");
  virtual ~StStrangeMuDstMaker();
  void DoT0JitterAbort(Bool_t doIt=kTRUE);
  /// turn off filling of TTree for this event, regardless
  void AbortEvent() { abortEvent = kTRUE; }

  /// @name Files for multiplicities/centralities
  //@{
  void SetCorrectionFile(char*);
  void SetFractionFile(char*);
  //@}

  /// @name I/O modes
  //@{
  void SetRead (const char* eFile=0, const char* treeName=0);
  void SetRead (StFile* eFiles, const char* treeName=0);
  void SetWrite(const char* eFile);
  void SetWrite(){SetWrite(0);}
  void SetNoKeep();
  char* GetFile();
  //@}

  /// @name Branch modes
  //@{
  void DoV0(Bool_t doIt=kTRUE);
  void DoXi(Bool_t doIt=kTRUE);
  void DoKink(Bool_t doIt=kTRUE);
  void DoMc(Bool_t doIt=kTRUE);
  void Do(Int_t dstType, Bool_t doIt=kTRUE);
  void Do(const char* name, Bool_t doIt=kTRUE);
  Bool_t GetDoMc();
  //@}

  /// @name Data array size accessor functions
  //@{
  Int_t GetNV0I(Bool_t MC=kFALSE)   { return (MC ? GetNV0Mc()   : GetNV0()); }
  Int_t GetNXiI(Bool_t MC=kFALSE)   { return (MC ? GetNXiMc()   : GetNXi()); }
  Int_t GetNKinkI(Bool_t MC=kFALSE) { return (MC ? GetNKinkMc() : GetNKink()); }

  Int_t GetNV0()        { return v0->GetN(); }
  Int_t GetNV0Mc()      { return v0->GetNMc(); }
  Int_t GetNV0Assoc()   { return v0->GetNAssoc(); }
  Int_t GetNXi()        { return xi->GetN(); }
  Int_t GetNXiMc()      { return xi->GetNMc(); }
  Int_t GetNXiAssoc()   { return xi->GetNAssoc(); }
  Int_t GetNKink()      { return kink->GetN(); }
  Int_t GetNKinkMc()    { return kink->GetNMc(); }
  Int_t GetNKinkAssoc() { return kink->GetNAssoc(); }
  //@}

  /// @name Data accessor functions
  //@{
  /// General datum, where dstType is of the enumeration ::StrDstType
  StStrangeMuDst* GetDatum(Int_t i=0, Bool_t MC=kFALSE, Int_t dstType=evT);

  StStrangeEvMuDst* GetEventI(Bool_t MC=kFALSE);
  StV0I*   GetV0I(Int_t i=0, Bool_t MC=kFALSE);
  StXiI*   GetXiI(Int_t i=0, Bool_t MC=kFALSE); /// Interfaces for data and MC
  StKinkI* GetKinkI(Int_t i=0, Bool_t MC=kFALSE);

  StStrangeEvMuDst* GetEvent(); /// Event information
  StStrangeEvMuDst* GetMcEvent();

  StV0MuDst*      GetV0(Int_t i=0);
  /// See StV0I interface for available functions
  StV0Mc*         GetV0Mc(Int_t i=0);

  StXiMuDst*      GetXi(Int_t i=0);
  /// See StXiI, StV0I interfaces for available functions
  /// (MC also uses StKinkI interface)
  StXiMc*         GetXiMc(Int_t i=0);

  StKinkMuDst*    GetKink(Int_t i=0);
  /// See StKinkI interface for available functions
  StKinkMc*       GetKinkMc(Int_t i=0);

  StStrangeAssoc* GetXiAssoc(Int_t i=0);
  StStrangeAssoc* GetV0Assoc(Int_t i=0);   /// Data-MC association maps
  StStrangeAssoc* GetKinkAssoc(Int_t i=0);

  StStrangeCuts& Cuts();
  //@}

  /// @name Data structure accessor functions
  //@{
  TTree* GetTree();
  TClonesArray* GetEvClonesArray();
  TClonesArray* GetEvMcArray();
  TClonesArray* GetCutsArray();

  TClonesArray* GetV0ClonesArray()   { return v0->GetDataArray(); }
  TClonesArray* GetV0McArray()       { return v0->GetMcArray(); }
  TClonesArray* GetV0AssocArray()    { return v0->GetAssocArray(); }
  TClonesArray* GetXiClonesArray()   { return xi->GetDataArray(); }
  TClonesArray* GetXiMcArray()       { return xi->GetMcArray(); }
  TClonesArray* GetXiAssocArray()    { return xi->GetAssocArray(); }
  TClonesArray* GetKinkClonesArray() { return kink->GetDataArray(); }
  TClonesArray* GetKinkMcArray()     { return kink->GetMcArray(); }
  TClonesArray* GetKinkAssocArray()  { return kink->GetAssocArray(); }
  //@}

  /// @name Maker functions
  //@{
  virtual Int_t Init();
  virtual Int_t Make();
  virtual void  Clear(Option_t *option="");
  virtual Int_t Finish();
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StStrangeMuDstMaker.h,v 3.21 2014/08/06 11:43:44 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
  //@}
  
  /// @name Functions for sub-dsts:
  //@{
  void SubDst(StStrangeMuDstMaker* maker);
  void SubDst(StStrangeMuDstMaker& maker);
  void SubDst(const char* maker_name);
  StStrangeMuDstMaker* GetSubDst();
  //@}
  
  /// @name Sub DST data selection functions
  //@{
  /// Select whole event for sub DST
  void SelectEvent();
  /// Unselect whole event for sub DST
  void UnselectEvent();
  // ...or select portions (use i<0 to select all of the V0s, etc...
  void SelectV0(Int_t i=-1)     { v0->Select(i); }
  void SelectXi(Int_t i=-1)     { xi->Select(i); }
  void SelectKink(Int_t i=-1)   { kink->Select(i); }
  void UnselectV0(Int_t i=-1)   { v0->Unselect(i); }
  void UnselectXi(Int_t i=-1)   { xi->Unselect(i); }
  void UnselectKink(Int_t i=-1) { kink->Unselect(i); }
  //@}

  /// @name Buffer sizes
  //@{
  void SetV0BufferSize(Int_t b)   { bsize[v0T]=b; }
  void SetXiBufferSize(Int_t b)   { bsize[xiT]=b; }
  void SetKinkBufferSize(Int_t b) { bsize[kinkT]=b; }
  //@}

  /// @name Controllers
  //@{
  StStrangeControllerBase* Get(const char* name) const;
  StStrangeControllerBase* Get(Int_t dstType) const;
  //@}
  

 protected:
  /// @name Maker helper functions
  //@{
  virtual void InitReadDst();
  virtual void InitCreateDst();
  virtual void InitCreateSubDst();
  virtual Int_t MakeReadDst();
  virtual Int_t MakeCreateDst();
  virtual Int_t MakeCreateMcDst();
  virtual Int_t MakeCreateSubDst();
  //@}
  
  /// @name File I/O functions
  //@{
  void SetFile(const char* eFile);
  void SetTreeName(const char* treeName);
  Int_t OpenFile();
  Int_t CloseFile();
  void CheckFile();
  //@}

  Int_t MatchName(const char* name) const;

  TTree* tree;                   //!
  TChain* chain;                 //!
  StStrangeCuts* cuts;           //!
  char file[1024];               //!
  TFile* muDst;                  //!

  Bool_t abortEvent;
  Bool_t firstEvent;
  Int_t evNumber;
  Int_t outFileNum;
  Bool_t fileBlind;

  Bool_t doT[strDstT];
  Bool_t doMc;
  Int_t  bsize[strDstT];

  Bool_t doT0JitterAbort;

  TClonesArray* evClonesArray;   //!
  TClonesArray* evMcArray;       //!
  TClonesArray* cutsArray;       //!

  StStrangeMuDstMaker* dstMaker; //!
  TObjArray subMakers;

  
  // Sub-controllers
  StStrangeControllerBase* v0;    //!
  StStrangeControllerBase* xi;    //!
  StStrangeControllerBase* kink;  //!
  StStrangeControllerBase* cont[strDstT];  //!
 private:
  virtual void  ClearForReal(Option_t *option="");

  ClassDef(StStrangeMuDstMaker,4)
};

inline void StStrangeMuDstMaker::DoT0JitterAbort(Bool_t doIt) 
            { doT0JitterAbort = doIt; }
inline void StStrangeMuDstMaker::DoV0(Bool_t doIt)
            { doT[v0T] = doIt; }
inline void StStrangeMuDstMaker::DoXi(Bool_t doIt)
            { doT[xiT] = doIt; }
inline void StStrangeMuDstMaker::DoKink(Bool_t doIt)
            { doT[kinkT] = doIt; }
inline void StStrangeMuDstMaker::Do(Int_t dstType, Bool_t doIt)
            { doT[dstType] = doIt; }
inline void StStrangeMuDstMaker::Do(const char* name, Bool_t doIt)
            { Do(MatchName(name),doIt); }
inline void StStrangeMuDstMaker::DoMc(Bool_t doIt)
            { doMc = doIt; }
inline Bool_t StStrangeMuDstMaker::GetDoMc()
            { return doMc; }
inline TClonesArray* StStrangeMuDstMaker::GetEvClonesArray()
            { return evClonesArray; }
inline TClonesArray* StStrangeMuDstMaker::GetEvMcArray()
            { return evMcArray; }
inline TClonesArray* StStrangeMuDstMaker::GetCutsArray()
            { return cutsArray; }
inline StStrangeMuDst* StStrangeMuDstMaker::GetDatum(Int_t i, Bool_t MC,
                                                     Int_t dstType)
            { if (dstType) return cont[dstType]->Get(i,(int) MC);
              else return (StStrangeMuDst*) GetEventI(MC); }
inline StStrangeEvMuDst* StStrangeMuDstMaker::GetEvent()
            { return (evClonesArray ?
            (StStrangeEvMuDst*) evClonesArray->At(0) : 0); }
inline StStrangeEvMuDst* StStrangeMuDstMaker::GetMcEvent()
            { return (evMcArray ?
            (StStrangeEvMuDst*) evMcArray->At(0) : 0); }
inline StStrangeEvMuDst* StStrangeMuDstMaker::GetEventI(Bool_t MC)
            { return (MC ? GetMcEvent() : GetEvent()); }
inline StV0I* StStrangeMuDstMaker::GetV0I(Int_t i, Bool_t MC)
            { return (StV0I*) (MC ? v0->GetMc(i) : v0->Get(i)); }
inline StXiI* StStrangeMuDstMaker::GetXiI(Int_t i, Bool_t MC)
            { return (StXiI*) (MC ? xi->GetMc(i) : xi->Get(i)); }
inline StKinkI* StStrangeMuDstMaker::GetKinkI(Int_t i, Bool_t MC)
            { return (StKinkI*) (MC ? kink->GetMc(i) : kink->Get(i)); }
inline StV0MuDst* StStrangeMuDstMaker::GetV0(Int_t i)
            { return (StV0MuDst*) v0->Get(i); }
inline StXiMuDst* StStrangeMuDstMaker::GetXi(Int_t i)
            { return (StXiMuDst*) xi->Get(i); }
inline StKinkMuDst* StStrangeMuDstMaker::GetKink(Int_t i)
            { return (StKinkMuDst*) kink->Get(i); }
inline StV0Mc* StStrangeMuDstMaker::GetV0Mc(Int_t i)
            { return (StV0Mc*) v0->GetMc(i); }
inline StXiMc* StStrangeMuDstMaker::GetXiMc(Int_t i)
            { return (StXiMc*) xi->GetMc(i); }
inline StKinkMc* StStrangeMuDstMaker::GetKinkMc(Int_t i)
            { return (StKinkMc*) kink->GetMc(i); }
inline StStrangeAssoc* StStrangeMuDstMaker::GetV0Assoc(Int_t i)
            { return v0->GetAssoc(i); }
inline StStrangeAssoc* StStrangeMuDstMaker::GetXiAssoc(Int_t i)
            { return xi->GetAssoc(i); }
inline StStrangeAssoc* StStrangeMuDstMaker::GetKinkAssoc(Int_t i)
            { return kink->GetAssoc(i); }
inline TTree* StStrangeMuDstMaker::GetTree()
            { return tree; }
inline StStrangeCuts& StStrangeMuDstMaker::Cuts()
            { return (*cuts); }
inline void StStrangeMuDstMaker::SubDst(StStrangeMuDstMaker& maker)
            { SubDst(&maker); }
inline void StStrangeMuDstMaker::SubDst(const char* maker_name)
            { SubDst((StStrangeMuDstMaker*) GetMaker(maker_name)); }
inline StStrangeMuDstMaker* StStrangeMuDstMaker::GetSubDst()
            { return dstMaker; }
inline StStrangeControllerBase* StStrangeMuDstMaker::Get(Int_t dstType) const
            { return cont[dstType]; }
inline StStrangeControllerBase* StStrangeMuDstMaker::Get(const char* name) const
            { return cont[MatchName(name)]; }
inline Int_t StStrangeMuDstMaker::MatchName(const char* name) const
            { for (Int_t i=1; i<strDstT; i++)
	        if (!(strcmp(name,strTypeNames[i]))) return i;
              return 0; }

#endif

//____________________________________________________________________
//
// $Id: StStrangeMuDstMaker.h,v 3.21 2014/08/06 11:43:44 jeromel Exp $
// $Log: StStrangeMuDstMaker.h,v $
// Revision 3.21  2014/08/06 11:43:44  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 3.20  2009/09/02 19:39:44  genevb
// Fixes to pointer and string conversions (RT ticket 1612), prep for 64-bit
//
// Revision 3.19  2009/08/26 16:56:06  fine
// fix the compilation issues under SL5_64_bits  gcc 4.3.2
//
// Revision 3.18  2004/11/02 17:54:07  genevb
// Leave corrupt file protection to ROOT / Remove my protection
//
// Revision 3.17  2004/07/12 21:45:35  genevb
// Handle missing Event branch info condition
//
// Revision 3.16  2003/10/20 00:21:47  genevb
// Fix a typo for Assoc introduced in vers. 3.13
//
// Revision 3.15  2003/09/02 17:59:04  perev
// gcc 3.2 updates + WarnOff
//
// Revision 3.14  2003/07/09 21:58:30  genevb
// Use Get/SetMode() from StMaker
//
// Revision 3.13  2003/05/30 21:20:19  genevb
// doxygen savvy, encoding of FTPC mults, change virtual funcs
//
// Revision 3.12  2003/02/16 21:28:45  jeromel
// GetCVS() added
//
// Revision 3.11  2003/02/10 16:02:24  genevb
// Now read files using TChains; no splitting of MuDst file
//
// Revision 3.10  2002/06/21 02:44:10  genevb
// handle events without primary vertex better
//
// Revision 3.9  2002/05/29 19:08:16  genevb
// Better handling of improperly closed files
//
// Revision 3.8  2002/04/30 16:02:48  genevb
// Common muDst, improved MC code, better kinks, StrangeCuts now a branch
//
// Revision 3.7  2001/09/14 21:39:02  genevb
// Adjustments to not depend on order in which maker Clear() is called
//
// Revision 3.6  2001/08/23 13:20:56  genevb
// Many bug workarounds...
//
// Revision 3.5  2001/05/04 20:15:14  genevb
// Common interfaces and reorganization of components, add MC event info
//
// Revision 3.4  2000/12/18 21:35:18  genevb
// Introduced variable buffer-sizing
//
// Revision 3.3  2000/09/28 20:16:05  jones
// Added doT0JitterAbort() optio; added fix to CheckFile in case of no file
//
// Revision 3.2  2000/09/07 02:22:10  genevb
// Added AbortEvent() functionality
//
// Revision 3.1  2000/07/17 20:28:40  genevb
// File size limitation workaround, some under the hood improvements
//
// Revision 3.0  2000/07/14 12:56:50  genevb
// Revision 3 has event multiplicities and dedx information for vertex tracks
//
// Revision 2.1  2000/06/09 22:17:11  genevb
// Allow MC data to be copied between DSTs, other small improvements
//
// Revision 2.0  2000/06/02 22:11:54  genevb
// New version of Strangeness micro DST package
//
// Revision 1.5  2000/04/18 02:30:04  genevb
// Added multi-file capabilities
//
// Revision 1.4  2000/04/06 14:51:11  genevb
// Fixed bug with storing event info when making subDST
//
// Revision 1.3  2000/04/05 20:23:53  genevb
// Introduce creating sub-Micro DSTs, dynamic expansion of clones arrays as needed, SetNoKeep() function
//
// Revision 1.2  2000/03/29 20:52:13  genevb
// Added StKinkMuDst, replaced arrays
//
// Revision 1.1  2000/03/29 03:10:07  genevb
// Introduction of Strangeness Micro DST package
//
