// $Id: StStrangeMuDstMaker.h,v 3.8 2002/04/30 16:02:48 genevb Exp $
// $Log: StStrangeMuDstMaker.h,v $
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
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StStrangeMuDstMaker strangeness micro DST maker                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef STAR_StStrangeMuDstMaker
#define STAR_StStrangeMuDstMaker
#include "StMaker.h"
#include "TClonesArray.h"
#include "StStrangeControllerBase.h"

class TFile;
class TTree;
class StStrangeEvMuDst;
class StV0MuDst;
class StXiMuDst;
class StKinkMuDst;
class StStrangeAssoc;
class StFile;
class StV0Mc;
class StXiMc;
class StKinkMc;
class StStrangeCuts;

enum StrangeEnum {StrangeNoKeep, StrangeNoFile, StrangeWrite, StrangeRead};
enum StrDstType  {evT = 0, v0T, xiT, kinkT, strDstT} ;
static char* strTypeNames[strDstT] = {"Ev","V0","Xi","Kink"};

class StStrangeMuDstMaker : public StMaker {
 public: 
  StStrangeMuDstMaker(const char *name="strangeMuDst");
  virtual ~StStrangeMuDstMaker();
  void SetRead (char* eFile=0, char* vFile=0, char* xFile=0, char* kFile=0);
  void SetRead (StFile* eFiles, StFile* vFiles=0,
                StFile* xFiles=0, StFile* kFiles=0);
  void SetWrite(char* eFile=0, char* vFile=0, char* xFile=0, char* kFile=0);
  void SetNoKeep();
  StrangeEnum GetMode();
  char* GetFile(Int_t dstType) const;
  void DoT0JitterAbort(Bool_t doIt=kTRUE);

  void DoV0(Bool_t doIt=kTRUE);
  void DoXi(Bool_t doIt=kTRUE);
  void DoKink(Bool_t doIt=kTRUE);
  void DoMc(Bool_t doIt=kTRUE);
  void Do(Int_t dstType, Bool_t doIt=kTRUE);
  void Do(const char* name, Bool_t doIt=kTRUE);
  Bool_t GetDoMc();

  void SetCorrectionFile(char*);
  void SetFractionFile(char*);

  
  StStrangeControllerBase* Get(const char* name) const;
  StStrangeControllerBase* Get(Int_t dstType) const;
  
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

  Int_t GetNV0()        { return v0->GetN(); }
  Int_t GetNV0Mc()      { return v0->GetNMc(); }
  Int_t GetNV0Assoc()   { return v0->GetNAssoc(); }
  Int_t GetNXi()        { return xi->GetN(); }
  Int_t GetNXiMc()      { return xi->GetNMc(); }
  Int_t GetNXiAssoc()   { return xi->GetNAssoc(); }
  Int_t GetNKink()      { return kink->GetN(); }
  Int_t GetNKinkMc()    { return kink->GetNMc(); }
  Int_t GetNKinkAssoc() { return kink->GetNAssoc(); }

  StStrangeEvMuDst* GetEvent();
  StStrangeEvMuDst* GetMcEvent();

  StV0MuDst* GetV0(Int_t i=0)              { return (StV0MuDst*) v0->Get(i); }
  StV0Mc* GetV0Mc(Int_t i=0)               { return (StV0Mc*) v0->GetMc(i); }
  StStrangeAssoc* GetV0Assoc(Int_t i=0)    { return v0->GetAssoc(i); }
  StXiMuDst* GetXi(Int_t i=0)              { return (StXiMuDst*) xi->Get(i); }
  StXiMc* GetXiMc(Int_t i=0)               { return (StXiMc*) xi->GetMc(i); }
  StStrangeAssoc* GetXiAssoc(Int_t i=0)    { return xi->GetAssoc(i); }
  StKinkMuDst* GetKink(Int_t i=0)          { return (StKinkMuDst*) kink->Get(i); }
  StKinkMc* GetKinkMc(Int_t i=0)           { return (StKinkMc*) kink->GetMc(i); }
  StStrangeAssoc* GetKinkAssoc(Int_t i=0)  { return kink->GetAssoc(i); }

  TTree* GetTree();
  StStrangeCuts& Cuts();  virtual Int_t Init();
  
  virtual Int_t Make();
  virtual void  Clear(Option_t *option="");
  virtual Int_t Finish();
  
  // Functions for sub-dsts:
  void SubDst(StStrangeMuDstMaker* maker);
  void SubDst(StStrangeMuDstMaker& maker);
  void SubDst(const char* maker_name);
  StStrangeMuDstMaker* GetSubDst();
  
  // Selects entire event for sub DST...
  void SelectEvent();                  // selects whole event for sub DST
  void UnselectEvent();                // unselects whole event for sub DST
  // ...or select portions (use i<0 to select all of the V0s, etc...
  void SelectV0(Int_t i=-1)     { v0->Select(i); }
  void SelectXi(Int_t i=-1)     { xi->Select(i); }
  void SelectKink(Int_t i=-1)   { kink->Select(i); }
  void UnselectV0(Int_t i=-1)   { v0->Unselect(i); }
  void UnselectXi(Int_t i=-1)   { xi->Unselect(i); }
  void UnselectKink(Int_t i=-1) { kink->Unselect(i); }

  void SetV0BufferSize(Int_t b)   { bsize[v0T]=b; }
  void SetXiBufferSize(Int_t b)   { bsize[xiT]=b; }
  void SetKinkBufferSize(Int_t b) { bsize[kinkT]=b; }

  // turn off filling of TTree for this event, regardless
  void AbortEvent() { abortEvent = kTRUE; }

 protected:
  virtual void InitReadDst();
  virtual void InitCreateDst();
  virtual void InitCreateSubDst();
  virtual Int_t MakeReadDst();
  virtual Int_t MakeCreateDst();
  virtual Int_t MakeCreateMcDst();
  virtual Int_t MakeCreateSubDst();
  
  void SetFiles(char* eFile, char* vFile, char* xFile, char* kFile);
  void SetStFiles();
  Int_t OpenFile();
  Int_t CloseFile();
  void CheckFile();

  Int_t MatchName(const char* name) const;

  TTree* tree;                   //!
  StStrangeCuts* cuts;           //!
  char* file[strDstT];           //!
  StFile* files[strDstT];        //!
  TFile* muDst;                  //!

  Bool_t abortEvent;
  Bool_t firstEvent;
  Int_t evNumber;
  Int_t outFileNum;

  Bool_t doT[strDstT];
  Bool_t doMc;
  Int_t  bsize[strDstT];

  Bool_t doT0JitterAbort;

  StrangeEnum rw;
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

inline StrangeEnum StStrangeMuDstMaker::GetMode()
            { return rw; }
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
inline StStrangeEvMuDst* StStrangeMuDstMaker::GetEvent()
            { return (evClonesArray ?
            (StStrangeEvMuDst*) (*evClonesArray)[0] : 0); }
inline StStrangeEvMuDst* StStrangeMuDstMaker::GetMcEvent()
            { return (evMcArray ?
            (StStrangeEvMuDst*) (*evMcArray)[0] : 0); }
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
inline char* StStrangeMuDstMaker::GetFile(Int_t dstType) const
            { return file[dstType]; }
inline Int_t StStrangeMuDstMaker::MatchName(const char* name) const
            { for (Int_t i=1; i<strDstT; i++)
	        if (!(strcmp(name,strTypeNames[i]))) return i;
              return 0; }

#endif
