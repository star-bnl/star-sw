// $Id: StStrangeMuDstMaker.h,v 2.0 2000/06/02 22:11:54 genevb Exp $
// $Log: StStrangeMuDstMaker.h,v $
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
  
  void DoV0(Bool_t doIt=kTRUE);
  void DoXi(Bool_t doIt=kTRUE);
  void DoKink(Bool_t doIt=kTRUE);
  void DoMc(Bool_t doIt=kTRUE);
  Bool_t GetDoMc();
  
  StStrangeControllerBase* Get(const char* name) const;
  
  TClonesArray* GetEvClonesArray();

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
  virtual void SubDst(StStrangeMuDstMaker* maker);
  virtual void SubDst(StStrangeMuDstMaker& maker);
  virtual void SubDst(const char* maker_name);
  virtual StStrangeMuDstMaker* GetSubDst();
  
  // Selects entire event for sub DST...
  virtual void SelectEvent();          // selects whole event for sub DST
  // ...or select portions (use i<0 to select all of the V0s, etc...
  virtual void SelectV0(Int_t i=-1)   { v0->Select(i); }
  virtual void SelectXi(Int_t i=-1)   { xi->Select(i); }
  virtual void SelectKink(Int_t i=-1) { kink->Select(i); }
  
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
  
  TTree* tree;                   //!
  StStrangeCuts* cuts;           //!
  char* evFile;                  //!
  char* v0File;                  //!
  char* xiFile;                  //!
  char* kinkFile;                //!
  StFile* evFiles;               //!
  StFile* v0Files;               //!
  StFile* xiFiles;               //!
  StFile* kinkFiles;             //!
  TFile* muDst;                  //!

  Bool_t firstEvent;
  Int_t evNumber;

  Bool_t doV0;
  Bool_t doXi;
  Bool_t doKink;
  Bool_t doMc;

  StrangeEnum rw;
  TClonesArray* evClonesArray;   //!

  StStrangeMuDstMaker* dstMaker; //!

  
  // Sub-controllers
  StStrangeControllerBase* v0;    //!
  StStrangeControllerBase* xi;    //!
  StStrangeControllerBase* kink;  //!
 private:
  ClassDef(StStrangeMuDstMaker,1)
};

inline StrangeEnum StStrangeMuDstMaker::GetMode()
            { return rw; }
inline void StStrangeMuDstMaker::DoV0(Bool_t doIt)
            { doV0 = doIt; }
inline void StStrangeMuDstMaker::DoXi(Bool_t doIt)
            { doXi = doIt; }
inline void StStrangeMuDstMaker::DoKink(Bool_t doIt)
            { doKink = doIt; }
inline void StStrangeMuDstMaker::DoMc(Bool_t doIt)
            { doMc = doIt; }
inline Bool_t StStrangeMuDstMaker::GetDoMc()
            { return doMc; }
inline TClonesArray* StStrangeMuDstMaker::GetEvClonesArray()
            { return evClonesArray; }
inline StStrangeEvMuDst* StStrangeMuDstMaker::GetEvent()
            { return (evClonesArray ?
            (StStrangeEvMuDst*) (*evClonesArray)[0] : 0); }
inline TTree* StStrangeMuDstMaker::GetTree()
            { return tree; }
inline StStrangeCuts& StStrangeMuDstMaker::Cuts()
            { return (*cuts); }
inline void StStrangeMuDstMaker::SubDst(StStrangeMuDstMaker* maker)
            { dstMaker = maker; }
inline void StStrangeMuDstMaker::SubDst(StStrangeMuDstMaker& maker)
            { dstMaker = &maker; }
inline void StStrangeMuDstMaker::SubDst(const char* maker_name)
            { SubDst((StStrangeMuDstMaker*) GetMaker(maker_name)); }
inline StStrangeMuDstMaker* StStrangeMuDstMaker::GetSubDst()
            { return dstMaker; }
inline StStrangeControllerBase* StStrangeMuDstMaker::Get(const char* name) const
            { if (!(strcmp(name,"V0"))) return v0;
	      if (!(strcmp(name,"Xi"))) return xi;
	      if (!(strcmp(name,"Kink"))) return kink;
	      return 0; }

#endif
