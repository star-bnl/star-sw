// $Id: StStrangeMuDstMaker.h,v 1.5 2000/04/18 02:30:04 genevb Exp $
// $Log: StStrangeMuDstMaker.h,v $
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
#ifndef STAR_StStrangeMuDstMaker
#define STAR_StStrangeMuDstMaker
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StStrangeMuDstMaker strangeness micro DST maker                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "StMaker.h"

class TFile;
class TTree;
class StStrangeEvMuDst;
class StV0MuDst;
class StXiMuDst;
class StKinkMuDst;
class TClonesArray;
class TArrayI;
class StFile;

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
  void DoV0(Bool_t doIt=kTRUE);
  void DoXi(Bool_t doIt=kTRUE);
  void DoKink(Bool_t doIt=kTRUE);
  TClonesArray* GetEvClonesArray();
  TClonesArray* GetV0ClonesArray();
  TClonesArray* GetXiClonesArray();
  TClonesArray* GetKinkClonesArray();
  Int_t GetNV0();
  Int_t GetNXi();
  Int_t GetNKink();
  StStrangeEvMuDst* GetEvent();
  StV0MuDst* GetV0(Int_t i=0);
  StXiMuDst* GetXi(Int_t i=0);
  StKinkMuDst* GetKink(Int_t i=0);
  TTree* GetTree();
  virtual Int_t Init();
  virtual Int_t Make();
  virtual void  Clear(Option_t *option="");
  virtual Int_t Finish();
  // Functions for sub-dsts:
  virtual void SubDst(StStrangeMuDstMaker* maker);
  virtual void SubDst(StStrangeMuDstMaker& maker);
  virtual void SubDst(const char* maker_name);
  virtual void SelectEvent();          // selects whole event for sub DST
  virtual void SelectV0(Int_t i=-1);   // use i<0 to specify whole event
  virtual void SelectXi(Int_t i=-1);   // use i<0 to specify whole event
  virtual void SelectKink(Int_t i=-1); // use i<0 to specify whole event
 protected:
  virtual void InitReadDst();
  virtual void InitCreateDst();
  virtual void InitCreateSubDst();
  virtual Int_t MakeReadDst();
  virtual Int_t MakeCreateDst();
  virtual Int_t MakeCreateSubDst();
  void SetFiles(char* eFile, char* vFile, char* xFile, char* kFile);
  void SetStFiles();
  Int_t OpenFile();
  Int_t CloseFile();
  TTree* tree;                   //!
  char* evFile;                  //!
  char* v0File;                  //!
  char* xiFile;                  //!
  char* kinkFile;                //!
  StFile* evFiles;               //!
  StFile* v0Files;               //!
  StFile* xiFiles;               //!
  StFile* kinkFiles;             //!
  Int_t evNumber;                //!
  TFile* muDst;                  //!
  Bool_t doV0;
  Bool_t doXi;
  Bool_t doKink;
  StrangeEnum rw;
  TClonesArray* evClonesArray;   //!
  TClonesArray* v0ClonesArray;   //!
  TClonesArray* xiClonesArray;   //!
  TClonesArray* kinkClonesArray; //!
  StStrangeMuDstMaker* dstMaker; //!
  // Arrays of muDst indices to copy
  TArrayI* v0Selections;         //!
  TArrayI* xiSelections;         //!
  TArrayI* kinkSelections;       //!
  // Totals for entire set
  Int_t nV0Entries;
  Int_t nXiEntries;
  Int_t nKinkEntries;
  // Totals for events
  Int_t v0Entries;
  Int_t xiEntries;
  Int_t kinkEntries;
 private:
  ClassDef(StStrangeMuDstMaker,1)
};

inline void StStrangeMuDstMaker::DoV0(Bool_t doIt)
            { doV0 = doIt; }
inline void StStrangeMuDstMaker::DoXi(Bool_t doIt)
            { doXi = doIt; }
inline void StStrangeMuDstMaker::DoKink(Bool_t doIt)
            { doKink = doIt; }
inline TClonesArray* StStrangeMuDstMaker::GetEvClonesArray()
            { return evClonesArray; }
inline TClonesArray* StStrangeMuDstMaker::GetV0ClonesArray()
            { return v0ClonesArray; }
inline TClonesArray* StStrangeMuDstMaker::GetXiClonesArray()
            { return xiClonesArray; }
inline TClonesArray* StStrangeMuDstMaker::GetKinkClonesArray()
            { return kinkClonesArray; }
inline TTree* StStrangeMuDstMaker::GetTree()
            { return tree; }
inline void StStrangeMuDstMaker::SubDst(StStrangeMuDstMaker* maker)
            { dstMaker = maker; }
inline void StStrangeMuDstMaker::SubDst(StStrangeMuDstMaker& maker)
            { dstMaker = &maker; }
inline void StStrangeMuDstMaker::SubDst(const char* maker_name)
            { SubDst((StStrangeMuDstMaker*) GetMaker(maker_name)); }
#endif
