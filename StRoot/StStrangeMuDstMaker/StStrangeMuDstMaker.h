// $Id: StStrangeMuDstMaker.h,v 1.2 2000/03/29 20:52:13 genevb Exp $
// $Log: StStrangeMuDstMaker.h,v $
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

enum StrangeEnum {StrangeNoFile, StrangeWrite, StrangeRead};

class StStrangeMuDstMaker : public StMaker {
 public: 
  StStrangeMuDstMaker(const char *name="strangeMuDst");
  virtual ~StStrangeMuDstMaker();
  void SetRead (char* eFile=0, char* vFile=0, char* xFile=0, char* kFile=0);
  void SetWrite(char* eFile=0, char* vFile=0, char* xFile=0, char* kFile=0);
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
 protected:
  void SetFiles(char* eFile, char* vFile, char* xFile, char* kFile);
  Int_t OpenFile();
  TTree* tree;                   //!
  char* evFile;                  //!
  char* v0File;                  //!
  char* xiFile;                  //!
  char* kinkFile;                //!
  TFile* muDst;                  //!
  Bool_t doV0;
  Bool_t doXi;
  Bool_t doKink;
  StrangeEnum rw;
  TClonesArray* evClonesArray;   //!
  TClonesArray* v0ClonesArray;   //!
  TClonesArray* xiClonesArray;   //!
  TClonesArray* kinkClonesArray; //!
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
#endif
