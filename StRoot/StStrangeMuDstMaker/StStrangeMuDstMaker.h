// $Id: StStrangeMuDstMaker.h,v 1.1 2000/03/29 03:10:07 genevb Exp $
// $Log: StStrangeMuDstMaker.h,v $
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
class TClonesArray;

enum StrangeEnum {StrangeNoFile, StrangeWrite, StrangeRead};

class StStrangeMuDstMaker : public StMaker {
 public: 
  StStrangeMuDstMaker(const char *name="strangeMuDst");
  virtual ~StStrangeMuDstMaker();
  void SetRead (char* eFile=0, char* vFile=0, char* xFile=0);
  void SetWrite(char* eFile=0, char* vFile=0, char* xFile=0);
  void DoV0(Bool_t doIt=kTRUE);
  void DoXi(Bool_t doIt=kTRUE);
  TClonesArray* GetEvClonesArray();
  TClonesArray* GetV0ClonesArray();
  TClonesArray* GetXiClonesArray();
  Int_t GetNV0();
  Int_t GetNXi();
  StStrangeEvMuDst* GetEvent();
  StV0MuDst* GetV0(Int_t i=0);
  StXiMuDst* GetXi(Int_t i=0);
  TTree* GetTree();
  virtual Int_t Init();
  virtual Int_t Make();
  virtual void  Clear(Option_t *option="");
  virtual Int_t Finish();
 protected:
  void SetFiles(char* eFile, char* vFile, char* xFile);
  Int_t OpenFile();
  TTree* tree;                 //!
  char* evFile;                //!
  char* v0File;                //!
  char* xiFile;                //!
  TFile* muDst;                //!
  Bool_t doV0;
  Bool_t doXi;
  StrangeEnum rw;
  TClonesArray* evClonesArray; //!
  TClonesArray* v0ClonesArray; //!
  TClonesArray* xiClonesArray; //!
 private:
  ClassDef(StStrangeMuDstMaker,1)
};

inline void StStrangeMuDstMaker::DoV0(Bool_t doIt)
            { doV0 = doIt; }
inline void StStrangeMuDstMaker::DoXi(Bool_t doIt)
            { doXi = doIt; }
inline TClonesArray* StStrangeMuDstMaker::GetEvClonesArray()
            { return evClonesArray; }
inline TClonesArray* StStrangeMuDstMaker::GetV0ClonesArray()
            { return v0ClonesArray; }
inline TClonesArray* StStrangeMuDstMaker::GetXiClonesArray()
            { return xiClonesArray; }
inline TTree* StStrangeMuDstMaker::GetTree()
            { return tree; }
#endif
