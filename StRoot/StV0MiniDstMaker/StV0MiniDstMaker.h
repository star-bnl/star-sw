// $Id: StV0MiniDstMaker.h,v 1.4 1999/11/19 19:44:48 genevb Exp $
// $Log: StV0MiniDstMaker.h,v $
// Revision 1.4  1999/11/19 19:44:48  genevb
// Modified for StEvent 2.0
//
// Revision 1.3  1999/09/02 09:04:57  jones
// Added StEvMiniDst class, New file handling, Partially implemented TTrees
//
// Revision 1.2  1999/08/13 12:38:17  jones
// Major revision to merge StV0MiniDstMaker and StXiMiniDstMaker
//
// Revision 1.1  1999/07/13 12:42:25  jones
// *** empty log message ***
//
#ifndef STAR_StV0MiniDstMaker
#define STAR_StV0MiniDstMaker
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StV0MiniDstMaker virtual base class for Maker                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "StMaker.h"
#include "StEnumerations.h"

class TOrdCollection;
class TClonesArray;

class StV0MiniDstMaker : public StMaker {
 public: 
  StV0MiniDstMaker(const char *name="mDst");
  virtual ~StV0MiniDstMaker();
  void SetXiVertexType();
  void SetV0VertexType();
  Int_t SetInputFile(const char* muDst="muDst.root");
  Int_t SetOutputFile(const char* muDst="muDst.root");
  void SetUseTree(Bool_t k=kFALSE);   
  TOrdCollection* Read(Int_t*);
  TOrdCollection* GetCollection();
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();
 protected:
  StVertexId mVertexType;    //!
  const Char_t *mFileName;     //!
  TClonesArray *mClonesArray;  //!
  TOrdCollection *mCollection; //!
  Int_t mEntries;              //!
  Bool_t mUseTree;             //!
  Bool_t mWriteFile;           //!
 private:
  ClassDef(StV0MiniDstMaker,1)   //virtual base class for Makers
};

inline void StV0MiniDstMaker::SetXiVertexType()
            { mVertexType = kXiVtxId; }
inline void StV0MiniDstMaker::SetV0VertexType()
            { mVertexType = kV0VtxId; }
inline void StV0MiniDstMaker::SetUseTree(Bool_t k)
            { mUseTree = k; }

inline TOrdCollection* StV0MiniDstMaker::GetCollection()
            { return mCollection; }
#endif
