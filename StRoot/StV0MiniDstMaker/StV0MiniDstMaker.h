// $Id: StV0MiniDstMaker.h,v 1.2 1999/08/13 12:38:17 jones Exp $
// $Log: StV0MiniDstMaker.h,v $
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

class StV0MiniDstMaker : public StMaker {
 private:
 protected:
  StVertexType mVertexType;    //!
  const Char_t *mFileName;     //!
  TOrdCollection *mCollection; //!
  Int_t mEntries;              //!
 public: 
  StV0MiniDstMaker(const char *name="mDst", 
		   const char *dst="mDst.root");
  virtual ~StV0MiniDstMaker();
          void  SetXiVertexType();
          void  SetV0VertexType();
  TOrdCollection* Read(Int_t*);
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();
  ClassDef(StV0MiniDstMaker,1)   //virtual base class for Makers
};

inline void StV0MiniDstMaker::SetXiVertexType()
            { mVertexType = Xi; }
inline void StV0MiniDstMaker::SetV0VertexType()
            { mVertexType = V0; }

#endif
