/***********************************************************************
 *
 * $Id: StStrangeEvMuDst.hh,v 1.3 2000/03/31 03:20:24 jones Exp $
 *
 * Authors: Gene Van Buren, UCLA, 24-Mar-2000
 *          Peter G. Jones, University of Birmingham, 19-Aug-1999
 *
 ***********************************************************************
 *
 * Description: Strangeness event micro dst class
 *
 ***********************************************************************
 *
 * $Log: StStrangeEvMuDst.hh,v $
 * Revision 1.3  2000/03/31 03:20:24  jones
 * Added topology map to V0/Xi; access funcs for each data member
 *
 * Revision 1.2  2000/03/29 20:52:13  genevb
 * Added StKinkMuDst, replaced arrays
 *
 * Revision 1.1  2000/03/29 03:10:07  genevb
 * Int_troduction of Strangeness Micro DST package
 *
 *
 ***********************************************************************/
#ifndef StStrangeEvMuDst_hh
#define StStrangeEvMuDst_hh
#include "TObject.h"

class StPrimaryVertex;
class StEvVertex;

class StStrangeEvMuDst : public TObject {
public:
  StStrangeEvMuDst();
  ~StStrangeEvMuDst();
  StStrangeEvMuDst(StPrimaryVertex*);
  void Fill(StPrimaryVertex*);
  void Clear() {}

  Int_t   run() const;            // Run number
  Int_t   event() const;          // Event number
  Float_t primaryVertexX() const; // Primary Vertex Position coordinates
  Float_t primaryVertexY() const;
  Float_t primaryVertexZ() const;

protected:
  Int_t   mRun;                   // These are written out
  Int_t   mEvent;
  Float_t mPrimaryVertexX;
  Float_t mPrimaryVertexY;
  Float_t mPrimaryVertexZ;

  ClassDef(StStrangeEvMuDst, 1)
};

inline         StStrangeEvMuDst::StStrangeEvMuDst(StPrimaryVertex* pv)
               { Fill(pv); }
inline Int_t   StStrangeEvMuDst::run() const
               { return mRun; }
inline Int_t   StStrangeEvMuDst::event() const
               { return mEvent; }
inline Float_t StStrangeEvMuDst::primaryVertexX() const 
               { return mPrimaryVertexX; }
inline Float_t StStrangeEvMuDst::primaryVertexY() const 
               { return mPrimaryVertexY; }
inline Float_t StStrangeEvMuDst::primaryVertexZ() const 
               { return mPrimaryVertexZ; }

#endif
