/***********************************************************************
 *
 * $Id: StStrangeEvMuDst.hh,v 3.0 2000/07/14 12:56:49 genevb Exp $
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
 * Revision 3.0  2000/07/14 12:56:49  genevb
 * Revision 3 has event multiplicities and dedx information for vertex tracks
 *
 * Revision 2.1  2000/06/09 22:17:10  genevb
 * Allow MC data to be copied between DSTs, other small improvements
 *
 * Revision 2.0  2000/06/02 22:11:54  genevb
 * New version of Strangeness micro DST package
 *
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
#include "StStrangeMuDst.hh"

class StEvent;

class StStrangeEvMuDst : public StStrangeMuDst {
public:
  StStrangeEvMuDst();
  ~StStrangeEvMuDst();
  StStrangeEvMuDst(StEvent&);
  void Fill(StEvent&);
  void Clear() {}

  Int_t   run() const;            // Run number
  Int_t   event() const;          // Event number
  Float_t primaryVertexX() const; // Primary Vertex Position coordinates
  Float_t primaryVertexY() const;
  Float_t primaryVertexZ() const;
  Int_t   globalTracks() const;
  Int_t   primaryTracks() const;

protected:
  Int_t   mRun;                   // These are written out
  Int_t   mEvent;
  Float_t mPrimaryVertexX;
  Float_t mPrimaryVertexY;
  Float_t mPrimaryVertexZ;
  Int_t   mGlobalTracks;
  Int_t   mPrimaryTracks;

  ClassDef(StStrangeEvMuDst,3)
};

inline         StStrangeEvMuDst::StStrangeEvMuDst(StEvent& event)
               { Fill(event); }
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
inline Int_t   StStrangeEvMuDst::globalTracks() const
               { return mGlobalTracks; }
inline Int_t   StStrangeEvMuDst::primaryTracks() const
               { return mPrimaryTracks; }

#endif
