/***********************************************************************
 *
 * $Id: StStrangeEvMuDst.hh,v 1.1 2000/03/29 03:10:07 genevb Exp $
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
 * Revision 1.1  2000/03/29 03:10:07  genevb
 * Introduction of Strangeness Micro DST package
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

  int   run() const;            // Run number
  int   event() const;          // Event number
  float *primaryVertex();       // Primary Vertex Position

protected:
  int   mRun;                   // These are written out
  int   mEvent;
  float mPrimaryVertex[3];

  ClassDef(StStrangeEvMuDst, 1)
};

inline       StStrangeEvMuDst::StStrangeEvMuDst(StPrimaryVertex* pv)
             { Fill(pv); }
inline int   StStrangeEvMuDst::run() const
             { return mRun; }
inline int   StStrangeEvMuDst::event() const
             { return mEvent; }
inline float *StStrangeEvMuDst::primaryVertex()
             { return mPrimaryVertex; }

#endif
