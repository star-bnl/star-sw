/***********************************************************************
 *
 * $Id: StEvMiniDst.hh,v 1.3 1999/11/19 19:44:47 genevb Exp $
 *
 * Author: Peter G. Jones, University of Birmingham, 19-Aug-1999
 *
 ***********************************************************************
 *
 * Description: Event mini dst class
 *
 ***********************************************************************
 *
 * $Log: StEvMiniDst.hh,v $
 * Revision 1.3  1999/11/19 19:44:47  genevb
 * Modified for StEvent 2.0
 *
 * Revision 1.2  1999/09/24 01:23:32  fisyak
 * Reduced Include Path
 *
 * Revision 1.1  1999/09/02 09:04:55  jones
 * Added StEvMiniDst class, New file handling, Partially implemented TTrees
 *
 *
 ***********************************************************************/
#ifndef StEvMiniDst_hh
#define StEvMiniDst_hh
#include "StHFillObject.h"

class StPrimaryVertex;
class StEvVertex;

class StEvMiniDst : public StHFillObject {
public:
  StEvMiniDst();
  ~StEvMiniDst();
  StEvMiniDst(StPrimaryVertex*);
  void UpdateEv();

  int   run() const;            // Run number
  int   event() const;          // Event number
  float *primaryVertex();       // Primary Vertex Position

protected:
  int   mRun;                   // These are written out
  int   mEvent;
  float mPrimaryVertex[3];

  ClassDef(StEvMiniDst, 1)
};

inline int   StEvMiniDst::run() const
             { return mRun; }
inline int   StEvMiniDst::event() const
             { return mEvent; }
inline float *StEvMiniDst::primaryVertex()
             { return mPrimaryVertex; }

#endif
