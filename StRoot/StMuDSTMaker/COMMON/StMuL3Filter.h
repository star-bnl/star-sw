/***************************************************************************
 *
 * $Id: StMuL3Filter.h,v 1.3 2002/05/04 23:56:30 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#ifndef StMuL3Filter_h
#define StMuL3Filter_h

#include "TObject.h"
#include "StMuCut.h"

class BetheBloch;
  
class StMuL3Filter : public StMuCut, public TObject{
 public: 
  StMuL3Filter();
  ~StMuL3Filter();
 private: 
  BetheBloch* mBB;

  bool accept( const StEvent*);
  bool accept( const StTrack*);
  bool accept( const StV0Vertex*);
  bool accept( const StXiVertex*);
  bool accept( const StKinkVertex*);
  bool accept( const StV0MuDst*);
  bool accept( const StXiMuDst*);
  bool accept( const StKinkMuDst*);

  ClassDef(StMuL3Filter,1)
};

#endif

/***************************************************************************
 *
 * $Log: StMuL3Filter.h,v $
 * Revision 1.3  2002/05/04 23:56:30  laue
 * some documentation added
 *
 * Revision 1.2  2002/03/20 16:04:12  laue
 * minor changes, mostly added access functions
 *
 * Revision 1.1  2002/03/08 17:04:18  laue
 * initial revision
 *
 *
 **************************************************************************/
