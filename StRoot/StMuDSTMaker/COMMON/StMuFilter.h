/***************************************************************************
 *
 * $Id: StMuFilter.h,v 1.1 2002/03/08 17:04:18 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#ifndef StMuFilter_h
#define StMuFilter_h

#include "TObject.h"
#include "StMuCut.h"


class StMuFilter : public StMuCut, public TObject{
 private:
  virtual bool accept( const StEvent*);
  virtual bool accept( const StTrack*);
  virtual bool accept( const StV0Vertex*);
  virtual bool accept( const StXiVertex*);
  virtual bool accept( const StKinkVertex*);

  ClassDef(StMuFilter,1)
};

#endif

/***************************************************************************
 *
 * $Log: StMuFilter.h,v $
 * Revision 1.1  2002/03/08 17:04:18  laue
 * initial revision
 *
 *
 **************************************************************************/
