/***************************************************************************
 *
 * $Id: StMuL3Filter.h,v 1.1 2002/03/08 17:04:18 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#ifndef StMuL3Filter_h
#define StMuL3Filter_h

#include "TObject.h"
#include "StMuCut.h"


class StMuL3Filter : public StMuCut, public TObject{
 private:
  virtual bool accept( const StEvent*);
  virtual bool accept( const StTrack*);
  virtual bool accept( const StV0Vertex*);
  virtual bool accept( const StXiVertex*);
  virtual bool accept( const StKinkVertex*);

  ClassDef(StMuL3Filter,1)
};

#endif

/***************************************************************************
 *
 * $Log: StMuL3Filter.h,v $
 * Revision 1.1  2002/03/08 17:04:18  laue
 * initial revision
 *
 *
 **************************************************************************/
