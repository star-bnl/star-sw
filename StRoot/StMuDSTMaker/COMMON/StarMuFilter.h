/***************************************************************************
 *
 * $Id: StarMuFilter.h,v 1.1 2002/03/05 15:41:09 jeromel Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#ifndef StarMuFilter_h
#define StarMuFilter_h

#include "TObject.h"
#include "StarMuCut.h"


class StarMuFilter : public StarMuCut, public TObject{
 private:
  virtual bool accept( const StEvent*);
  virtual bool accept( const StTrack*);
  virtual bool accept( const StV0Vertex*);
  virtual bool accept( const StXiVertex*);
  virtual bool accept( const StKinkVertex*);

  ClassDef(StarMuFilter,1)
};

#endif

/***************************************************************************
 *
 * $Log: StarMuFilter.h,v $
 * Revision 1.1  2002/03/05 15:41:09  jeromel
 * First version of Frank's Commone MicroDST.
 *
 *
 **************************************************************************/
