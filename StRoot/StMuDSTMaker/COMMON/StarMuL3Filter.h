/***************************************************************************
 *
 * $Id: StarMuL3Filter.h,v 1.1 2002/03/05 15:41:10 jeromel Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#ifndef StarMuL3Filter_h
#define StarMuL3Filter_h

#include "TObject.h"
#include "StarMuCut.h"


class StarMuL3Filter : public StarMuCut, public TObject{
 private:
  virtual bool accept( const StEvent*);
  virtual bool accept( const StTrack*);
  virtual bool accept( const StV0Vertex*);
  virtual bool accept( const StXiVertex*);
  virtual bool accept( const StKinkVertex*);

  ClassDef(StarMuL3Filter,1)
};

#endif

/***************************************************************************
 *
 * $Log: StarMuL3Filter.h,v $
 * Revision 1.1  2002/03/05 15:41:10  jeromel
 * First version of Frank's Commone MicroDST.
 *
 *
 **************************************************************************/
