/***************************************************************************
 * $Id: StGenericVertexFinder.h,v 1.1 2003/05/09 22:22:36 lbarnby Exp $
 *
 * Author: Lee Barnby, April 2003
 *
 ***************************************************************************
 * Description: Base class for vertex finders
 *
 ***************************************************************************/

#ifndef STAR_StGenericVertexFinder
#define STAR_StGenericVertexFinder

#include "StEventTypes.h"


class StEvent;

class StGenericVertexFinder {
 public:
  virtual bool fit(StEvent*)=0;
  virtual int status() const =0;
  virtual StThreeVectorD result() const=0;
  virtual StThreeVectorD error() const=0;
  //virtual ~StGenericVertexFinder();
  void FillStEvent(StEvent*) const;
};

// $Log: StGenericVertexFinder.h,v $
// Revision 1.1  2003/05/09 22:22:36  lbarnby
// Initial revision: a base class for STAR (StEvent-based) vertex finders
//
#endif
