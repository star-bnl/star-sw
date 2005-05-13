// $Id: StSpaListStrip.hh,v 1.2 2005/05/13 08:39:32 lmartin Exp $
//
// $Log: StSpaListStrip.hh,v $
// Revision 1.2  2005/05/13 08:39:32  lmartin
// CVS tags added
//

#ifndef STSPALISTSTRIP_HH
#define STSPALISTSTRIP_HH
#include <stdlib.h>
#include <math.h>
#include "StSpaStrip.hh"
#include "StSpaNoise.hh"
#include "StSpaListNoise.hh"


class StSpaListNoise;
class StSpaListStrip
{
 public:
  StSpaListStrip();
  ~StSpaListStrip();

  StSpaStrip*     next(StSpaStrip *ptr);
  StSpaStrip*     prev(StSpaStrip *ptr);
  StSpaStrip*     first();
  StSpaStrip*     last();
  int             addNewStrip(StSpaStrip *ptr);
  StSpaListStrip* addListStrip(StSpaListStrip *list);
  void            exchangeTwoStrips(StSpaStrip *ptr1, StSpaStrip *ptr2);
  void            sortStrip();
  int             removeStrip(StSpaStrip *ptr);
  int             getSize();
  void            updateListStrip(StSpaListNoise *ptr);

private:
  int         mListLength;
  StSpaStrip *mFirstS;
  StSpaStrip *mLastS;
};
#endif
