#ifndef STSPALISTSTRIP_HH
#define STSPALISTSTRIP_HH
# include <stdiostream.h>
# include <stdlib.h>
# include <math.h>
# include "StSpaStrip.hh"
# include "StSpaNoise.hh"
# include "StSpaListNoise.hh"


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
