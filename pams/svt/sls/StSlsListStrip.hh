#ifndef STSLSLISTSTRIP_HH
#define STSLSLISTSTRIP_HH
#include "StSlsStrip.hh"

class StSlsListStrip
{
 public:
  StSlsListStrip();
  ~StSlsListStrip();

  StSlsStrip*     next(StSlsStrip *ptr);
  StSlsStrip*     prev(StSlsStrip *ptr);
  StSlsStrip*     first();
  StSlsStrip*     last();
  int             getSize();
  int             addNewStrip(StSlsStrip *ptr);
  int             removeStrip(StSlsStrip *ptr);
  void            exchangeTwoStrips(StSlsStrip *ptr1, StSlsStrip *ptr2);
  void            updateStrip(StSlsStrip *ptr);
  StSlsListStrip* addListStrip(StSlsListStrip *list);
  StSlsListStrip* sortStrip();

private:
  int             mListLength;
  StSlsStrip     *mFirstS;
  StSlsStrip     *mLastS;
};
#endif
