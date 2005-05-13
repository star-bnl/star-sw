// $Id: StSlsListStrip.hh,v 1.2 2005/05/13 08:39:31 lmartin Exp $
//
// $Log: StSlsListStrip.hh,v $
// Revision 1.2  2005/05/13 08:39:31  lmartin
// CVS tags added
//

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
