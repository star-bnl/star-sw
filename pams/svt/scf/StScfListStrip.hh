#ifndef STSCFLISTSTRIP_HH
#define STSCFLISTSTRIP_HH
# include <stdiostream.h>
# include <stdlib.h>
# include <math.h>
# include "StScfStrip.hh"
# include "scf_am.h"



class StScfListStrip
{
 public:
  StScfListStrip();
  ~StScfListStrip();

  StScfStrip* next(StScfStrip *ptr);
  StScfStrip* prev(StScfStrip *ptr);
  StScfStrip* first();
  StScfStrip* last();
  StScfStrip* getStrip(int idStrip);
  int        addNewStrip(StScfStrip *ptr);
  void       exchangeTwoStrips(StScfStrip *ptr1, StScfStrip *ptr2);
  void       sortStrip();
  void       setSigma(int iStrip, int iSigma, sls_ctrl_st *sls_ctrl);
  int        removeStrip(StScfStrip *ptr);
  int        getSize();
  int*       getListAdc(int idStrip, int SizeCluster);
  int        isSorted();
private:
  int        mListLength;
  StScfStrip *mFirstS;
  StScfStrip *mLastS;
};
#endif
