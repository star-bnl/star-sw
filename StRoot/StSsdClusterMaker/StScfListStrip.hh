// $Id: StScfListStrip.hh,v 1.3 2006/09/15 21:04:49 bouchet Exp $
//
// $Log: StScfListStrip.hh,v $
// Revision 1.3  2006/09/15 21:04:49  bouchet
// noise of the strips and clusters coded as a float ; read the noise from ssdStripCalib
//
// Revision 1.2  2005/05/17 14:16:34  lmartin
// CVS tags added
//
#ifndef STSCFLISTSTRIP_HH
#define STSCFLISTSTRIP_HH
#include <stdlib.h>
#include <math.h>
#include "StScfStrip.hh"
//# include "scf_am.h"
#include "slsCtrl.h"

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
  void       setSigma(int iStrip, float iSigma, slsCtrl_st *slsCtrl);
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
