// $Id: StScfListCluster.hh,v 1.3 2005/06/13 16:01:00 reinnart Exp $
//
// $Log: StScfListCluster.hh,v $
// Revision 1.3  2005/06/13 16:01:00  reinnart
// Jonathan and Joerg changed the update function
//
// Revision 1.2  2005/05/17 14:16:33  lmartin
// CVS tags added
//
#ifndef STSCFLISTCLUSTER_HH
#define STSCFLISTCLUSTER_HH
#include <stdlib.h>
#include <math.h>
#include "StScfCluster.hh"
//#include "scf_am.h"
#include "tables/St_scf_ctrl_Table.h"

class StScfListStrip;

class StScfListCluster
{
 public:
  StScfListCluster();
  ~StScfListCluster();

  StScfCluster* next(StScfCluster *ptr);
  StScfCluster* prev(StScfCluster *ptr);
  StScfCluster* first();
  StScfCluster* last();  
  int        addNewCluster(StScfCluster *ptr);
  void       exchangeTwoClusters(StScfCluster *ptr1, StScfCluster *ptr2);
  void       sortCluster();
  void       renumCluster();
  int        removeCluster(StScfCluster *ptr);
  int        getSize();
  int        splitCluster(St_scf_ctrl *my_scf_ctrl, StScfCluster *CurrentCluster, int *ListAdc, StScfListStrip *currentListStrip);
  //int        splitCluster(scf_ctrl_st *scf_ctrl, StScfCluster *CurrentCluster, int *ListAdc, StScfListStrip *currentListStrip);
  int        isSorted();
  

private:
  int        mListLength;
  StScfCluster *mFirstS;
  StScfCluster *mLastS;
};
#endif
