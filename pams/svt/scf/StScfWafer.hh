#ifndef STSCFWAFER_HH
#define STSCFWAFER_HH
# include <stdiostream.h>
# include <stdlib.h>
# include <math.h>
# include "StScfListCluster.hh"
# include "StScfListStrip.hh"
# include "StScfCluster.hh"
# include "StScfStrip.hh"
class StScfWafer
{
 public:
                    StScfWafer(int id);
                    ~StScfWafer();

  StScfListCluster* getClusterP();
  StScfListCluster* getClusterN();
  StScfListStrip*   getStripP();
  StScfListStrip*   getStripN();
  void              addStrip(StScfStrip *ptr, int iSide);
  void              setSigmaStrip(int iStrip, int iSide, int iSigma, sls_ctrl_st *sls_ctrl);
  void              sortCluster();
  void              sortStrip();
  void              doClusterisation(int *numberOfCluster, sls_ctrl_st *sls_ctrl, scf_ctrl_st *scf_ctrl);

private:
  int               mId;
  StScfListStrip    *mStripP;
  StScfListStrip    *mStripN;
  StScfListCluster  *mClusterP;
  StScfListCluster  *mClusterN;
  int               doFindCluster(sls_ctrl_st *sls_ctrl, scf_ctrl_st *scf_ctrl, int iSide);
  int               doClusterSplitting(scf_ctrl_st *scf_ctrl, int iSide);
};
  
#endif
