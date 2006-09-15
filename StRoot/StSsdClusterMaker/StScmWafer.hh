// $Id: StScmWafer.hh,v 1.3 2006/09/15 21:04:50 bouchet Exp $
//
// $Log: StScmWafer.hh,v $
// Revision 1.3  2006/09/15 21:04:50  bouchet
// noise of the strips and clusters coded as a float ; read the noise from ssdStripCalib
//
// Revision 1.2  2005/05/17 14:16:37  lmartin
// CVS tags added
//
#ifndef STSCMWAFER_HH
#define STSCMWAFER_HH
#include <stdlib.h>
#include <math.h>
#include "StScmListCluster.hh"
#include "StScmListPackage.hh"
#include "StScmListPoint.hh"
#include "StScmCluster.hh"
#include "StScmPackage.hh"
#include "StScmPoint.hh"

#include "tables/St_ssdDimensions_Table.h"

class StScmWafer
{
 public:
//                   StScmWafer(int id, int *deadStripP, int *deadStripN);
                  StScmWafer(int id);
                  ~StScmWafer();

  void            init(int rId, Double_t *rD, Double_t *rT, Double_t *rN, Double_t *rX);

  StScmListCluster* getClusterP();
  StScmListCluster* getClusterN();
  StScmListPackage* getPackage();
  StScmListPoint*   getPoint();

  void            addCluster(StScmCluster *ptr, int iSide);
  void            addPackage(StScmPackage *ptr);
  void            addPoint(StScmPoint *ptr);

  void            sortCluster();
  void            sortPoint();

  int             doFindPackage(ssdDimensions_st *geom_par, scm_ctrl_st *scm_ctrl); 
  int             doSolvePerfect(ssdDimensions_st *geom_par, scm_ctrl_st *scm_ctrl);
  void            doStatPerfect(int nPerfectPoint, scm_ctrl_st *scm_ctrl);
  int             doSolvePackage(ssdDimensions_st *geom_par, scm_ctrl_st *scm_ctrl);
  int             convertDigitToAnalog(double pairCreationEnergy);
  int             convertUFrameToLocal(ssdDimensions_st *geom_par);
  int             convertLocalToGlobal();
  int             printborder();
 
 private:
  StScmWafer(const StScmWafer & originalWafer);
  StScmWafer& operator=(const StScmWafer originalWafer);

  int                      mId;
  Double_t                   *mD;
  Double_t                   *mT;
  Double_t                   *mN;
  Double_t                   *mX;
  Double_t                    mPerfectMean;
  Double_t                    mPerfectSigma;
//   int                     *mDeadStripP;
//   int                     *mDeadStripN;
  StScmListCluster        *mClusterP;
  StScmListCluster        *mClusterN;
  StScmListPackage        *mPackage;
  StScmListPoint          *mPoint;

  int             geoMatched(ssdDimensions_st *geom_par, StScmCluster *ptr1, StScmCluster *ptr2);
  int             setMatcheds(ssdDimensions_st *geom_par, StScmPoint *Spt, StScmCluster *pMatched, StScmCluster *nMatched);
  Double_t           matchDistr(scm_ctrl_st *scm_ctrl, Double_t x);
};  
#endif
