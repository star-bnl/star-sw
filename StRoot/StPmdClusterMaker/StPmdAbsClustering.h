/*!
 * \class StPmdAbsClustering
 * \author
 */
/******************************************************
 *
 * $Id: StPmdAbsClustering.h,v 1.2 2003/05/12 12:12:18 subhasis Exp $
 *
 * Author: Subhasis
 ******************************************************
 *
 * Description: Base class for Abstract cluster finder
 *
 ******************************************************
 *
 * $Log: StPmdAbsClustering.h,v $
 * Revision 1.2  2003/05/12 12:12:18  subhasis
 * StEvent added
 *
 *
 ******************************************************/
#ifndef STAR_StPmdAbsClustering
#define STAR_StPmdAbsClustering
#include <TH2.h>
#include <TH1.h>
#include <TCanvas.h>
#include <TArrayF.h>
#include <TArrayI.h>


class StPmdDetector;
class StPmdClusterCollection;
class StPmdAbsClustering
{

  private:

  protected:
 //! pointer for detector Pmd
  StPmdDetector *m_pmd_det; 
//! pointer for detector Cpv
  StPmdDetector *m_cpv_det;  

   public:
  StPmdAbsClustering(StPmdDetector *, StPmdDetector*);
  StPmdAbsClustering();
  virtual ~StPmdAbsClustering();
  virtual void findPmdClusters() =0;
  virtual void findCpvClusters() =0;
  ClassDef(StPmdAbsClustering, 1) 
    };


#endif


