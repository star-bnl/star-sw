/*!
 * \class StPmdAbsClustering
 * \author
 */
/******************************************************
 *
 * $Id: StPmdAbsClustering.h,v 1.5 2004/09/07 23:02:43 fisyak Exp $
 *
 * Author: Subhasis
 ******************************************************
 *
 * Description: Base class for Abstract cluster finder
 *
 ******************************************************
 *
 * $Log: StPmdAbsClustering.h,v $
 * Revision 1.5  2004/09/07 23:02:43  fisyak
 * Add missing default ctors
 *
 * Revision 1.4  2004/06/24 13:47:19  subhasis
 * several changes in clustering code
 *
 * Revision 1.3  2003/05/14 10:49:12  subhasis
 * CPV clustering added
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
  StPmdAbsClustering(StPmdDetector *pmd_det = 0, StPmdDetector* cpv_det=0);
  virtual ~StPmdAbsClustering();
  //  virtual void findPmdClusters() =0;
  //  virtual void findPmdClusters(StPmdDetector*) =0;
  //  virtual void findCpvClusters() =0;
  ClassDef(StPmdAbsClustering, 1) 
    };


#endif


