/*!
 * \class StPmdClustering
 * \author
 */
/******************************************************
 *
 * $Id: StPmdClustering.h,v 1.1 2002/08/27 12:08:12 subhasis Exp $
 *
 * Author: Dr. S.C. Phatak
 *         Dipak Mishra
 ******************************************************
 *
 * Description: Base class for PMD clusters
 *
 * $Log: StPmdClustering.h,v $
 * Revision 1.1  2002/08/27 12:08:12  subhasis
 * First version
 *
 *
 ******************************************************/
#ifndef STAR_StPmdClustering
#define STAR_StPmdClustering
#include <TH2.h>
#include <TH1.h>
#include <TCanvas.h>
#include <TArrayF.h>
#include <TArrayI.h>
#include "StPmdAbsClustering.h"

class StPmdDetector;
class StPmdClusterCollection;
class StPmdClustering:public StPmdAbsClustering
{

  private:

  protected:
//  StPmdDetector *m_pmd_det;
//  StPmdDetector *m_cpv_det;

   public:
  //! functions for clustering

  StPmdClustering(StPmdDetector *, StPmdDetector*); 
  StPmdClustering();
  virtual ~StPmdClustering();
  void findPmdClusters();    //! for Pmd clusters
  void findCpvClusters();    //! for Cpv clusters
  Int_t crclust(Double_t , Double_t, Int_t, Int_t);  //! crude clustering
  void refclust(Int_t, Int_t, Int_t,StPmdClusterCollection*); //! refined clustering
  Double_t ranmar();  //!random number generator
  void order(Int_t);   //! for ordering
  void arrange(Int_t);  
  Double_t Dist(Double_t, Double_t, Double_t, Double_t);  //! distance between two points
  void gaussfit(Int_t, Int_t, Double_t &, Double_t &, Double_t &, Double_t &, Double_t &,Double_t &,Double_t & );  //! for parametrization
  void printclust(Int_t, Int_t, Int_t,StPmdClusterCollection*);  //! printing clusters

  ClassDef(StPmdClustering, 1) 
    };


#endif





