/*!
 * \class StPmdClustering
 * \author
 */
/******************************************************
 *
 * $Id: StPmdClustering.h,v 1.3 2003/05/14 10:49:12 subhasis Exp $
 *
 * Author: Dr. S.C. Phatak
 *         Dipak Mishra
 ******************************************************
 *
 * Description: Base class for PMD clusters
 *
 * $Log: StPmdClustering.h,v $
 * Revision 1.3  2003/05/14 10:49:12  subhasis
 * CPV clustering added
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
class StPmdHit;
class StPmdCluster;
class StPmdDetector;
class StPmdClusterCollection;
class StPmdClustering:public StPmdAbsClustering
{

  private:

  protected:
//  StPmdDetector *m_pmd_det;
//  StPmdDetector *m_cpv_det;

   public:
  // functions for clustering
//!constructor 
  StPmdClustering(StPmdDetector *, StPmdDetector*);
  //!constructor
  StPmdClustering();                             
  //!destructor
  virtual ~StPmdClustering();
  //! for Pmd clusters                     
  void findPmdClusters();
  //! for Cpv clusters
  void findCpvClusters(); 

 //! crude clustering
  Int_t crclust(Double_t , Double_t, Int_t, Int_t); 
  //! refined clustering
  void refclust(StPmdDetector*,Int_t, Int_t, Int_t,StPmdClusterCollection*);
  //!random number generator
  Double_t ranmar(); 
  //! for ordering
  void order(Int_t);  
  //! ordering 
  void arrange(Int_t);
  Double_t Dist(Double_t, Double_t, Double_t, Double_t);  //! distance between two clusters
  //! for parametrization
  void gaussfit(Int_t, Int_t, Double_t &, Double_t &, Double_t &, Double_t &, Double_t &,Double_t &,Double_t & ); 
  //  void printclust(Int_t, Int_t, Int_t,StPmdClusterCollection*,StPmdCluster*); 
  //! printing clusters
  void printclust(Int_t,Int_t, StPmdCluster*); 
  //! for getting hits of each cluster
  //  StPmdHit* GetHit(StPmdDetector*, Int_t, Int_t, Int_t);
  StPmdHit* GetHit(StPmdDetector*, Int_t, Double_t, Double_t);

  ClassDef(StPmdClustering, 1) 
    };


#endif





