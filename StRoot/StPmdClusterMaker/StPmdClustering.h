/*!
 * \class StPmdClustering
 * \author
 */
/******************************************************
 *
 * $Id: StPmdClustering.h,v 1.6 2004/07/19 13:23:27 subhasis Exp $
 *
 * Author: Dr. S.C. Phatak
 *         Dipak Mishra
 ******************************************************
 *
 * Description: Base class for PMD clusters
 *
 * $Log: StPmdClustering.h,v $
 * Revision 1.6  2004/07/19 13:23:27  subhasis
 * checks applied on clust_cell dimension
 *
 * Revision 1.5  2004/06/24 13:43:02  subhasis
 * several changes in clustering code
 *
 *
 ******************************************************/
#ifndef STAR_StPmdClustering
#define STAR_StPmdClustering
#include <TH2.h>
#include<math.h>
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


   public:
  // functions for clustering
//!constructor 
  StPmdClustering(StPmdDetector *, StPmdDetector*);
  //!constructor
  StPmdClustering();                             
  //!destructor
  virtual ~StPmdClustering();

  //! for Pmd clusters                     
  //  void findPmdClusters(); // old method
  void findPmdClusters(StPmdDetector *);


 //! crude clustering
  Int_t crclust(Double_t , Double_t, Int_t, Int_t); 
  //! refined clustering
  void refclust(StPmdDetector*,Int_t, Int_t, Int_t,StPmdClusterCollection*);
  
  //! for ordering
  void order(Int_t);  
  //! ordering 
  void arrange(Int_t);
  Double_t Dist(Double_t, Double_t, Double_t, Double_t);  //! distance between two clusters
  
  //! for parametrization : not used any more
  //  void gaussfit(Int_t, Int_t, Double_t &, Double_t &, Double_t &, Double_t &, Double_t &,Double_t &,Double_t & ); 
  //! printing clusters
  void printclust(Int_t,Int_t, StPmdCluster*); 
  //! for Calculating cluster properties, those clusters having more then two cells
  Int_t CentroidCal(Int_t,Int_t,Double_t&,Double_t&,Double_t&,Double_t&,Double_t&,Double_t&,Double_t&,Double_t&,Double_t&);
  

  //! for getting hits of each cluster
  //  StPmdHit* GetHit(StPmdDetector*, Int_t, Int_t, Int_t);
  StPmdHit* GetHit(StPmdDetector*, Int_t, Double_t, Double_t);

  ClassDef(StPmdClustering, 1) 
    };

    
#endif





