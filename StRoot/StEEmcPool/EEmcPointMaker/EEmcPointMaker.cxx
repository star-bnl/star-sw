#include "EEmcPointMaker.h"

#include <iostream>
#include <algorithm>
#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h" 

ClassImp(EEmcPointMaker);


// ----------------------------------------------------------------------------
EEmcPointMaker::EEmcPointMaker( const Char_t *name ) : StMaker(name)
{

}

// ----------------------------------------------------------------------------
Int_t EEmcPointMaker::Init()
{

  mEEmcAnalysisMaker = (EEmcAnalysisMaker *)GetMaker("eemcAnalysisMaker");
  assert(mEEmcAnalysisMaker); 

  mEEmcClusterMaker = (EEmcClusterMaker *)GetMaker("eemcClusterMaker");
  assert(mEEmcClusterMaker);

  mEEmcClusterMaker2 = (EEmcClusterMaker2 *)GetMaker("eemcClusterMaker2");
  assert(mEEmcClusterMaker2);

  mEEmcSmdClusterMaker = (EEmcSmdClusterMaker *)GetMaker("eemcSmdClusterMaker");
  assert(mEEmcSmdClusterMaker);

  return StMaker::Init();

}

// ----------------------------------------------------------------------------
Int_t EEmcPointMaker::Make()
{


  /// Order the SMD clusters
  OrderClusters();

  /// Build all possible SMD points
  while ( 1 ) {
    if ( !BuildSmdPoints() ) break;
  }

  /// Build all SMD + tower points
  BuildPoints();


  /// Build an independent set of points from
  /// EEMC towers only
  BuildTowerPoints();

  //  printPoints();

  return kStOK;

}



// ----------------------------------------------------------------------------
void EEmcPointMaker::Clear( Option_t *opts )
{

  mSmdPoints.clear();
  mMatchedSmdPoints.clear();
  mOrderedClusters.clear();
  mOrderedU.clear();
  mOrderedV.clear();
  mPoints.clear();
  mUcluster2point.clear();
  mVcluster2point.clear();
  mCluster2point.clear();
  mTowerPoints.clear();
}


// ----------------------------------------------------------------------------
void EEmcPointMaker::print( EEezSmdClusterPtrVec_t &oclusters )
{

  EEezSmdClusterPtrVecIter_t oiter = oclusters.begin();
  while ( oiter != oclusters.end() ) {

      EEezSmdCluster *cluster = (*oiter); 
      Int_t plane = cluster -> getPlane(); 

      EEmcSmdPointPtrVec_t points = (plane==0)?
	 mUcluster2point[ cluster ] :
	 mVcluster2point[ cluster ] ; 

      std::cout 
	  << "sec= " << cluster -> getSector() 
	  << " plane=" << cluster -> getPlane()  
	  << " mean=" << cluster->getMean() 
	  << " energy=" << cluster->getEnergy()*1000 
	  << " npoint=" << points.size() << std::endl; 

      EEmcSmdPointPtrVecIter_t piter = points.begin();
      while ( piter != points.end() ) {
	  std::cout 
	      << " + u=" << (*piter)->clusterU()->getMean()
	      << " v=" << (*piter)->clusterV()->getMean();
	  if ( (*piter)->tower() ) std::cout
	    << " tow=" << (*piter)->tower()->getName()
	    << " E=" << (*piter)->tower()->getEnergy(0);
	  else
	      std::cout << " no tower"; 
	  std::cout << std::endl; 
	      
	  piter++;
      } 

      oiter++;
  }

}

// ----------------------------------------------------------------------------
Bool_t EEmcPointMaker::BuildSmdPoints()
{


  /// Build the collision tables
  BuildArrays( mOrderedU, mOrderedV );

//   std::cout << "-----------------------------------------------------------------------------" << std::endl;
//   std::cout << "Building ESMD points, stage 1" << std::endl;
 
  /// We simultaneously loop over all U and V clusters.
  EEezSmdClusterPtrVec_t::iterator uiter = mOrderedU.begin();
  EEezSmdClusterPtrVec_t::iterator viter = mOrderedV.begin();

  ///
  /// Loop over all U and V clusters until we find a cluster
  /// which uniquely matches a single point.  Once we find
  /// such a cluster, we add the point to the list of matched
  /// points, remove the pair of clusters from the list of
  /// available clusters, and return a true value (indicating
  /// that we found a match).
  ///
  while ( uiter != mOrderedU.end() || viter != mOrderedV.end() ) {

    EEezSmdCluster *ucluster = 0;
    EEezSmdCluster *vcluster = 0;

    if ( uiter != mOrderedU.end() ) ucluster = (*uiter);
    if ( viter != mOrderedV.end() ) vcluster = (*viter);

    /// Decide whether we work with the U plane or 
    /// the V plane on this iteration of the loop
    Int_t iuv = 0;
    if ( !vcluster ) 
      iuv = 0;
    else if ( !ucluster )
      iuv = 1;
    else if ( ucluster -> getMean() < vcluster -> getMean() )
      iuv = 0;
    else
      iuv = 1;    





    /// Get the list of all SMD points which match
    /// this cluster.  
    EEmcSmdPointPtrVec_t points = (iuv==0)?
      mUcluster2point[ ucluster ]:
      mVcluster2point[ vcluster ];



    /// If we're dealing with a single possible
    /// reconstruction point, we add this to our
    /// list of matched points and we remove the U
    /// and V clusters from their respective lists.
    /// Finally, we return from this routine.
    if ( points.size() == 1 ) {

      EEezSmdCluster *u = points[0]->clusterU();
      EEezSmdCluster *v = points[0]->clusterV();
      EEmcSmdPoint p(u,v);
      p.setTower( points[0] -> tower() );
      mMatchedSmdPoints.push_back(p);
      KillSmdClusters(u,v);

      return true;

    }
    
    if ( iuv==0 ) uiter++;
    if ( iuv==1 ) viter++;
    
  }


  ///
  /// If we've reached this point, then we failed to find a
  /// unique match between SMD points based only on geometry.
  /// We will now search for SMD clusters which match only
  /// two points.  We decide which of the two points the cluster
  /// belongs to based on which provides the better match in
  /// energy between the two planes.
  ///

  uiter = mOrderedU.begin();
  viter = mOrderedV.begin();

  while ( uiter != mOrderedU.end() || viter != mOrderedV.end() ) {

    EEezSmdCluster *ucluster = 0;
    EEezSmdCluster *vcluster = 0;

    if ( uiter != mOrderedU.end() ) ucluster = (*uiter);
    if ( viter != mOrderedV.end() ) vcluster = (*viter);

    /// Decide whether we work with the U plane or 
    /// the V plane on this iteration of the loop
    Int_t iuv = 0;
    if ( !vcluster ) 
      iuv = 0;
    else if ( !ucluster )
      iuv = 1;
    else if ( ucluster -> getMean() < vcluster -> getMean() )
      iuv = 0;
    else
      iuv = 1;    


    /// Get the list of all SMD points which match
    /// this cluster.  
    EEmcSmdPointPtrVec_t points = (iuv==0)?
      mUcluster2point[ ucluster ]:
      mVcluster2point[ vcluster ];



    /// If we are dealing with a cluster which matches
    /// exactly two points, then we choose between the
    /// two points based on the fractional-logarithmic
    /// differnce in energy between the U and V plane
    /// (that should be clear when you see it below...)
    
    EEmcSmdPointPtrVecIter_t piter1 = points.begin();
    EEmcSmdPointPtrVecIter_t piter2 = piter1+1;
    EEmcSmdPointPtrVecIter_t piter = piter1;

   if ( points.size() == 2 ) {


     ///
     /// The fractional-log energy differences for each point
     ///
     Float_t frac1 = fraction( points[0]->clusterU(), points[0]->clusterV() );
     Float_t frac2 = fraction( points[1]->clusterU(), points[1]->clusterV() );

     ///
     /// Frac-log E-difference for two points
     ///
     Float_t frac3 = 1.;
     if ( iuv==0 ) 
       frac3 = fraction( points[0]->clusterU(), points[0]->clusterV(), 
			 points[1]->clusterV() );
     else
       frac3 = fraction( points[0]->clusterV(), points[0]->clusterU(),
			 points[1]->clusterU() );

     ///
     /// If the SMD cluster matches multiple (e.g. 2) points, then
     /// we set it aside and handle it in the next loop.
     /// 
     
     if ( frac3 < frac1 && frac3 < frac2 ) {
       if (iuv==0) uiter++;
       if (iuv==1) viter++;
       continue;
     }


     if ( frac1 < frac2 ) 
       piter = piter1;
     else
       piter = piter2;

     EEezSmdCluster *u = (*piter)->clusterU();
     EEezSmdCluster *v = (*piter)->clusterV();
     EEmcSmdPoint p(u,v);
     p.setTower( (*piter)->tower() );
     mMatchedSmdPoints.push_back(p);
     
     /// Remove the associated SMD clusters from further consideration
     KillSmdClusters( u,v );
     
      /// And return true;
     return true;
     
   }
   
   if ( iuv==0 ) uiter++;
   if ( iuv==1 ) viter++;
   
  }




  ///
  /// In this third scan through the event, we are looking for
  /// SMD clusters which match 2 SMD points, but it appears that
  /// the cluster has enough energy to match both.
  ///

  uiter = mOrderedU.begin();
  viter = mOrderedV.begin();

  while ( uiter != mOrderedU.end() || viter != mOrderedV.end() ) {

    EEezSmdCluster *ucluster = 0;
    EEezSmdCluster *vcluster = 0;

    if ( uiter != mOrderedU.end() ) ucluster = (*uiter);
    if ( viter != mOrderedV.end() ) vcluster = (*viter);

    /// Decide whether we work with the U plane or 
    /// the V plane on this iteration of the loop
    Int_t iuv = 0;
    if ( !vcluster ) 
      iuv = 0;
    else if ( !ucluster )
      iuv = 1;
    else if ( ucluster -> getMean() < vcluster -> getMean() )
      iuv = 0;
    else
      iuv = 1;    


    /// Get the list of all SMD points which match
    /// this cluster.  
    EEmcSmdPointPtrVec_t points = (iuv==0)?
      mUcluster2point[ ucluster ]:
      mVcluster2point[ vcluster ];
    
    
    
    /// If we are dealing with a cluster which matches
    /// exactly two points, then we choose between the
    /// two points based on the fractional-logarithmic
    /// differnce in energy between the U and V plane
    /// (that should be clear when you see it below...)
    
    EEmcSmdPointPtrVecIter_t piter1 = points.begin();
    EEmcSmdPointPtrVecIter_t piter2 = piter1+1;
    
    if ( points.size() == 2 ) {
      
      
      /// Determine total and individual energy of the
      /// two clusters in the complimentary view, also 
      /// the energy of the SMD cluster being split.
      Float_t ecomp = 0;
      Float_t ecomp1 = 0;
      Float_t ecomp2 = 0;
      Float_t eview  = 0;
      if ( iuv == 0 ) {
	ecomp1 = (*piter1)->clusterV()->getEnergy();
	ecomp2 = (*piter2)->clusterV()->getEnergy();
	eview  = ucluster->getEnergy();
      }
      else {
	ecomp1 = (*piter1)->clusterU()->getEnergy();
	ecomp2 = (*piter2)->clusterU()->getEnergy();
	eview  = vcluster->getEnergy();
      }
      ecomp = ecomp1+ecomp2;


      /// ----> MATCHED <----


      EEezSmdCluster *u1 = (*piter1)->clusterU();
      EEezSmdCluster *v1 = (*piter1)->clusterV();
      EEmcSmdPoint p1(u1,v1);
      p1.setTower( (*piter1)->tower() );
      p1.setEnergy( eview * (ecomp1/ecomp) + ecomp1 );
      mMatchedSmdPoints.push_back(p1);

      EEezSmdCluster *u2 = (*piter2)->clusterU();
      EEezSmdCluster *v2 = (*piter2)->clusterV();
      EEmcSmdPoint p2(u2,v2);
      p2.setTower( (*piter2)->tower() );
      p2.setEnergy( eview * (ecomp2/ecomp) + ecomp2 );
     mMatchedSmdPoints.push_back(p2);
      
      /// Remove the associated SMD clusters from further consideration
      KillSmdClusters( u1, v1 );
      KillSmdClusters( u2, v2 );
      
      /// And return true;
      return true;
     
   }
   
   if ( iuv==0 ) uiter++;
   if ( iuv==1 ) viter++;
   
  }



  /// We didn't find a match, so return false,
  /// terminating the iteration
  return false;


}


// ----------------------------------------------------------------------------
void EEmcPointMaker::OrderClusters()
{

 
  /// Get copies of the SMD clusters from the smd cluster maker.
  /// Keep in mind that we are dealing with a copy of a vector
  /// of **pointers to our only copy** of SMD clusters.  It is 
  /// safe to change the vectors, but not the clusters themeselves.
  EEezSmdClusterPtrVec_t uclusters = mEEmcSmdClusterMaker -> getUClusters();
  EEezSmdClusterPtrVec_t vclusters = mEEmcSmdClusterMaker -> getVClusters();

 
  /// uorder and vorder contain the index of the lowest-lying
  /// smd clusters in ascending order.  In principle, all sectors
  /// are present in uclusters and vclusters. 

  std::vector<UInt_t> uorder;
  std::vector<UInt_t> vorder;
  uorder.push_back(0); 
  vorder.push_back(0); 

  for ( UInt_t i = 1; i < uclusters.size(); i++ ) {

      uorder.push_back(i); 
      Bool_t go = uclusters[i]->getMean()<uclusters[i-1]->getMean();
      UInt_t j = i; 
      while ( go ) {
	  uorder[j] = j-1;
	  uorder[j-1] = j;
	  j--; 
	  if ( j == 0 ) break;
	  go = uclusters[j]->getMean()<uclusters[j-1]->getMean(); 
      }

  } 


  for ( UInt_t i = 1; i < vclusters.size(); i++ ) {

      vorder.push_back(i); 
      Bool_t go = vclusters[i]->getMean()<vclusters[i-1]->getMean();
      UInt_t j = i; 
      while ( go ) {
	  vorder[j] = j-1;
	  vorder[j-1] = j;
	  j--; 
	  if ( j == 0 ) break;
	  go = vclusters[j]->getMean()<vclusters[j-1]->getMean(); 
      }

  } 


  for ( UInt_t i = 0; i < uclusters.size(); i++ ) 
    mOrderedU.push_back( uclusters [ uorder[i] ] );


  for ( UInt_t i = 0; i < vclusters.size(); i++ ) 
    mOrderedV.push_back( vclusters [ vorder[i] ] );
  

  /// Next we create a vector which will hold all SMD clusters
  /// in ascending order of index (i.e. out from the smallest
  /// strips at eta = 2 to the ... small strips at eta = 1...
  /// we will revise this so it goes from shortest to longest
  /// at a later date). 
  //  EEezSmdClusterPtrVec_t mOrderedClusters; 
  //  EEezSmdClusterPtrVec_t::iterator oiter; 

  UInt_t iu = 0;
  UInt_t iv = 0;
  UInt_t nu = uclusters.size();
  UInt_t nv = vclusters.size(); 

  while ( iu+iv < nu+nv ) {
    
    Float_t umean = 999.; 
    Float_t vmean = 999.; 
    if ( iu < nu ) umean = uclusters[ uorder[iu] ] -> getMean();
    if ( iv < nv ) vmean = vclusters[ vorder[iv] ] -> getMean();
    
    if ( umean < vmean ) 
      mOrderedClusters.push_back( uclusters[ uorder[iu++] ] ); 
    else if ( vmean <= umean )   
      mOrderedClusters.push_back( vclusters[ vorder[iv++] ] ); 
    else
      continue; 
    
    
  } 



}

// ----------------------------------------------------------------------------
void EEmcPointMaker::BuildArrays( EEezSmdClusterPtrVec_t uclusters, EEezSmdClusterPtrVec_t vclusters ) 
{


  /// Clear the arrays from the previous iteration
  mSmdPoints.clear();
  mUcluster2point.clear();
  mVcluster2point.clear();

  EEezSmdClusterPtrVec_t::iterator uiter = uclusters.begin();
  EEezSmdClusterPtrVec_t::iterator viter = vclusters.begin();

  /// Loop over all U clusters
  while ( uiter != uclusters.end() ) {
    
    EEezSmdCluster *ucluster = (*uiter);
    
    /// Loop over all V clusters
    viter = vclusters.begin();
    while ( viter != vclusters.end() ) {
      
      EEezSmdCluster *vcluster = (*viter);
      
      /// Skip to the next pair if these are in two separate sectors
      if ( ucluster -> getSector() != vcluster -> getSector() ) {
	viter++;
	continue;
      }
      
      /// Create the new SMD point between these clusters
      EEmcSmdPoint point(ucluster,vcluster);
      
      /// Get the tower this point falls beneath
      EEezTower *tower = mEEmcAnalysisMaker -> getTower( point.position() );
      point.setTower(tower); 
      
      /// If the tower is NULL or wasn't hit, skip this pair
      if ( !tower ) {
	viter++;
	continue;
      }
      if ( tower->getEnergy() == 0. && !tower->fail() ) {
	viter++;
	continue;
      }
      
      /// Add the point to the list of points, and then 
      /// add pointsers to the collision tables
      mSmdPoints.push_back( point );      
      mUcluster2point[ ucluster ].push_back( &mSmdPoints.back() );
      mVcluster2point[ vcluster ].push_back( &mSmdPoints.back() );
      
      viter++;
    }
    
    uiter++;
  } 
  

}

// ----------------------------------------------------------------------------
void EEmcPointMaker::KillSmdClusters ( EEezSmdCluster *u, EEezSmdCluster *v )
{

  ///
  /// Loops through the U and V cluster lists until it finds
  /// the specified clusters, then removes them from the lists
  ///

  EEezSmdClusterPtrVecIter_t uiter = mOrderedU.begin();
  while ( uiter != mOrderedU.end() ) {
    
    EEezSmdCluster *c = (*uiter);   
    if ( c==u ) {
      mOrderedU.erase(uiter);
      break;
    }    
    uiter++;
  }

  EEezSmdClusterPtrVecIter_t viter = mOrderedV.begin();
  while ( viter != mOrderedV.end() ) {
    
    EEezSmdCluster *c = (*viter);   
    if ( c==v ) {
      mOrderedV.erase(viter);
      break;
    }    
    viter++;
  }

}

// ----------------------------------------------------------------------------
void EEmcPointMaker::printPoints()
{

  std::cout << "--------------------------------- printPoints() --" << std::endl;
  std::cout << "Found npoints=" << mMatchedSmdPoints.size()
	    << std::endl;
  EEmcSmdPointVecIter_t iter = mMatchedSmdPoints.begin();
  while ( iter != mMatchedSmdPoints.end() ) {

    std::cout << "tow=" << (*iter).tower()->getName()
	      << " u=" << (*iter).clusterU()->getMean()
	      << " v=" << (*iter).clusterV()->getMean()
	      << std::endl;

    iter++;
  }


  EEmcPointVecIter_t piter = mPoints.begin();
  while ( piter != mPoints.end() ) {
    (*piter).print();
    piter++;
  }

}

// ----------------------------------------------------------------------------
void EEmcPointMaker::printArrays()
{

  std::cout << "--------------------------------- printArrays() --" << std::endl;

  std::cout << "Collisions with remaining U clusters" << std::endl;
  print( mOrderedU );

  std::cout << "Collisions with remaining V clusters" << std::endl;
  print( mOrderedV );

}



// ----------------------------------------------------------------------------
Float_t EEmcPointMaker::fraction( EEezSmdCluster *c1, EEezSmdCluster *c2 )
{

  Float_t e1 = c1->getEnergy();
  Float_t e2 = c2->getEnergy();

  if ( e1==e2 ) return 0.;
  if ( e1+e2 == 0. ) return 1.;

  Float_t diff = TMath::Log(e1)-TMath::Log(e2);
  Float_t sum  = TMath::Log(e1)+TMath::Log(e2);

  return TMath::Abs(diff/sum);

}

Float_t EEmcPointMaker::fraction( EEezSmdCluster *c, EEezSmdCluster *c1, EEezSmdCluster *c2 )
{

  Float_t e = c->getEnergy();
  Float_t e1 = c1->getEnergy();
  Float_t e2 = c2->getEnergy();

  if ( e==e1+e2 ) return 0.;
  if ( e+e1+e2==0. ) return 1.;

  Float_t diff = TMath::Log(e)-TMath::Log(e1)-TMath::Log(e2);
  Float_t sum  = TMath::Log(e)+TMath::Log(e1)+TMath::Log(e2);

  return TMath::Abs(diff/sum);

}




// ----------------------------------------------------------------------------
void EEmcPointMaker::BuildPoints()
{

  /// Get the list of EEmc Clusters
  EEmcClusterVec_t clusters = mEEmcClusterMaker2 -> clusters();

  /// Iterate over list and find matching SMD points
  EEmcClusterVec_t::iterator citer = clusters.begin();
  while ( citer != clusters.end() ) {

    EEmcCluster cluster = (*citer);

    ///
    /// On the first loop, we count the number of matching 
    /// SMD points and sum up their energy deposits
    ///
    EEmcSmdPointVec_t::iterator piter = mMatchedSmdPoints.begin();
    Int_t   npoints = 0;
    Float_t epoints = 0.;
    while ( piter != mMatchedSmdPoints.end() ) {

      EEmcSmdPoint point = (*piter);

      /// Determine if the point matches this cluster
      if ( point.match( &cluster ) ) {
	npoints++;
	epoints+=point.energy();
      }

      piter++;
    }

    ///
    /// On the second pass, we split the energy of the
    /// cluster between the different SMD points and
    /// form EEmcPoints
    ///
    piter = mMatchedSmdPoints.begin();
    if ( epoints > 0. )
      while ( piter != mMatchedSmdPoints.end() ) {

	EEmcSmdPoint point = (*piter);
	
	/// Determine if the point matches this cluster
	if ( point.match( &cluster ) ) {
	  Float_t f = point.energy()/epoints;

	  EEmcPoint p;
	  p.setSector( point.tower()->getSector() );
	  p.setCluster( cluster );
	  p.setSmdPoint( point );
	  p.setFraction( f );
	  p.setEnergy( f * cluster.energy() );
	  p.setPosition( point.position() );

	  mPoints.push_back(p);

	}
	
	
	piter++;
      }
    
    
    citer++;
  }

}

// ----------------------------------------------------------------------------
void EEmcPointMaker::BuildTowerPoints()
{

  /// Get the list of tower-only cluster
  EEezClusterVec_t clusters = mEEmcClusterMaker->getClusters();
  EEezClusterVecIter_t iter = clusters.begin();
  while ( iter != clusters.end() ) {

    mTowerPoints.push_back ( EEmcPoint( &(*iter) ) );

    iter++;
  }

}
// ----------------------------------------------------------------------------
