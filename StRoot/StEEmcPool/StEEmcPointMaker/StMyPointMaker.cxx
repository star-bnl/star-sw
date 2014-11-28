#include "StMyPointMaker.h"

#include "StMessMgr.h"
#include <algorithm>

#include "StEEmcPool/StEEmcClusterMaker/StEEmcCluster.h"
#include "StEEmcPool/StEEmcClusterMaker/StEEmcSmdCluster.h"

ClassImp(StMyPointMaker);

// ---------------------------------------------------------------------
StMyPointMaker::StMyPointMaker(const Char_t *name, const StEEmcA2EMaker *a2e, const StEEmcGenericClusterMaker *cl):
  StEEmcGenericPointMaker(name,a2e,cl)
{
  mAllowSplitting=false;
  mSplitMinimumET=0.0;
  setSmdMinFraction(0.0);
  mMaxClusterId = 0;
}

// ---------------------------------------------------------------------
Int_t StMyPointMaker::Init()
{

  return StEEmcGenericPointMaker::Init();
}

// ---------------------------------------------------------------------
Int_t StMyPointMaker::Make()
{

  gMessMgr->Info() << GetName() << " -I- building points " << endm;

  typedef StEEmcGenericClusterMaker::EEmatch EEmatch_t;

  if (mEEclusters) mMaxClusterId = mEEclusters->maxClusterId();

  ////////////////////////////////////////////////////////////////////
  //
  // loop over all sectors and determine the best set of points 
  // beneath clusters
  //
  ////////////////////////////////////////////////////////////////////
  for ( Int_t sector=0;sector<12;sector++ )
    {

      // get the vector of tower clusters, sorted descending in energy
      StEEmcClusterVec_t clusters = mEEclusters->clusters(sector, 0);
      std::sort( clusters.begin(), clusters.end() );
      std::reverse( clusters.begin(), clusters.end() );

      LOG_INFO << GetName() << " sector "<<sector+1
	       <<" nclusters="<<clusters.size()
	       <<" nu="<<mEEclusters->numberOfClusters(sector,4)
	       <<" nv=" <<mEEclusters->numberOfClusters(sector,5)
	       << endm;


      // loop over tower clusters and associate the matching SMD clusters
      // into points, based on which pairings minimize a chi^2-like energy
      // difference
      for ( UInt_t ic=0;ic<clusters.size();ic++ )
	{

	  StEEmcCluster cluster=clusters[ic];
	  LOG_DEBUG<<GetName()<<" tower cluster at "<<cluster.tower(0).name()<<endm;

	  EEmatch_t matches = mEEclusters->clusterMatch( cluster );

	  // clusters1 is the smaller of the two lists of smd clusters 
	  // matched to this tower cluster
	  StEEmcSmdClusterVec_t clusters1 = 
	    (matches.smdu.size()<matches.smdv.size()) ? matches.smdu : matches.smdv;

	  // clusters2 is the larger of the two lists of smd clusters
	  // matched to this tower cluster
	  StEEmcSmdClusterVec_t clusters2 = 
	    (matches.smdu.size()<matches.smdv.size()) ? matches.smdv : matches.smdu;

	  if ( clusters1.size() == 0 ) continue;
	  if ( clusters2.size() == 0 ) continue;

	  /*
	   * Test all permutations of the larger list of SMD clusters against
	   * the smaller list of clusters to find the best matches (minimize
	   * a chi^2-like function).
	   */
	  AssociateClusters( clusters1, clusters2 );

	  /*
	   * Attempt to split clusters and, if successful, rerun assocation
	   */
	  if ( mAllowSplitting && cluster.momentum().Perp() > mSplitMinimumET )
	    {
	      if ( SplitClusters( clusters1, clusters2 ) )
		{
		  AssociateClusters( clusters1, clusters2 );
		}
	    }
	
	    


	  StEEmcSmdClusterVec_t uclusters = ( clusters1[0].plane()==0 ) ? clusters1 : clusters2 ;
	  StEEmcSmdClusterVec_t vclusters = ( clusters1[0].plane()==0 ) ? clusters2 : clusters1 ;

	  // we will form this many points associated with this tower cluster
	  UInt_t npoints = clusters1.size();
	  for ( UInt_t ipoint = 0; ipoint < npoints; ipoint++ )
	    {

	      StEEmcPoint p;
	      p.cluster(cluster, 0 ); /* set tower cluster */

	      StEEmcSmdCluster u=uclusters[ipoint];
	      StEEmcSmdCluster v=vclusters[ipoint];
	      p.cluster( u, 0 );
	      p.cluster( v, 1 );
	      
	      Float_t energy = ( u.energy() + v.energy() ) / 0.014; /* initial estimate assumes 1.4% s.f. */
	      p.energy( energy );

	      Float_t sigma  = ( u.sigma() + v.sigma() ) / 2.0;
	      p.sigma( sigma );

	      TVector3 position = mEEsmd->getIntersection( u.sector(), u.mean(), v.mean() );
	      if ( position.Z() < 0. )
		{
		  LOG_WARN<<GetName()<<" attempt to add point with invalid intersection: "<<Form(" X=%5.1f Y=%5.1f sec=%i %i u=%5.1f v=%5.1f", 
					    position.X(),position.Y(),u.sector(),v.sector(),u.mean(),v.mean())<<endm;
		  continue;
		}
	      p.position(position);

	      /*
	       * Obtain a pointer to the tower which this point sits under, and copy
	       * the tower data structure into the point.  In the unlikely event that
	       * the tower is out of the acceptance of the endcap, then there is some-
	       * thing wrong with the smd geometry class or the point maker.
	       */
	      const StEEmcTower *tower = mEEanalysis->tower( position, 0 );
	      if ( !tower )
		{
		  LOG_WARN<<GetName()<<" attempt to add point which misses the EEMC towers: "<<Form(" X=%5.1f Y=%5.1f sec=%i %i u=%5.1f v=%5.1f", 
					    position.X(),position.Y(),u.sector(),v.sector(),u.mean(),v.mean())<<endm;
		  continue;
		}
	      p.tower(*tower);

	      /*
	       * If we've reached this point then, to get to the point of the matter
	       * (sorry...)
	       *
	       * Add the point to the list of SMD points
	       *
	       */
	      addSmdPoint( p );

	      /*
	       * Something is screwy here, and we lose the tower cluster when addSmdPoint
	       * is called.  Maybe a problem in the point's copy constructor?  Kludge it
	       * for now.
	       */
	      //mSmdPoints.back().cluster( cluster, 0 );
               
	    }
	  
	  
	 	    
	}// loop over tower clusters



    }// loop over sectors
  



  //
  // Now divide tower energy between all smd points.  Start by incrementing a weight
  // for each endcap tower by the smd energy of the point, and increment weights in
  // all neighboring towers as well.
  //
  Float_t sumw[720]; for ( Int_t ii=0;ii<720;ii++ ) sumw[ii]=0.;
  for ( Int_t ipoint=0;ipoint<numberOfSmdPoints();ipoint++ )
    {

      StEEmcPoint point = smdPoint(ipoint);
      StEEmcTower tower = point.tower(0);
      sumw[ tower.index() ] += point.energy();

      for ( Int_t jj=0;jj<tower.numberOfNeighbors();jj++ )
	{
	  sumw[tower.neighbor(jj).index()] += point.energy();
	}

    }


  for ( Int_t ipoint=0;ipoint<numberOfSmdPoints();ipoint++ )
    {

      StEEmcPoint point = smdPoint(ipoint);
      StEEmcTower tower = point.tower(0);
      Float_t epoint = 0.;
      Float_t w = sumw[tower.index()];
      if ( !tower.fail() && w>0. ) epoint+=tower.energy() * point.energy()/w;
      for ( Int_t jj=0;jj<tower.numberOfNeighbors();jj++ )
	{	  
	  StEEmcTower neighbor=tower.neighbor(jj);
	  w = sumw[ neighbor.index() ];
	  if ( !neighbor.fail() && w>0. ) epoint+=neighbor.energy() * point.energy()/w;

	}

      point.energy(epoint);


      // for now, associate energy of preshower element including point w/ the 
      // point.
      Int_t index = tower.index();
      Float_t epre1 = mEEanalysis->tower(index,1).energy(); /* energy of pre1 */
      Float_t epre2 = mEEanalysis->tower(index,2).energy(); /* energy of pre2 */
      Float_t epost = mEEanalysis->tower(index,3).energy(); /* energy of post */

      point.energy(epre1,1);
      point.energy(epre2,2);
      point.energy(epost,3);

      addPoint( point );
      
    }

  return StEEmcGenericPointMaker::Make();
}

// ----------------------------------------------------------------------------
Bool_t StMyPointMaker::AssociateClusters( const StEEmcSmdClusterVec_t &list1, StEEmcSmdClusterVec_t &list2 )
{

  /*
   * Sanity checks
   */
  if ( list1.size() == 1 && list2.size()==1 )
    return true;

  if ( list1.size() > list2.size() )
    assert(2+2==5); // list1 should be smaller than list2

  if ( list2.size() == 0 )
    return false;

  LOG_DEBUG<<GetName()<<" associating smd clusters"<<endm;


  /*
   * Index1 and index2 specify the order we take clusters from the list
   */
  std::vector<UInt_t> index1;
  std::vector<UInt_t> index2;
  for ( UInt_t i=0;i<list1.size();i++ ){ index1.push_back(i); }
  for ( UInt_t i=0;i<list2.size();i++ ){ index2.push_back(i); }

  std::vector<UInt_t> best1;
  std::vector<UInt_t> best2;

  /*
   * Loop while Index2 has a permutation
   */
  Bool_t go = true;
  Float_t ediff_min = 9.0E9;

  Int_t count=0;
  while ( go ) 
    {

      LOG_DEBUG<<GetName()<<" permutation = " << count++ << " ediff_min="<< ediff_min<< endm;
      
      // calculate energy chi^2 sum for the current order of 
      // list 2 relative to list 1
      Float_t ediff_sum = 0.0;
      for ( UInt_t i=0;i<list1.size();i++ )
	{

	  // consider this pairing of clusters
	  UInt_t i1 = index1[i];
	  UInt_t i2 = index2[i];
	  StEEmcSmdCluster cluster1 = list1[i1];
	  StEEmcSmdCluster cluster2 = list2[i2];


	  if ( cluster1.sector() != cluster2.sector() ) goto INVALID_PERM;
	  if ( cluster1.plane () == cluster2.plane () ) goto INVALID_PERM;

	  Float_t mean1 = cluster1.mean();
	  Float_t mean2 = cluster2.mean();

	  TVector3 hit = mEEsmd->getIntersection( cluster1.sector(), mean1, mean2 );
	  if ( hit.Z() < -1.0 ) goto INVALID_PERM;

	  Float_t echi2 = energyChi2( cluster1, cluster2 );
	  if ( echi2 < 0. ) goto INVALID_PERM;

	  ediff_sum += echi2;
	  
	}

      
      if ( ediff_sum < ediff_min )
	{
	  best1 = index1;
	  best2 = index2;
	  ediff_min = ediff_sum;
	}
      

    INVALID_PERM:
      go = std::next_permutation( index2.begin(), index2.end() );

    }


  // if we find a valid permuation, overwrite list2 with new
  // ordering and return true
  if ( best1.size() == index1.size() )
    {

      StEEmcSmdClusterVec_t temp2;

      for ( UInt_t i=0;i<best2.size();i++ )
	{

	  temp2.push_back( list2[ best2[i] ] );

	}

      LOG_DEBUG<<GetName()<<Form(" ukey\tvkey\t\tvkey")<<endm;
      LOG_DEBUG<<GetName()<<Form("     \t(initial)\t(final)")<<endm;
      for ( UInt_t i=0;i<best1.size();i++ )
	{
	  LOG_DEBUG<<GetName()<<Form(" %i\t%i\t\t%i", list1[i].key(), list2[i].key(), temp2[i].key() )<<endm;
	}

      list2.clear();
      list2=temp2;
      return true;

    }

  /*
   * If we reached this point, no valid points were found
   */

  return false;

}

// -----------------------------------------------------------------------------
Bool_t StMyPointMaker::SplitClusters( StEEmcSmdClusterVec_t &list1,
				      const StEEmcSmdClusterVec_t &list2 )
{


  //
  // Only split clusters if we have two clusters in list2
  //
  if ( list2.size() != 2 ) return false;

  //
  // Likewise, only split if we have one cluster in list 1
  // 
  if ( list1.size() != 1 ) return false;

  
  //
  // energy difference for 1 + 2 case
  //
  Float_t ediff12 = energyChi2( list1[0], list2[0] );

  //
  // energy difference for 2 + 2 case
  //
  Float_t ediff22 = energyChi2( list1[0], list2[0], list2[1] );

  //
  // if "chi2" is better for the 2 + 2 case, then split list1[0]  
  //
  if ( ediff12 <= ediff22 ) return false;

  StEEmcSmdCluster tempa, tempb;
  Float_t chi2_a=9.0E9, chi2_b = 9.0E9;

  StEEmcSmdCluster mergeda=list1[0];
  StEEmcSmdCluster mergedb=list1[0];

  Bool_t a = split( list2[0], list2[1], mergeda, tempa, chi2_a );
  Bool_t b = split( list2[1], list2[0], mergedb, tempb, chi2_b );
  
  // case where either order is found
  if ( a && b ) 
    {
      if ( chi2_a <= chi2_b ) {
	tempa.key( mMaxClusterId++ );
	mergeda.key( mMaxClusterId++ );
	list1[0]=mergeda; 
	list1.push_back( tempa );
	return true;
      }
      else {
	tempb.key( mMaxClusterId++ );
	mergedb.key( mMaxClusterId++ );
	list1[0]=mergedb; 
	list1.push_back( tempb );
	return true;
      }

    }
  // only first orientation valid
  else if ( a )
    {
	tempa.key( mMaxClusterId++ );
	mergeda.key( mMaxClusterId++ );
	list1[0]=mergeda; 
	list1.push_back( tempa );
	return true;
    }
  // only second orientation valid
  else if ( b )
    {
	tempb.key( mMaxClusterId++ );
	mergedb.key( mMaxClusterId++ );
	list1[0]=mergedb; 
	list1.push_back( tempb );
	return true;
    }
  
  

  return true;
}


// ---------------------------------------------------------------------
Float_t StMyPointMaker::energyChi2( const StEEmcSmdCluster &c1, const StEEmcSmdCluster &c2 ) const
{
  Float_t e1 = c1.energy() * 1000.0;
  Float_t e2 = c2.energy() * 1000.0;
  Float_t esum = e1+e2;
  Float_t edif = e1-e2;
  Float_t nmip = esum/1.3;
  if ( esum <= 0. ) return -1.;
  return edif*edif/nmip;
}
Float_t StMyPointMaker::energyChi2( const StEEmcSmdCluster &c1, const StEEmcSmdCluster &c2, const StEEmcSmdCluster &c3 ) const
{
  Float_t e1 = c1.energy() * 1000.0;
  Float_t e2 = c2.energy() * 1000.0;
  Float_t e3 = c3.energy() * 1000.0;
  Float_t esum = e1+e2+e3;
  Float_t edif = e1-e2-e3;
  Float_t nmip = esum/1.3;
  if ( esum <= 0. ) return -1.;
  return edif*edif/nmip;
}


// ---------------------------------------------------------------------
void StMyPointMaker::Clear(Option_t *opts)
{
  return StEEmcGenericPointMaker::Clear(opts);
}

// ---------------------------------------------------------------------
Bool_t StMyPointMaker::split( const StEEmcSmdCluster &in1,  // first resolved cluster in view 1
			      const StEEmcSmdCluster &in2,  // second resolved cluster in view 1
			      StEEmcSmdCluster &out1, // merged cluster in view 2, will output first split cluster
			      StEEmcSmdCluster &out2, // for output of second split cluster
			      Float_t &chi2out
			      )
{

  /// first some sanity checks.
  if ( in1.sector() != in2.sector() ) return false;
  if ( in1.plane()  != in2.plane()  ) return false;
  if ( in1.sector() != out1.sector() ) return false;
  if ( in1.plane()  == out1.plane()  ) return false;
  
  /// get a pointer to the smd geometry class
  //EEmcSmdGeom *geom = EEmcSmdGeom::instance();

  //Int_t nstrips_in  = (Int_t)geom -> getEEmcSector( in1.plane(), in1.sector() ).stripPtrVec.size();
  //Int_t nstrips_out = (Int_t)geom -> getEEmcSector( out1.plane(), out1.sector() ).stripPtrVec.size();

  /// determine size of the cluster we will attempt to split
  Int_t size = out1.size();

  /// we will loop over strips +/- size/2 around the seed strip
  StEEmcStrip seed = out1.seed();
  Int_t id_min = seed.index() - size/2;
  Int_t id_max = seed.index() + size/2;


  ///
  /// We will be doing a chi^2 minimization using the two clusters in the resolved
  /// plane as the fitting function.  We line up the seed strip of the more energetic
  /// cluster with the cluster we are trying to split.  Then we scan the lower-energy
  /// cluster across to find the id where we minimize
  ///
  /// sum_i (Eu_i - E`u_i)^2/Nmip where Eu_i and E`u_i are the measured and projected
  /// energies for each strip in the u plane assuming the v plane is resolved...
  ///
  /// E`u_i = Ev_j + Ev_k where j and k ...
  ///

  /// This is the index in the merged plane where we place the more energetic cluster
  Int_t id_first = seed.index();

  /// We will search for the index where the second cluster goes  
  Int_t   id_second = -1;
  Float_t min_chi2 = 9.0E9;

  Float_t weight1[288]; for ( Int_t ii=0;ii<288;ii++ ) weight1[ii]=0.;
  Float_t weight2[288]; for ( Int_t ii=0;ii<288;ii++ ) weight2[ii]=0.;

  /// Scan location of second cluster
  for ( Int_t id_scan=id_min; id_scan<=id_max; id_scan++ )
    {

      /// Map energy of both clusters in the opposing view into view of the merged cluster
      Float_t energy[288]; for ( Int_t ii=0;ii<288;ii++ ) energy[ii]=0.;

      /// Map first cluster
      Int_t shift = seed.index() - in1.seed().index();
      for ( Int_t ii=0;ii<in1.size();ii++ )
	{
	  StEEmcStrip strip = in1.strip(ii);
	  Int_t       index = strip.index() + shift;
	  if ( index < 0 || index > 287 ) continue;
	  energy[index] = strip.energy();
	}

      /// Map second cluster
      shift = id_scan - in2.seed().index();
      for ( Int_t ii=0;ii<in2.size();ii++ )
	{
	  StEEmcStrip strip = in2.strip(ii);
	  Int_t       index = strip.index() + shift;
	  if ( index < 0 || index > 287 ) continue;
	  energy[index] += strip.energy();
	}      


      /// Now compute chi^2 
      Float_t chi2 = 0.0;
      for ( Int_t ii=0;ii<size;ii++ )
	{

	  StEEmcStrip strip = out1.strip(ii);
	  Int_t jj=strip.index();
	  Float_t ediff = 1000.0 * ( strip.energy() - energy[jj] );
	  Float_t esum  = 1000.0 * ( strip.energy() + energy[jj] );
	  Float_t nmip  = esum / 1.3;

	  if ( nmip > 0. ) chi2+=ediff*ediff/nmip;

	}

      if ( chi2 < min_chi2 ) 
	{

	  min_chi2 = chi2;
	  chi2out = chi2;
	  id_second = id_scan;

	}


    }// scan 2nd resolved cluster across the merged cluster

  
  if ( id_second < 0 ) return false; /* failed to find minimum */


  /// We will be returning these clusters as output
  StEEmcSmdCluster my1, my2;

  /// Set keys equal to parent key so that we can tell that they have been 
  /// split from a cluster later in the chain
  my1.key( out1.key() );
  //my2.key( out1.key() );
  my2.key( mMaxClusterId++ );

  
  /// To keep with convention, we need to add the seed strip first
  StEEmcStrip seed1 = mEEanalysis->strip( out1.sector(), out1.plane(), id_first );
  StEEmcStrip seed2 = mEEanalysis->strip( out1.sector(), out1.plane(), id_second );


  /// Determine weights with which we add the strips to each cluster.  Weights will be E1(2)/E1+E2
  /// for the cluster in the opposing smd plane
  Float_t energy1[288], energy2[288];
  for ( Int_t ii=0;ii<288;ii++ ) { energy1[ii]=0.; energy2[ii]=0.; }

  Int_t shift = id_first - in1.seed().index();
  for ( Int_t ii=0;ii<in1.size();ii++ )
    {
      StEEmcStrip strip=in1.strip(ii);
      Int_t index = strip.index() + shift;
      if ( !strip.fail() )
	energy1[index] = strip.energy();
    }

  shift = id_second - in2.seed().index();
  for ( Int_t ii=0;ii<in2.size();ii++ )
    {
      StEEmcStrip strip=in2.strip(ii);
      Int_t index = strip.index()+shift;
      if ( !strip.fail() )
	energy2[index] = strip.energy();
    }


  /// add the seed strips first
  {
    Float_t e1 = energy1[ seed1.index() ];
    Float_t e2 = energy2[ seed1.index() ];
    if ( e1==0. ) e1=0.5*( energy1[seed1.index()-1] + energy1[seed1.index()+1] );
    if ( e2==0. ) e2=0.5*( energy2[seed1.index()-1] + energy2[seed1.index()+1] );
    Float_t sumw = e1+e2;
    my1.add( seed1, e1/sumw );
  }
  {
    Float_t e1 = energy1[ seed2.index() ];
    Float_t e2 = energy2[ seed2.index() ];
    if ( e1==0. ) e1=0.5*( energy1[seed2.index()-1] + energy1[seed2.index()+1] );
    if ( e2==0. ) e2=0.5*( energy2[seed2.index()-1] + energy2[seed2.index()+1] );
    Float_t sumw = e1+e2;
    my2.add( seed2, e2/sumw );
  }

  for ( Int_t ii=0;ii<out1.size();ii++ )
    {

      StEEmcStrip strip = out1.strip(ii);

      Float_t e1 = energy1[ strip.index() ];
      Float_t e2 = energy2[ strip.index() ];
      if ( e1==0. ) e1=0.5*( energy1[strip.index()-1] + energy1[strip.index()+1] );
      if ( e2==0. ) e2=0.5*( energy2[strip.index()-1] + energy2[strip.index()+1] );

      Float_t sumw = e1+e2;
      if ( sumw==0. ) continue; 

      if ( strip.index() != seed1.index() ) my1.add( strip, e1/sumw );
      if ( strip.index() != seed2.index() ) my2.add( strip, e2/sumw );

    }


  out1=my1;
  out2=my2;

  return true;

}
