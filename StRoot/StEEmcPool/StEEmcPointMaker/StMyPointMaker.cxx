#include "StMyPointMaker.h"

#include "StMessMgr.h"
#include <algorithm>

#include "StEEmcPool/StEEmcClusterMaker/StEEmcCluster.h"
#include "StEEmcPool/StEEmcClusterMaker/StEEmcSmdCluster.h"

ClassImp(StMyPointMaker);

// ---------------------------------------------------------------------
StMyPointMaker::StMyPointMaker(const Char_t *name, StEEmcA2EMaker *a2e, StEEmcGenericClusterMaker *cl):
  StEEmcGenericPointMaker(name,a2e,cl)
{
  mAllowSplitting=false;
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


      // loop over tower clusters and associate the matching SMD clusters
      // into points, based on which pairings minimize a chi^2-like energy
      // difference
      for ( UInt_t ic=0;ic<clusters.size();ic++ )
	{


	  StEEmcCluster cluster=clusters[ic];

	  EEmatch_t matches = mEEclusters->clusterMatch( cluster );

	  // clusters1 is the smaller of the two lists of smd clusters 
	  // matched to this tower cluster
	  StEEmcSmdClusterVec_t clusters1 = 
	    (matches.smdu.size()<matches.smdv.size()) ? matches.smdu : matches.smdv;
	  if ( clusters1.size() == 0 ) continue;

	  // clusters2 is the larger of the two lists of smd clusters
	  // matched to this tower cluster
	  StEEmcSmdClusterVec_t clusters2 = 
	    (matches.smdu.size()<matches.smdv.size()) ? matches.smdv : matches.smdu;
	  if ( clusters2.size() == 0 ) continue;
	  

	  //	  // minimum point energy
	  //	  Float_t energy_minmin = cluster.energy() * 0.014 * 0.10;

	  // find the permutation of the larger list which minimizes
	  // the sum of (Eu-Ev)^2/Nmips
	  Float_t ediff_min = 9.0E9;

	  // vector of clusters holding the best permutation of the larger vector
	  StEEmcSmdClusterVec_t best;

	FIND_POINTS:
	  ediff_min = 9.0E9;

	  //
	  // NOTE-- this is the slowest way possible to do this, O(N!).  
	  //

	  //Int_t count = 0;
	  Bool_t go = true;
	  while ( go )
	    {

	      Float_t ediff = 0;
	      for ( UInt_t ii=0;ii<clusters1.size();ii++ )
		{
		  
		  if ( clusters1[ii].sector() != clusters2[ii].sector() ) 
		    goto INVALID_PERMUTATION;

		  Float_t e1=1000.0*clusters1[ii].energy(); /* energy in MeV */
		  Float_t e2=1000.0*clusters2[ii].energy(); /* energy in MeV */
		  Float_t nmip=(e1+e2)/1.3;

		  if ( nmip <= 0. )
		    goto INVALID_PERMUTATION;

		  ediff += (e1-e2)*(e1-e2)/nmip;

		}

	      if ( ediff < ediff_min )
		{
		  ediff_min = ediff;
		  best = clusters2; /* copy current permutation into best */		  
		}

	      // try the next permutation
	      go = std::next_permutation( clusters2.begin(), clusters2.end() );
	      continue;

	    INVALID_PERMUTATION:
	      // branch to this point if clusters cannot overlap
	      gMessMgr->Debug()<<GetName()<<" -D- cluster pair is not valid" << endm;

	      
	    }// loop over tower clusters
	  if ( best.size() == 0 ) continue;


	  Float_t ediff=0.;
	  for ( UInt_t ii=0;ii<clusters1.size();ii++ )
	    {
	      Float_t e1=1000.0*clusters1[ii].energy(); /* energy in MeV */
	      Float_t e2=1000.0*best[ii].energy(); /* energy in MeV */
	      Float_t nmip=(e1+e2)/1.3;
	      ediff += (e1-e2)*(e1-e2)/nmip;
	    }


	  

	  if ( mAllowSplitting && cluster.momentum().Perp()>6.0 )
	    if ( clusters1.size()==1 &&
		 best.size()==2 )
	      {
		
		Float_t e1=1000.0*clusters1[0].energy();
		Float_t e2=1000.0*(best[0].energy()+best[1].energy());
		Float_t nmip=(e1+e2)/1.3;
		Float_t ediff_split=(e1-e2)*(e1-e2)/nmip;
		
		if ( ediff_split < ediff )
		  {
		    
		    
		    std::sort( best.begin(), best.end() );
		    std::reverse( best.begin(), best.end() );
		    StEEmcSmdCluster tempa, tempb;
		    //		  split ( clusters1[0], clusters1[1], best[0], temp );
		    Float_t chi2_a=9.0E9, chi2_b = 9.0E9;
		    
		    StEEmcSmdCluster mergeda=clusters1[0];
		    StEEmcSmdCluster mergedb=clusters1[0];
		  
		    Bool_t a=split( best[0], best[1], mergeda, tempa, chi2_a );
		    Bool_t b=split( best[1], best[0], mergedb, tempb, chi2_b );
		    //		    clusters1.push_back(temp);
		    if ( a && b ) {
		      if ( chi2_a <= chi2_b ) {
			clusters1.push_back(tempa);
			clusters1[0]=mergeda;
		      }
		      else {
			clusters1.push_back(tempb);
			clusters1[0]=mergedb;
		      }
		    }
		    
		    goto FIND_POINTS;/* ack this is bad */

		  }

	    }
	  

	  // determine which list is U and which is V
	  UInt_t npoints = clusters1.size();
	  StEEmcSmdClusterVec_t uclusters = (clusters1[0].plane()==0)? clusters1:best;
	  StEEmcSmdClusterVec_t vclusters = (clusters1[0].plane()==1)? clusters1:best;


	  for ( UInt_t ipoint=0;ipoint<npoints;ipoint++ )
	    {

	      StEEmcPoint p;

	      p.cluster( cluster, 0 );              /* set tower cluster */

	      StEEmcSmdCluster u=uclusters[ipoint];
	      StEEmcSmdCluster v=vclusters[ipoint];

	      p.cluster( u, 0 );        /* set smdu */
	      p.cluster( v, 1 );        /* set smdv */
	      
	      // SMD energy based on an (approximate) 7% sampling fraction for the SMD
	      Float_t energy = u.energy() + v.energy();
	      p.energy( (Float_t)(energy/0.014 ));

	      // width of the SMD distribution
	      Float_t sigma = u.sigma() + v.sigma();
	      sigma/=2.0;
	      p.sigma( sigma );

	      // set the position of the point
	      TVector3 position=mEEsmd->getIntersection( p.sector(), u.mean(), v.mean() );
	      p.position( position );

	      // FIX FIX FIX FIX FIX FIX FIX FIX FIX FIX FIX FIX FIX FIX FIX FIX FIX FIX FIX 
	      // get the tower the point sits under and skip if it's outside of the endcap
	      // acceptance.  Note that when this happens, the cluster matching algorithm
	      // messed up.  
	      // FIX FIX FIX FIX FIX FIX FIX FIX FIX FIX FIX FIX FIX FIX FIX FIX FIX FIX FIX 
	      StEEmcTower *tower = mEEanalysis->tower( position, 0 );
	      if ( !tower ) continue; 
	      //printf("set tower\n");
	      p.tower( *tower );

	      // add the point to the list of SMD points
	      //printf("add the smd point\n");
	      addSmdPoint( p );

	      // kludge for now 
	      mSmdPoints.back().cluster( cluster, 0 );

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
      addPoint( point );

    }

  

  return StEEmcGenericPointMaker::Make();
}

// ---------------------------------------------------------------------
void StMyPointMaker::Clear(Option_t *opts)
{
  return StEEmcGenericPointMaker::Clear(opts);
}

// ---------------------------------------------------------------------
Bool_t StMyPointMaker::split( StEEmcSmdCluster &in1,  // first resolved cluster in view 1
			      StEEmcSmdCluster &in2,  // second resolved cluster in view 1
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
  my2.key( out1.key() );

  
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
















