#include "StEEmcPointMaker.h" 
#include "StEEmcA2EMaker.h"
#include "StEEmcClusterMaker.h"

#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcUtil/StEEmcSmd/EEmcSmdGeom.h" 

#include <iostream> 
#include <algorithm>
#include <map>

#include "eeTowerFunction.h"

/* StEvent stuff */
#include "StEvent/StEvent.h"
#include "StEvent/StEmcCollection.h"
#include "StEvent/StEmcPoint.h"

/* Root's linear algebra package */
#include "TMatrixF.h"

//#define DEBUG_buildPoints

ClassImp(StEEmcPointMaker);

// ----------------------------------------------------------------------------
StEEmcPointMaker::StEEmcPointMaker(const Char_t *name):StMaker(name)
{
  std::cout << "StEEmcPointMaker("<<name<<")" << std::endl;

    ///
    /// Initialize the geometry packages
    ///
    mEEtow=new EEmcGeomSimple();
    mEEsmd=EEmcSmdGeom::instance();
    mEEmap=EEmcSmdMap::instance(); 

    mTowerThreshold=0.;
    mFillStEvent=false;
    mEnergyMode=1;
    mLimit=10;

}

// ----------------------------------------------------------------------------
Int_t StEEmcPointMaker::Init()
{
    mEEanalysis=(StEEmcA2EMaker*)GetMaker(mNameAnalysis);
    mEEclusters=(StEEmcClusterMaker*)GetMaker(mNameClusters);
    assert(mEEanalysis);
    assert(mEEclusters);
    return StMaker::Init();
}

// ----------------------------------------------------------------------------
Int_t StEEmcPointMaker::Make()
{
 
  ///
  /// First phase of the game -- build smd points.  All pairs 
  /// of U & V clusters which cross beneath a struck (or dead)
  /// tower are added to the list of smd points.  We also keep
  /// tracks of the associations between the 
  /// 
  
  // Loop over all 12 EEMC sectors
  for ( Int_t sector=0; sector<12; sector++ )
    {

      /// Get smd clusters
      StEEmcSmdClusterVec_t uclusters=mEEclusters->smdclusters(sector,0);
      StEEmcSmdClusterVec_t vclusters=mEEclusters->smdclusters(sector,1);

      /// Sort from closest to the beam to furthest
      std::sort( uclusters.begin(), uclusters.end(), inner );
      std::sort( vclusters.begin(), vclusters.end(), inner );

      findPoints( sector, uclusters, vclusters, mPoints );
      
    }


  ///
  /// Point energies were initially set to be the energy 
  /// deposited in the SMD.  We will now overwrite these
  /// energies, calculating the equivalent EM energy using
  /// a sampling fraction of 0.7% per smd plane
  ///  
  for ( UInt_t i=0;i<mPoints.size();i++ )
    {
      mPoints[i].energy( mPoints[i].energy()/0.007/2. );
    }




  StEEmcPointVec_t orgpoints=mPoints;

  /// do energy sharing
  //  shareEnergy(); //  <<<<<<< leads to negative point energies... rethink

  if ( mEnergyMode == 1 )
    shareEnergySimple();
  else
    shareEnergySmd();


  /// count the number of "related" points
  countRelatives();
  

  if ( mFillStEvent )
    {
      fillStEvent();
      verifyStEvent();
    }

    
  return kStOK;
}

// ----------------------------------------------------------------------------

StEEmcPointVec_t StEEmcPointMaker::buildSmdPoints( Int_t sector, 
						   StEEmcSmdClusterVec_t &u, 
						   StEEmcSmdClusterVec_t &v )
{

  StEEmcPointVec_t points;

  for ( UInt_t i=0; i<u.size(); i++ ) 
    {

      StEEmcSmdCluster uc=u[i];
      Float_t xu=uc.mean();

      for ( UInt_t j=0;j<v.size(); j++ ) 
	{


	  StEEmcSmdCluster vc=v[j];
	  Float_t xv=vc.mean();

	  /// determine which tower this smd point is under
	  TVector3 direct = mEEsmd->getIntersection( sector, xu, xv );
	  
	  Int_t sec,sub,eta;
	  if ( !mEEtow->getTower(direct,sec,sub,eta) )
	    {
	      continue; /// invalid intersection
	    }
	  else
	    {
	      /// happy intersection
	    }

	  /// well, maybe and maybe not.  make sure that the
	  /// candidate point lies in the same sector
	  if ( sector != sec )
	    continue;



	  /// to form a valid smd point, we require a struck 
	  /// tower, or a "fail" bit to be set and one of the
	  /// other tower detectors to fire (pre/postshower).
	  Bool_t good = false;
	  if ( mEEanalysis->tower(sec,sub,eta).energy() > 0. ) 
	    good=true;
	  else if ( mEEanalysis->tower(sec,sub,eta).fail() ) 
	    {
	      for ( Int_t layer=1;layer<=3;layer++ ) 
		{
		  if ( mEEanalysis->tower(sec,sub,eta,layer).energy() > 0. )
		    good=true;		
		}
	    }
	  
	  if ( good ) {

	    StEEmcPoint p;
	    p.cluster( uc, 0 );
	    p.cluster( vc, 1 );
	    p.energy( uc.energy() + vc.energy() );
	    p.tower( mEEanalysis->tower(sec,sub,eta) );
	    TVector3 position=mEEsmd->getIntersection(sector,uc.mean(),vc.mean());
	    p.position(position);
	    points.push_back(p);

	  }
	         	
	}

    }
  
  return points;
}

















// ----------------------------------------------------------------------------
Bool_t StEEmcPointMaker::findPoints( Int_t sector, 
				     StEEmcSmdClusterVec_t uclusters, 
				     StEEmcSmdClusterVec_t vclusters,
				     StEEmcPointVec_t &points )
{


  /// Temp vector to store candidate points
  StEEmcPointVec_t mypoints;

  /// Sort from closest to the beam to furthest
  std::sort( uclusters.begin(), uclusters.end(), inner );
  std::sort( vclusters.begin(), vclusters.end(), inner );

  /// build smd points
  StEEmcPointVec_t smdpoints = buildSmdPoints( sector, uclusters, vclusters );

  
  /// nothing left to do here
  if ( smdpoints.size() < 1 ) return false;
  
  /// presort smd points by chi^2.
  std::sort( smdpoints.begin(), smdpoints.end(), chiSquare );
  

  /// create associative arrays matching smd clusters to points,
  /// or in this case index into the smdpoints array.  NOTE-- from
  /// here on, it is important that smdpoints do not get sorted.
  std::map< Int_t, std::vector<Int_t> > u2p, v2p;            
  for ( UInt_t i=0; i<smdpoints.size(); i++ ) 
    {
      u2p[ smdpoints[i].cluster(0).key() ].push_back( i );
      v2p[ smdpoints[i].cluster(1).key() ].push_back( i );	  
    }

  /// look for smd clusters which match a single smd point.
  /// where we find such unique matches, we add the point to
  /// the list of found points, and remove the matching pair
  /// of smd clusters from the list of clusters.  
  Bool_t go = false;
  StEEmcSmdClusterVec_t::iterator uiter=uclusters.begin();
  StEEmcSmdClusterVec_t::iterator viter=vclusters.begin();


  
  //    ----------<<<<<<<<< stage one >>>>>>>>>-------------



  while ( uiter<uclusters.end() || viter<vclusters.end() )
    {

      /// Get clusters and determine which plane we're working with
      StEEmcSmdCluster ucl;ucl.key(-1);
      StEEmcSmdCluster vcl;vcl.key(-1);
      if ( uiter<uclusters.end() ) ucl=(*uiter);
      if ( viter<vclusters.end() ) vcl=(*viter);
      Int_t iUV=-1;
      if ( ucl.key()<0 )
	iUV=1;
      else if ( vcl.key()<0 )
	iUV=0;
      else if ( (*uiter).mean() < (*viter).mean() )
	iUV=0;
      else
	iUV=1;

      /// Get a reference to the cluster we're working with
      StEEmcSmdCluster &cluster=(iUV==0)?ucl:vcl;

      /// Deterimine how many points are matched to this cluster
      std::vector<Int_t> matched=(iUV==0)?
	u2p[cluster.key()]:
	v2p[cluster.key()];


      /// cluster is orphaned, no matching V cluster, or cluster
      /// matches multiple points, then we go to the next cluster
      if (  matched.size()==0 || matched.size() >1 )
	{
	  if ( iUV==0 ) uiter++;
	  if ( iUV==1 ) viter++;
	  continue;
	}

      /// When set, it means we have found an SMD cluster which
      /// uniquely matches an smd point.  Thus, we will not proceed
      /// to the second-stage logic which determines ambiguous
      /// matches using energies of clusters
      go=true;
      	
      /// push the point into the list of points and remove
      /// the associated pair of clusters
      StEEmcPoint p=smdpoints[matched.back()];
      
      /// add to list of candidate points
      mypoints.push_back(p);

      if ( iUV==0 ) uiter++;
      if ( iUV==1 ) viter++;
          
    }


  /// Loop over candidate points and find best "chi^2" from the
  /// potential 1:1 matches
  Float_t chisq=9.0E9;
  Int_t  imin=-1;
  for ( UInt_t i=0; i<mypoints.size(); i++ )
    {
      Float_t eu=mypoints[i].cluster(0).energy();
      Float_t ev=mypoints[i].cluster(1).energy();
      Float_t x2=(eu-ev)*(eu-ev);
      if ( x2 < chisq ) {
	imin=(Int_t)i;
	chisq=x2;
      }
    }


  /// If we found a candidate match, add to the list of points and
  /// call the algorithm recursively.  Then we return.  This is a
  /// "cute" algorithm (i.e. hard to debug).  The idea is that each
  /// call to findPoints is responsible for finding 1 and only 1 point,
  /// then passing the remaining smd clusters to the next iteration of
  /// the algorithm.
  if ( imin >= 0 ) {

    StEEmcPoint p=mypoints[imin];
    removeCluster( uclusters, p.cluster(0).key() );
    removeCluster( vclusters, p.cluster(1).key() );
    points.push_back(p);
    findPoints(sector, uclusters, vclusters, points );
    return true;

  }





  ///         ---------<<<<<<<<< stage 2 >>>>>>>>>------------


  ///
  /// second stage of point finding.  we loop over all remaining 
  /// clusters.  when we have a cluster which matches 2 or more 
  /// points, we will select the point with the better chi^2
  /// (best energy match between the two smd planes).
  ///
  uiter=uclusters.begin();
  viter=vclusters.begin();
  while ( uiter!=uclusters.end() || viter!=vclusters.end() )
    {

      /// Get clusters and determine which plane we're working with
      StEEmcSmdCluster ucl;ucl.key(-1);
      StEEmcSmdCluster vcl;vcl.key(-1);
      if ( uiter!=uclusters.end() ) ucl=(*uiter);
      if ( viter!=vclusters.end() ) vcl=(*viter);
      Int_t iUV=-1;
      if ( ucl.key()<0 )
	iUV=1;
      else if ( vcl.key()<0 )
	iUV=0;
      else if ( (*uiter).mean() < (*viter).mean() )
	iUV=0;
      else
	iUV=1;

      /// Get a reference to the cluster we're working with
      StEEmcSmdCluster &cluster=(iUV==0)?ucl:vcl;

      /// Deterimine how many points are matched to this cluster
      std::vector<Int_t> matched=(iUV==0)?
	u2p[cluster.key()]:
	v2p[cluster.key()];

      /// cluster is orphaned or has a unique match, we'll catch
      /// it on the next pass
      if ( matched.size()==0 || matched.size()==1 )
	{
	  if ( iUV==0 ) uiter++;
	  if ( iUV==1 ) viter++;
	  continue;
	}
      
      /// push the point with lowest chi2 into the list of
      /// candidate points
      StEEmcPoint p=smdpoints[matched.front()];
      
      /// add to list of candidate points
      mypoints.push_back(p);
    
      if ( iUV==0 ) uiter++;
      if ( iUV==1 ) viter++;

    }


  /// Loop over candidate points and find best "chi^2" from the
  /// potential 1:1 matches
  chisq=9.0E9;
  imin=-1;
  for ( UInt_t i=0; i<mypoints.size(); i++ )
    {
      Float_t eu=mypoints[i].cluster(0).energy();
      Float_t ev=mypoints[i].cluster(1).energy();
      Float_t x2=(eu-ev)*(eu-ev);
      if ( x2 < chisq ) {
	imin=(Int_t)i;
	chisq=x2;
      }
    }



  /// If we found a candidate match, add to the list of points and
  /// call the algorithm recursively.  Then we return.  This is a
  /// "cute" algorithm (i.e. hard to debug).  The idea is that each
  /// call to findPoints is responsible for finding 1 and only 1 point,
  /// then passing the remaining smd clusters to the next iteration of
  /// the algorithm.
  if ( imin >= 0 ) {

    StEEmcPoint p=mypoints[imin];
    removeCluster( uclusters, p.cluster(0).key() );
    removeCluster( vclusters, p.cluster(1).key() );
    points.push_back(p);
    findPoints(sector, uclusters, vclusters, points );
    return true;

  }
 

  return true;

}



// ----------------------------------------------------------------------------
void  StEEmcPointMaker::Clear( Option_t *opts )
{

  mEseen=0.;
  mPoints.clear(); 

}

// ----------------------------------------------------------------------------
void StEEmcPointMaker::removeCluster( StEEmcSmdClusterVec_t &clusters, Int_t k )
{
  StEEmcSmdClusterVec_t::iterator iter=clusters.begin();
  while ( iter != clusters.end() )
    {
      if ( (*iter).key() == k ) {
	clusters.erase(iter);
	return;
      }
      iter++;
    }
}

// ----------------------------------------------------------------------------
void StEEmcPointMaker::shareEnergy()
{

  ///
  /// Perform energy sharing between points.  We fit the tower
  /// response (analytically minimize chi^2) for the identified
  /// smd points.
  ///



  Int_t nrow=(Int_t)mPoints.size();
  Int_t ncol=(Int_t)mPoints.size();
  if ( nrow < 1 ) return; /// nothing to do here

  std::vector<Float_t> Ef(nrow,0.);

  TMatrixF fractions(nrow,ncol);

  /// Loop over all hit towers
  for ( Int_t k=0; k<mEEanalysis->numberOfHitTowers(0); k++ ) 
    {

      /// Get hit tower
      StEEmcTower tower=mEEanalysis->hittower(k,0);

      /// Loop over all hit points
      for ( UInt_t i=0; i< mPoints.size(); i++ ) 
	{

	  /// Fraction of point's energy expected 
	  /// within the tower
	  Float_t fi=fracp2t( mPoints[i], tower );
	  if ( fi<=0. ) continue;

	  /// Running sum of energy fraction times tower energy
	  Ef[i] += fi * tower.energy();

	  for ( UInt_t j=0; j<mPoints.size(); j++ )
	    {

	      /// Fraction of point's energy expected
	      /// within the tower
	      Float_t fj=fracp2t( mPoints[j], tower );
	      if (fi*fj<=0.) continue;

	      fractions[i][j] += fi*fj;
	      
	    }
	  
	}

    }

  fractions.Print();

  /// Invert the matrix
  Double_t det = 0.;
  TMatrixF invert= fractions;
  invert.Invert(&det);

  invert.Print();

  TMatrixF test= fractions * invert;

  test.Print();


  /// Now solve for the energies of the points
  
  std::vector<Float_t> epoints(nrow,0.);

  for ( Int_t i=0; i<nrow; i++ ) 
    { 
      for ( Int_t j=0; j<ncol; j++ )
	{
	  epoints[i] += invert[i][j] * Ef[j];
	}      


    }

 

  


}


// ----------------------------------------------------------------------------
Float_t StEEmcPointMaker::fracp2t( StEEmcPoint &p, StEEmcTower &t )
{

  /// Returns the fraction of the point's energy expected in tower


  /// if the tower is not a neighbor of the tower beneath
  /// the point, return nothing
  if ( !t.isNeighbor( p.tower(0) ) ) return 0.;
  //if ( !(t.index()==p.tower(0).index()) ) return 0.;


  /// Get eta and phibin for this tower
  Float_t xeta=(Float_t)t.etabin();
  Float_t xphi=(Float_t)t.phibin();
  Double_t X[]={xphi,xeta}; 

  /// Get the eta/phi bin for the tower beneath the point
  Float_t xeta0=(Float_t)p.tower(0).etabin();
  Float_t xphi0=(Float_t)p.tower(0).phibin();

  /// Get the position of the point on the endcap.  If the point is off
  /// the endcap, we're hosed.  but just return 0.
  Int_t sec,sub,eta;
  Float_t dphi,deta;
  if ( !mEEtow->getTower(p.position(), sec,sub,eta, dphi,deta ) ) return 0.;
  dphi/=2.0;
  deta/=2.0;

  /// Position of the point in fractional eta,phi space
  Float_t xetap=xeta0+deta;
  Float_t xphip=xphi0+dphi;
  Double_t P[]={xphip,xetap,1.0};

  return eeTowerFunction( X, P );

}


// ----------------------------------------------------------------------------
void StEEmcPointMaker::shareEnergySimple()
{

  Int_t limit=mLimit; // algo quickly converges on energy
  Int_t count=0;

  while ( count++ < limit ) 
    {

      /// Sum of energy from each point predicted to be
      /// within each tower
      Float_t sumw[720]; 
      for (Int_t i=0;i<720;i++) sumw[i]=0.;

      /// Loop over all points and increment the predicted
      /// energy in each tower from each point
      for ( UInt_t i=0;i<mPoints.size();i++ )
	{

	  StEEmcPoint point=mPoints[i];
	  StEEmcTower tower=point.tower(0);

	  sumw[ tower.index() ] += point.energy() * fracp2t( point, tower );

	  /// loop over neighboring towers
	  for ( Int_t i=0; i<tower.numberOfNeighbors(); i++ )
	    {
	      StEEmcTower neighbor=tower.neighbor(i);
	      sumw[ neighbor.index() ] += point.energy() * fracp2t( point, neighbor );
	    }

	}


      /// Loop over all points and divide energy of each tower
      /// between the points according to the ratio of the energy
      /// of the point in the tower to the total predicted
      
      for ( UInt_t i=0;i<mPoints.size();i++ )
	{

	  StEEmcPoint &point=mPoints[i]; // note the reference
	  StEEmcTower tower=point.tower(0);
	  Float_t epoint=0.;

	  Float_t frac=0.;
	  if ( !tower.fail() && !tower.stat() && sumw[tower.index()]>0. )
	  frac = point.energy() * fracp2t(point,tower) / sumw[ tower.index() ];

	  epoint += tower.energy() * frac;
	  
	  /// loop over neighboring towers and repeat
	  /// loop over neighboring towers
	  for ( Int_t i=0; i<tower.numberOfNeighbors(); i++ )
	    {
	      StEEmcTower neighbor=tower.neighbor(i);
	      if ( neighbor.stat() || neighbor.fail() || sumw[neighbor.index()]<=0. ) continue;
	      frac = point.energy() * fracp2t(point,neighbor) / sumw[ neighbor.index() ];
	      epoint += frac * neighbor.energy();
	    }
	  
	  
	  /// Set the energy of this point
	  point.energy( epoint );

	}

    }




  std::vector<Bool_t> seen(720,false);
  for ( UInt_t i=0;i<mPoints.size();i++ )
    {

      StEEmcTower tow=mPoints[i].tower(0);
      if ( !seen[ tow.index() ] ) mEseen+=tow.energy();
      seen[ tow.index() ] = true;

      for ( Int_t j=0;j<tow.numberOfNeighbors();j++ )
	{
	  StEEmcTower nei=tow.neighbor(j);
	  if ( !seen[ nei.index() ] ) mEseen += nei.energy();
	  seen[ nei.index() ] = true;
	}

    }
  





}





// ----------------------------------------------------------------------------
void StEEmcPointMaker::fillStEvent()
{


  StEvent *stevent=(StEvent*)GetInputDS("StEvent");
  if ( !stevent ) {
    Warning("fillStEvent","called, but no StEvent to be found");
    return;
  }


  /// loop over all eemc points
  for ( UInt_t i=0; i<mPoints.size(); i++ )
    {

      StEmcPoint *point=mPoints[i].stemc();
      stevent->emcCollection()->addEndcapPoint( point );

      mEtoEE[ point ] = mPoints[i];


    }

}



// ----------------------------------------------------------------------------
void StEEmcPointMaker::verifyStEvent()
{

  Float_t emc_sum_points = 0.;
  Float_t eemc_sum_points = 0.;

  StEvent *stevent=(StEvent*)GetInputDS("StEvent");
  if ( !stevent ) {
    Warning("verifyStEvent","called, but no StEvent to be found");
    return;
  }

  StSPtrVecEmcPoint& emcpts = stevent->emcCollection()->endcapPoints();
  for ( UInt_t i=0;i<emcpts.size();i++ )
    {

      StEmcPoint *p=emcpts[i];
      assert(p);
      emc_sum_points += p->energy();

    }

  for ( UInt_t i=0;i<mPoints.size();i++ )
    {

      StEEmcPoint p=mPoints[i];
      eemc_sum_points += p.energy();

    }

  std::cout << "StEEmcPointMaker point checksum: ";
  if ( emc_sum_points == eemc_sum_points )
    {
      std::cout << "passed";
    }
  else
    std::cout << "FAILED";
  std::cout << std::endl;
    

}


// ----------------------------------------------------------------------------
void StEEmcPointMaker::countRelatives()
{

  /// Loop over all points and count how many are beneath each tower
  Int_t npoints[720];
  for ( Int_t i=0;i<720;i++ ) npoints[i]=0;

  for ( UInt_t i=0;i<mPoints.size();i++ )    
      npoints[ mPoints[i].tower(0).index() ]++;

  /// Loop over all points and set the number of "relatives"
  for ( UInt_t i=0;i<mPoints.size();i++ )
    {

      StEEmcTower tower=mPoints[i].tower(0);
      Int_t nn=tower.numberOfNeighbors();

      Int_t nrel=npoints[ tower.index() ] - 1; // don't count self
      assert(nrel>=0); // pbck

      for ( Int_t j=0;j<nn;j++ )
	{
	  StEEmcTower t2=tower.neighbor(j);
	  nrel+=npoints[ t2.index() ];
	}

      mPoints[i].numberOfRelatives(nrel);

    }
    

}




// ----------------------------------------------------------------------------
void StEEmcPointMaker::shareEnergySmd()
{

  /// Sum of the smd-energy of all points which are in or adjacent
  /// to tower with a given index.
  Float_t sumw[720];for ( Int_t i=0;i<720;i++ )sumw[i]=0.;

  for ( UInt_t ipoint=0;ipoint<mPoints.size();ipoint++ )
    {


      StEEmcPoint point=mPoints[ipoint];
      StEEmcTower tower=point.tower(0);
      sumw[tower.index()]+=point.energy();

      for ( Int_t itow=0;itow<tower.numberOfNeighbors();itow++ )
	{
	  StEEmcTower t=tower.neighbor(itow);
	  Int_t index=t.index();
	  sumw[index]+=point.energy();
	}
     
    }

  /// Now loop over all points and divide energy of each tower in
  /// proportion to the smd response

  for ( UInt_t ipoint=0;ipoint<mPoints.size();ipoint++ )
    {
      
      StEEmcPoint point=mPoints[ipoint]; // note reference
      StEEmcTower tower = point.tower(0);
      Float_t epoint = 0.;
      Int_t index = tower.index();
      epoint += tower.energy() * point.energy() / sumw[index];

      for ( Int_t itow=0;itow<tower.numberOfNeighbors();itow++ )
	{
	  StEEmcTower t=tower.neighbor(itow);
	  index = t.index();
	  epoint += t.energy() * point.energy() / sumw[index];
	}

      //      point.energy( epoint );
      mPoints[ipoint].energy(epoint);

    }


}


