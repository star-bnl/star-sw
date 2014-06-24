/**\\Jason Web, Weihong He
 * \class StEEmcIUPointMaker
 * \brief Class for building points from smd clusters
 *
 * This class produces points using smd clusters found from an
 * instance of StEEmcIUClusterMaker. 
 *
 * A point is defined as the coincidence of a cluster in each smd 
 * plane with an active tower.
 *
 * Points are found in the following order:
 *
 * 1. Loop over all SMD clusters beginning with those closest to
 *    the beam and working radially outwards.
 * 2. Any unique U,V pair below an active tower is considered to 
 *    be a point regardless of the energy match between the pair.
 *    When we find such a match we remove the U and V clusters from
 *    the pool of smd clusters and search for another match. (goto 1).
 * 3. If no unique pair was found, we select the U,V pair with the
 *    closest match in energy between the two planes, if the pair is underneath an active tower, it is identified as a point candidate. Once we find all point candidates in the sector, we check to see if we need to apply splitting algo to save more points according to energy matching. Then, we remove the
 *    pair of clusters from the pool of clusters, and repeat.  (goto 1).
 * 4. The algo procedes to the next sector.
 * 5. The energy of a point is decided by tower cluster energy. The position of a point is decided by SMD information.
 */

#include "StEEmcIUPointMaker.h" 
#include "StEEmcPool/StEEmcA2EMaker/StEEmcA2EMaker.h"
#include "StEEmcIUClusterMaker.h"
#include "StEEmcIUCluster.h"
#include "StEEmcIUSmdCluster.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcUtil/StEEmcSmd/EEmcSmdGeom.h" 
#include "TH1F.h"
#include "TH2F.h"
#include <iostream> 
#include <algorithm>
#include <map>

#include "StEEmcPool/StEEmcPointMaker/eeTowerFunction.h"

/* StEvent stuff */
#include "StEvent/StEvent.h"
#include "StEvent/StEmcCollection.h"
#include "StEvent/StEmcPoint.h"

/* Root's linear algebra package */
#include "TMatrixF.h"

//#define DEBUG_buildPoints

ClassImp(StEEmcIUPointMaker);

// ----------------------------------------------------------------------------
StEEmcIUPointMaker::StEEmcIUPointMaker(const Char_t *name):StMaker(name)
{
  std::cout << "StEEmcIUPointMaker("<<name<<")" << std::endl;

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
    mSmdMatch = 0.;
    ukey=0;
    vkey=0;

}

// ----------------------------------------------------------------------------
Int_t StEEmcIUPointMaker::Init()
{
    mEEanalysis=(StEEmcA2EMaker*)GetMaker(mNameAnalysis);
    mEEclusters=(StEEmcIUClusterMaker*)GetMaker(mNameClusters);
    assert(mEEanalysis);
    assert(mEEclusters);
    hZEratio=new TH1F("smdezratio","energy Zratio between smd clusters",10,0.,1.);
    pointet=new TH1F("pointet","transverse energy of every point candidate",40,0,10);
    pointgeo=new TH2F("pointgeo", "Reconstructed Point Geometry; phi / deg; Eta",360,-180.,180,20,1.,2.);
    return StMaker::Init();
}

// ----------------------------------------------------------------------------
Int_t StEEmcIUPointMaker::Make()
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
      StEEmcIUSmdClusterVec_t uclusters=mEEclusters->smdclusters(sector,0);
      StEEmcIUSmdClusterVec_t vclusters=mEEclusters->smdclusters(sector,1);

      /// Sort from closest to the beam to furthest
      std::sort( uclusters.begin(), uclusters.end(), inner );
      std::sort( vclusters.begin(), vclusters.end(), inner );

      findPoints( sector, uclusters, vclusters, mPoints );
      
    }


  ///
  /// Point energies were initially set to be the energy 
  /// deposited in the SMD.  We will now overwrite these
  /// energies, calculating the equivalent EM energy using
  /// a sampling fraction of 0.7% per smd plane?
  ///  
  for ( UInt_t i=0;i<mPoints.size();i++ )
    {
      
      mPoints[i].energy( (Float_t)(mPoints[i].energy()/0.007/2.) );
     
      TVector3 pointpos=mPoints[i].position();
      pointpos=pointpos.Unit();
      Float_t pet = (mPoints[i].energy()*pointpos).Perp();
      pointet->Fill(pet);
      pointgeo->Fill(mPoints[i].position().Phi()/3.1416*180,mPoints[i].position().Eta());
    }
  



  StEEmcIUPointVec_t orgpoints=mPoints;

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

  /*
  for ( UInt_t i=0;i<mPoints.size();i++ )
    {
      mPoints[i].print();
    }
  for ( Int_t i=0;i<10;i++ ) std::cout << std::endl;
*/
    
  return kStOK;
}

// ----------------------------------------------------------------------------

StEEmcIUPointVec_t StEEmcIUPointMaker::buildSmdPoints( Int_t sector, 
						   StEEmcIUSmdClusterVec_t &u, 
						   StEEmcIUSmdClusterVec_t &v )
{

  StEEmcIUPointVec_t points;
  //printf("u.size=%d v.size=%d\n",u.size(),v.size());
  for ( UInt_t i=0; i<u.size(); i++ ) 
    {

      StEEmcIUSmdCluster uc=u[i];
      
      Float_t xu=uc.mean();


      for ( UInt_t j=0;j<v.size(); j++ ) 
	{

	  
	  StEEmcIUSmdCluster vc=v[j];
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
	  if ( mEEanalysis->tower(sec,sub,eta).energy() > mTowerThreshold ) 
	    good=true;
	  else if ( mEEanalysis->tower(sec,sub,eta).fail() ) 
	    {
	      for ( Int_t layer=1;layer<=3;layer++ ) 
		{
		  if ( mEEanalysis->tower(sec,sub,eta,layer).energy() > 0. )
		    good=true;		
		}
	    }

	  /// furthermore, we may require a degree of energy
	  /// matching between the smd points
	  /*
	  Float_t uvdiff = TMath::Abs( uc.energy() - vc.energy() );
	  Float_t uvavg  = 0.5 * ( uc.energy() + vc.energy() );
	  if ( uvdiff > mSmdMatch * uvavg ) good = false;
	  */
	  Float_t Eu=uc.energy();
	  Float_t Ev=vc.energy();
	  if ( Eu < mSmdMatch * Ev || Ev < mSmdMatch * Eu ) good=false;
	  
	  if ( good ) {

	    StEEmcIUPoint p;
	    p.cluster( uc, 0 );
	    p.cluster( vc, 1 );
	    p.energy( uc.energy() + vc.energy() );
	    p.tower( mEEanalysis->tower(sec,sub,eta) );
	    TVector3 position=mEEsmd->getIntersection(sector,uc.mean(),vc.mean());
	    p.position(position);
	    points.push_back(p);

	    /// Add to list of smd only points 
	    mSmdPoints.push_back(p);


	  }
	         	
	}

    }
  
  return points;

}

















// ----------------------------------------------------------------------------
Bool_t StEEmcIUPointMaker::findPoints( Int_t sector, 
				     StEEmcIUSmdClusterVec_t uclusters, 
				     StEEmcIUSmdClusterVec_t vclusters,
				     StEEmcIUPointVec_t &points )
{


  /// Temp vector to store candidate points
  StEEmcIUPointVec_t Tmypoints;
  StEEmcIUPointVec_t mypoints;
  /// Sort from closest to the beam to furthest
  std::sort( uclusters.begin(), uclusters.end(), inner );
  std::sort( vclusters.begin(), vclusters.end(), inner );

  /// build smd points
  StEEmcIUPointVec_t smdpoints = buildSmdPoints( sector, uclusters, vclusters );

 
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

  ////     stage zero
  Bool_t go = false;
  StEEmcIUSmdClusterVec_t::iterator uiter=uclusters.begin();
  StEEmcIUSmdClusterVec_t::iterator viter=vclusters.begin();


  /// look for smd clusters which match a single smd point.
  /// where we find such unique matches, we add the point to
  /// the list of found points, and remove the matching pair
  /// of smd clusters from the list of clusters.  
  
  //    ----------<<<<<<<<< stage one >>>>>>>>>-------------
 
  int iii=0,jjj=0;

  while ( uiter<uclusters.end() || viter<vclusters.end() )
    {

      /// Get clusters and determine which plane we're working with
      StEEmcIUSmdCluster ucl;ucl.key(-1);
      StEEmcIUSmdCluster vcl;vcl.key(-1);
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
      StEEmcIUSmdCluster &cluster=(iUV==0)?ucl:vcl;

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
      StEEmcIUPoint p=smdpoints[matched.back()];
      
      /// add to list of candidate points
      Tmypoints.push_back(p);
      
      if ( iUV==0 ) {uiter++;iii++;}
      if ( iUV==1 ) {viter++;jjj++;}
      
    }//end of while
 

  //splitting algo starts. When two points ramp into one lane, it only shows us one Smd cluster information in one of the Smd planes, thus can only reconstruct one point. We now split the overlapped U/V Smd cluster based on the other two Smd cluster in V/U accordingly. 
  std::sort( Tmypoints.begin(), Tmypoints.end(), chiSquare );
  //>=2 to turn on splitting algo, <0 to off splitting algo.
  if(Tmypoints.size()>=2)
    { 
      int aa=0,bb=0;
      for ( UInt_t i=0; i<Tmypoints.size(); i++ )
	{
	  StEEmcIUSmdCluster uone=Tmypoints[i].cluster(0);

	  StEEmcIUSmdCluster vone=Tmypoints[i].cluster(1);

	  
	  for ( UInt_t j=i+1; j<Tmypoints.size(); j++ )
	    {	  
	      StEEmcIUSmdCluster utwo=Tmypoints[j].cluster(0);
	      StEEmcIUSmdCluster vtwo=Tmypoints[j].cluster(1);
	      
	      if((uone.mean()==utwo.mean())||(vone.mean()==vtwo.mean()))
		{
		  //case 1
		  if(uone.mean()==utwo.mean()&&(aa==0)&&(vone.mean()!=vtwo.mean()))
		    {
		      float uzr=fabs(uone.energy()-vone.energy()-vtwo.energy())/(uone.energy()+vone.energy()+vtwo.energy());
		     
		      hZEratio->Fill(uzr);
		     
		      if(uzr<=0.2)
			{
			  float vratio1=vone.energy()/(vone.energy()+vtwo.energy());
			  float vratio2=vtwo.energy()/(vone.energy()+vtwo.energy());
			   
			  int ukey=uone.key();
			  int ukey1=ukey+100;
			  int ukey2=ukey+200;
			  float u1energy=uone.energy()*vratio1;
			  float u2energy=uone.energy()*vratio2;
			  uone.key(ukey1);
			  utwo.key(ukey2);
			  uone.energy(u1energy);
			  utwo.energy(u2energy);
			  float p1energy=uone.energy()+vone.energy();
			  Tmypoints[i].energy(p1energy);
			  float p2energy=utwo.energy()+vtwo.energy();
			  Tmypoints[j].energy(p2energy);
			  Tmypoints[i].cluster(uone,0);
			  Tmypoints[j].cluster(utwo,0);
			  StEEmcIUPoint p1=Tmypoints[i];
			  
			  //mypoints.push_back(p1);
			  StEEmcIUPoint p2=Tmypoints[j];
			  //mypoints.push_back(p2);
			  
			  removeCluster( uclusters, p1.cluster(0).key() );
			  removeCluster( vclusters, p1.cluster(1).key() );
			  points.push_back(p1);
			  removeCluster( uclusters, p2.cluster(0).key() );
			  removeCluster( vclusters, p2.cluster(1).key() );
			  points.push_back(p2);
			  removeCluster( uclusters, ukey );
			  
			  findPoints(sector, uclusters, vclusters, points );
			  return true;
			  
			  aa++;
			}
		    } //end of case 1
		  //case 2
		  if(vone.mean()==vtwo.mean()&&(bb==0)&&(uone.mean()!=utwo.mean()))
		    {
		      float vzr=fabs(vone.energy()-uone.energy()-utwo.energy())/(vone.energy()+uone.energy()+utwo.energy());
		      hZEratio->Fill(vzr);
		      if(vzr<=0.2)
			{
			  float uratio1=uone.energy()/(uone.energy()+utwo.energy());
			  float uratio2=utwo.energy()/(uone.energy()+utwo.energy());
			  
			  
			  int vkey=vone.key();
			  int vkey1=vkey+100;
			  int vkey2=vkey+200;
			  float v1energy=vone.energy()*uratio1;
			  float v2energy=vone.energy()*uratio2;
			  vone.key(vkey1);
			  vtwo.key(vkey2);
			  vone.energy(v1energy);
			  vtwo.energy(v2energy);
			  float p1energy=vone.energy()+uone.energy();
			  Tmypoints[i].energy(p1energy);
			  float p2energy=vtwo.energy()+utwo.energy();
			  Tmypoints[j].energy(p2energy);
			  Tmypoints[i].cluster(vone,1);
			  Tmypoints[j].cluster(vtwo,1);
			  StEEmcIUPoint p1=Tmypoints[i];
			  
			  //mypoints.push_back(p1);
			  StEEmcIUPoint p2=Tmypoints[j];
			  //mypoints.push_back(p2);
			  
			  removeCluster( uclusters, p1.cluster(0).key() );
			  removeCluster( vclusters, p1.cluster(1).key() );
			  points.push_back(p1);
			  removeCluster( uclusters, p2.cluster(0).key() );
			  removeCluster( vclusters, p2.cluster(1).key() );
			  points.push_back(p2);
			  removeCluster( vclusters, vkey );
			  
			  findPoints(sector, uclusters, vclusters, points );
			  return true;
			  
			  
			  bb++;
			}
		    }//end of case 2
		  //case 3
		  if(uone.mean()==utwo.mean()&&(vone.mean()==vtwo.mean()))
		    {
		      int cc=0;
		      for ( UInt_t i=0; i<Tmypoints.size(); i++ )
			{
			  StEEmcIUPoint p=Tmypoints[i];
			  mypoints.push_back(p);
			  cc++;
			}
		
		      //printf("caase 3 points.size=%d\n",mypoints.size());
		    }//end of case 3
		}
	    }
	}

      //case 4 catch those isolated points
      for ( UInt_t i=0; i<Tmypoints.size(); i++ )
	{
	  StEEmcIUPoint pp=Tmypoints[i];
	  mypoints.push_back(pp);
	}//end of case 4
    }
  else
    {
      for ( UInt_t i=0; i<Tmypoints.size(); i++ )
	{
	  StEEmcIUPoint p=Tmypoints[i];
	  mypoints.push_back(p);
	}
    }
  /// Loop over candidate points and find best "chi^2" from the
  /// potential 1:1 matches
  Float_t chisq=9.0E9;
  Int_t  imin=-1;
  for ( UInt_t i=0; i<mypoints.size(); i++ )
    {
      Float_t eu=mypoints[i].cluster(0).energy();
      Float_t ev=mypoints[i].cluster(1).energy();
      Float_t x2=fabs((eu-ev)/(eu+ev));
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

    StEEmcIUPoint p=mypoints[imin];
 
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
      StEEmcIUSmdCluster ucl;ucl.key(-1);
      StEEmcIUSmdCluster vcl;vcl.key(-1);
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
      StEEmcIUSmdCluster &cluster=(iUV==0)?ucl:vcl;

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
      StEEmcIUPoint p=smdpoints[matched.front()];
      //StEEmcIUPoint p2=smdpoints[matched.front()++];
      /// add to list of candidate points
      mypoints.push_back(p);
      //mypoints.push_back(p2);
    
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
      Float_t x2=fabs((eu-ev)/(eu+ev));
      if ( x2 < chisq ) {
	imin=(Int_t)i;
	chisq=x2;
      }
    }
  float chisq2=9.0E9;
  int imin2=-1,inn=-1;
  
  for ( UInt_t i=0; i<mypoints.size(); i++ )
    {
      inn=i;
      if(inn!=imin)
	{

	  Float_t eu=mypoints[i].cluster(0).energy();
	  Float_t ev=mypoints[i].cluster(1).energy();
	  Float_t x22=(eu-ev)*(eu-ev);
	  if ( x22 < chisq2 ) {
	    imin2=i;
	    chisq2=x22;
	  }
	}
    }



  /// If we found a candidate match, add to the list of points and
  /// call the algorithm recursively.  Then we return.  This is a
  /// "cute" algorithm (i.e. hard to debug).  The idea is that each
  /// call to findPoints is responsible for finding 1 and only 1 point,
  /// then passing the remaining smd clusters to the next iteration of
  /// the algorithm.
  if ( imin >= 0 ) {

    StEEmcIUPoint p1=mypoints[imin];
 
    removeCluster( uclusters, p1.cluster(0).key() );
    removeCluster( vclusters, p1.cluster(1).key() );
 
    points.push_back(p1);
 
    findPoints(sector, uclusters, vclusters, points );
    return true;

  }


  return true;

}



// ----------------------------------------------------------------------------
void  StEEmcIUPointMaker::Clear( Option_t *opts )
{

  mEseen=0.;
  mPoints.clear(); 
  mSmdPoints.clear(); 

}

// ----------------------------------------------------------------------------
void StEEmcIUPointMaker::removeCluster( StEEmcIUSmdClusterVec_t &clusters, Int_t k )
{
  StEEmcIUSmdClusterVec_t::iterator iter=clusters.begin();
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
void StEEmcIUPointMaker::shareEnergy()
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
Float_t StEEmcIUPointMaker::fracp2t( StEEmcIUPoint &p, StEEmcTower &t )
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
void StEEmcIUPointMaker::shareEnergySimple()
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

	  StEEmcIUPoint point=mPoints[i];
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

	  StEEmcIUPoint &point=mPoints[i]; // note the reference
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
void StEEmcIUPointMaker::fillStEvent()
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
void StEEmcIUPointMaker::verifyStEvent()
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

      StEEmcIUPoint p=mPoints[i];
      eemc_sum_points += p.energy();

    }

  std::cout << "StEEmcIUPointMaker point checksum: ";
  if ( emc_sum_points == eemc_sum_points )
    {
      std::cout << "passed";
    }
  else
    std::cout << "FAILED";
  std::cout << std::endl;
    

}


// ----------------------------------------------------------------------------
void StEEmcIUPointMaker::countRelatives()
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
void StEEmcIUPointMaker::shareEnergySmd()
{

  /// Sum of the smd-energy of all points which are in or adjacent
  /// to tower with a given index.
  Float_t sumw[720];for ( Int_t i=0;i<720;i++ )sumw[i]=0.;
  Float_t sumw1[720];for ( Int_t i=0;i<720;i++ )sumw1[i]=0.;

  for ( UInt_t ipoint=0;ipoint<mPoints.size();ipoint++ )
    {

      StEEmcIUPoint point=mPoints[ipoint];
      StEEmcTower tower=point.tower(0);
      sumw[tower.index()]+=point.energy();
      sumw1[tower.index()]+=point.energy(); 
      //printf("en==%f index=%d sumw=%f\n",point.energy(),tower.index(),sumw[tower.index()]);
      for ( Int_t itow=0;itow<tower.numberOfNeighbors();itow++ )
	{
	  StEEmcTower t=tower.neighbor(itow);
	  Int_t index=t.index();
	 
	  sumw[index]+=point.energy();
	  //printf("index=%d sumw=%f\n",index,sumw[index]); 
	}
     
    }
  //printf("end of first loop.........................\n");
  /// Now loop over all points and divide energy of each tower in
  /// proportion to the smd response

  for ( UInt_t ipoint=0;ipoint<mPoints.size();ipoint++ )
    {
      
      StEEmcIUPoint point=mPoints[ipoint]; // note reference
      StEEmcTower tower = point.tower(0);
      StEEmcTower pre1  = mEEanalysis->tower(tower.index(),1); 
      StEEmcTower pre2  = mEEanalysis->tower(tower.index(),2); 
      StEEmcTower post  = mEEanalysis->tower(tower.index(),3); 
      //hPostE->Fill(post.energy()*1000.0);
      Float_t epoint = 0.;
      Float_t epre1  = 0.; 
      Float_t epre2  = 0.; 
      Float_t epost  = 0.;  

      Int_t index = tower.index();
      epoint += tower.energy() * point.energy() / sumw[index];
      //printf("start of the calculation of a point\n");
      //printf("tower.energy=%f epoint=%f smde=%f sumw=%f index=%d\n",tower.energy(),epoint,point.energy(),sumw[index],index);
      epre1  += pre1.energy()  * point.energy() / sumw1[index]; 
      epre2  += pre2.energy()  * point.energy() / sumw1[index]; 
      epost  += post.energy()  * point.energy() / sumw[index]; 
      //printf("start of the neighbor calculation of a point\n");
      for ( Int_t itow=0;itow<tower.numberOfNeighbors();itow++ )
	{
	  StEEmcTower t=tower.neighbor(itow);
//	  StEEmcTower p=pre1.neighbor(itow);
//	  StEEmcTower q=pre2.neighbor(itow);
	  StEEmcTower r=post.neighbor(itow); 

	  index = t.index();
	  
	  epoint += t.energy() * point.energy() / sumw[index];
	  
	  //printf("neighbortower.energy=%f epoint=%f smde=%f sumw=%f index=%d\n",t.energy(),epoint,point.energy(),sumw[index],index);
//	  epre1  += p.energy() * point.energy() / sumw[index];
//	  epre2  += q.energy() * point.energy() / sumw[index];
//	  epost  += r.energy() * point.energy() / sumw[index];
	}


      mPoints[ipoint].energy(epoint);
      mPoints[ipoint].energy(epre1,1);
      mPoints[ipoint].energy(epre2,2);
      mPoints[ipoint].energy(epost,3);
 
      
    }
  
  
}


