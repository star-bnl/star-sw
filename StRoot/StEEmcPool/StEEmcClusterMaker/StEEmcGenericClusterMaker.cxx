#include "StEEmcGenericClusterMaker.h"

#include "TH1F.h"
#include "TH2F.h"
#include "TLine.h"

#include "StEEmcPool/StEEmcA2EMaker/StEEmcA2EMaker.h"

#include "StEvent.h"
#include "StEmcDetector.h"
#include "StEmcCollection.h"

#include "StMessMgr.h"

#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"

ClassImp(StEEmcGenericClusterMaker);

// ----------------------------------------------------------------------------
StEEmcGenericClusterMaker::StEEmcGenericClusterMaker( const Char_t *name, const StEEmcA2EMaker *a2e ) 
    : StMaker(name)
{

  mEEanalysis=a2e; /* adc to energy maker */

  //  mCollection=new StEEmcClusterCollection();

  mEEmcGeom=new EEmcGeomSimple();
  mESmdGeom=EEmcSmdGeom::instance();
  mESmdMap =EEmcSmdMap::instance();

  ///
  /// Initialize storage banks for tower and SMD clusters.
  /// Both will be stored sector-wise and layer-wise.
  ///
  /// Tower clusters are stored according to the sector
  /// in which we find their seed tower.  
  ///
  /// Layers: T=0, P=1, Q=2, R=3   /  Planes: U=0, V=1
  ///
  /// mTowerClusters[sector][layer] returns vector of tower clusters
  /// mSmdClusters[sector][plane]   returns vector of SMD clusters
  ///

  StEEmcClusterVec_t t;
  std::vector< StEEmcClusterVec_t > layers;
  for ( Int_t i = 0; i < 4; i++ ) layers.push_back(t);
  for ( Int_t i = 0; i < 12; i++ ) mTowerClusters.push_back(layers);

  StEEmcSmdClusterVec_t s;
  std::vector< StEEmcSmdClusterVec_t > planes;
  planes.push_back(s);
  planes.push_back(s);
  for ( Int_t i = 0; i < 12; i++ ) mSmdClusters.push_back(planes);

  mSmdMatchRange = 40;

  
  
  /// Tracks will be matched to clusters if they fall within a user-specified
  /// distance between the track extrapolated to the layer containing the 
  /// cluster.  (Tracks are extrapolated to the smd layer when matching
  /// tower clusters).
  mClusterTrackSeparation[0] = 0.05;  /**< default parameter: tower, dr<0.05 where dr=sqrt(deta^2+dphi^2) */
  mClusterTrackSeparation[1] = 0.075; /**< default parameter: preshower 1 dr<0.075 */
  mClusterTrackSeparation[2] = 0.075; /**< default parameter: preshower 2 dr<0.075 */
  mClusterTrackSeparation[3] = 0.075; /**< default parameter: postshower  dr<0.075 */
  mClusterTrackSeparation[4] = 3.0;   /**< default parameter: smd u 3.0 strips */
  mClusterTrackSeparation[5] = 3.0;   /**< default parameter: smd v 3.0 strips */

  Clear();

}

// ----------------------------------------------------------------------------
Int_t StEEmcGenericClusterMaker::Init()
{

  // If it's missing from the constructor, obtain pointer to 
  // the "default" eemc adc 2 energy maker.  If it doesn't
  // exist, crash and burn.
  if (!mEEanalysis) mEEanalysis=(StEEmcA2EMaker*)GetMaker("EEmcAdc2Energy");
  assert(mEEanalysis);// no input calib maker

  return StMaker::Init();
}

// ----------------------------------------------------------------------------
Int_t StEEmcGenericClusterMaker::Make() // runs after user's Make()
{
  Int_t result = StMaker::Make(); 
  makeClusterMap();
  makeTrackMap();
  makeStEvent();
  makeHistograms();

  // add clusters to collection

  /***  
  mCollection->setNumberOfClusters(numberOfClusters());
  for ( Int_t sec=0;sec<12;sec++ )
    {
      for ( Int_t lay=0;lay<4;lay++ )
	{
	  for ( UInt_t i=0;i<mTowerClusters[sec][lay].size();i++ )
	    {
	      mCollection->add( mTowerClusters[sec][lay][i] );
	    }
	}
      for ( Int_t pln=0;pln<2;pln++ )
	{
	  for ( UInt_t i=0;i<mSmdClusters[sec][pln].size();i++ )
	    {
	      mCollection->add( mSmdClusters[sec][pln][i] );
	    }
	}
    }
  ***/
    
  return result;

}

void StEEmcGenericClusterMaker::makeHistograms()
{
 
 
}

void StEEmcGenericClusterMaker::makeClusterMap()
{

  
  // loop over all sectors
  for ( UInt_t sec=0;sec<12;sec++ )
    {

      // get vectors of clusters in this sector
      StEEmcClusterVec_t    &Tclusters = mTowerClusters[sec][0];
      StEEmcClusterVec_t    &Pclusters = mTowerClusters[sec][1];
      StEEmcClusterVec_t    &Qclusters = mTowerClusters[sec][2];
      StEEmcClusterVec_t    &Rclusters = mTowerClusters[sec][3];
      StEEmcSmdClusterVec_t &Uclusters = mSmdClusters[sec][0];
      StEEmcSmdClusterVec_t &Vclusters = mSmdClusters[sec][1];

      // loop over all cluster pairs and build associative maps
      for ( UInt_t t=0;t<Tclusters.size(); t++ )// towers
	{

	  StEEmcCluster &c = Tclusters[t];
	  EEmatch *mymatch = &mClusterMap[c.key()];

	  for ( UInt_t p=0;p<Pclusters.size();p++ )	    
	    if ( match(c,Pclusters[p]) ) mymatch->pre1.push_back(Pclusters[p]);
	  for ( UInt_t q=0;q<Qclusters.size();q++ )	    
	    if ( match(c,Qclusters[q]) ) mymatch->pre2.push_back(Qclusters[q]);
	  for ( UInt_t r=0;r<Rclusters.size();r++ )	    
	    if ( match(c,Rclusters[r]) ) mymatch->post.push_back(Rclusters[r]);
	  for ( UInt_t u=0;u<Uclusters.size();u++ )
	    if ( match(c,Uclusters[u]) ) mymatch->smdu.push_back(Uclusters[u]);
	  for ( UInt_t v=0;v<Vclusters.size();v++ )
	    if ( match(c,Vclusters[v]) ) mymatch->smdv.push_back(Vclusters[v]);


	  for ( UInt_t p=0;p<Pclusters.size();p++ )	    
	    if ( match(c,Pclusters[p]) ) c.addMatch( Pclusters[p].key(), 1 );
	  for ( UInt_t q=0;q<Qclusters.size();q++ )	    
	    if ( match(c,Qclusters[q]) ) c.addMatch( Qclusters[q].key(), 2 );
	  for ( UInt_t r=0;r<Rclusters.size();r++ )	    
	    if ( match(c,Rclusters[r]) ) c.addMatch( Rclusters[r].key(), 3 );
	  for ( UInt_t u=0;u<Uclusters.size();u++ )
	    if ( match(c,Uclusters[u]) ) c.addMatch( Uclusters[u].key(), 4 );
	  for ( UInt_t v=0;v<Vclusters.size();v++ )
	    if ( match(c,Vclusters[v]) ) c.addMatch( Vclusters[v].key(), 5 );
	   
	}

#if 0
      // print matches
      for ( UInt_t i=0;i<Tclusters.size();i++ )
	{

	  std::cout << "sec=" << sec << " i=" << i << " ------------------------------------" << std::endl;
	  StEEmcCluster c=Tclusters[i];
	  c.print();
	  Int_t k=c.key();
	  EEmatch m=mClusterMap[k];
	  for ( UInt_t j=0;j<m.pre1.size();j++ )	    
	    m.pre1[j].print();
	  for ( UInt_t j=0;j<m.pre2.size();j++ )
	    m.pre2[j].print();
	  for ( UInt_t j=0;j<m.post.size();j++ )
	    m.post[j].print();
	  for ( UInt_t j=0;j<m.smdu.size();j++ )
	    m.smdu[j].printLine();
	  for ( UInt_t j=0;j<m.smdv.size();j++ )
	    m.smdv[j].printLine();
	   
	}
    
#endif
#if 0
      // print matches
      for ( UInt_t i=0;i<Tclusters.size();i++ )
	{
	  std::cout << "sec=" << sec << " i=" << i << " ------------------------------------" << std::endl;
	  StEEmcCluster c=Tclusters[i];
	  c.print();

	  std::cout << "pre1 matches: ";
	  for ( Int_t j=0;j<c.numberOfMatchingClusters(1);j++ ) 
	    std::cout << c.getMatch(j,1) << " ";
	  std::cout << std::endl;

	  std::cout << "pre2 matches: ";
	  for ( Int_t j=0;j<c.numberOfMatchingClusters(2);j++ ) 
	    std::cout << c.getMatch(j,2) << " ";
	  std::cout << std::endl;


	  std::cout << "post matches: ";
	  for ( Int_t j=0;j<c.numberOfMatchingClusters(3);j++ ) 
	    std::cout << c.getMatch(j,3) << " ";
	  std::cout << std::endl;


	}
#endif


    }





}

void StEEmcGenericClusterMaker::makeTrackMap()
{

  LOG_DEBUG << GetName() << " -I- entering makeTrackMap()" << endm;

  StMuDstMaker *maker = (StMuDstMaker*)GetMaker("MuDst");
  if ( !maker )
    {
      LOG_DEBUG << GetName() << " -I- mudst maker not in chain?" << endm;
      return;
    }

  StMuDst *mudst = maker->muDst();

  //--
  //-- Match primary tracks to tower, pre/postshower clusters
  //--
  Int_t nprimary = (Int_t)mudst->numberOfPrimaryTracks();

  LOG_DEBUG<<" checking nprimary="<<nprimary<< " tracks"<<endm;

  for ( Int_t iprimary = 0; iprimary < nprimary; iprimary++ )
    {
      StMuTrack *track = mudst->primaryTracks(iprimary);
      if (!track)continue;
      
      /// loop over sectors
      for ( Int_t sec=0;sec<12;sec++ )
	{

	  /// loop over layers
	  for ( Int_t layer = 0; layer < 4; layer++ )
	    {
	  
	      for ( Int_t ii=0;ii<numberOfClusters(sec,layer); ii++ )
		{
		  StEEmcCluster mycluster = cluster(sec,layer,ii);
		  if ( match(mycluster, track) )
		    {
		      LOG_DEBUG << GetName() << " -I- matched cluster to track in layer=" << layer << endm;		  
		      //mycluster.print();
		      //std::cout << "cluster: eta=" << mycluster.momentum().Eta() << " phi=" << mycluster.momentum().Phi() << std::endl;
		      //std::cout << "track:   eta=" << track->eta() << " phi=" << track->phi() << std::endl;
		      mClusterTrackMap[ mycluster.key() ].push_back( track );
		    }
		}

	    }
	}

    }


  
  //--
  //-- Match global tracks flagged as background to tower, pre/postshower
  //-- clusters
  //--
  Int_t nglobal = (Int_t)mudst->numberOfGlobalTracks();
  for ( Int_t iglobal = 0; iglobal < nglobal; iglobal++ )
    {
      StMuTrack *track = mudst->globalTracks(iglobal);
      if (!track) continue;

      // verify track from background tracker
      if (! (track->flag()==901) ) continue;

      /// loop over sectors
      for ( Int_t sec=0;sec<12;sec++ )
	{

	  /// loop over layers
	  for ( Int_t layer = 0; layer < 4; layer++ )
	    {
	  
	      for ( Int_t ii=0;ii<numberOfClusters(sec,layer); ii++ )
		{
		  StEEmcCluster mycluster = cluster(sec,layer,ii);
		  if ( matchBackgroundTrack(mycluster, track) )
		    {
		      LOG_DEBUG << GetName() << " -I- matched cluster to background track in layer=" << layer << endm;		  
		      //mycluster.print();
		      //std::cout << "cluster: eta=" << mycluster.momentum().Eta() << " phi=" << mycluster.momentum().Phi() << std::endl;
		      //std::cout << "track:   eta=" << track->eta() << " phi=" << track->phi() << std::endl;
		      mBackgroundTrackMap[ mycluster.key() ].push_back( track );
		    }
		}

	    }
	}

    }
  
  

}



void StEEmcGenericClusterMaker::makeStEvent()
{

  /*
   * create StEmcClusters and fill StEvent (to be added)
   */

  StEvent *stevent=(StEvent*)GetInputDS("StEvent");
  if ( !stevent ) {
    // assume we're running on MuDst and no collections need populated
    return;
  }
  
  ///
  /// First the eemc tower clusters
  ///
  StEmcDetector *detector=stevent->emcCollection()->detector(kEndcapEmcTowerId);
  if ( !detector )
    {
      // meh Warning("fillStEvent","detector == NULL, MAJOR StEvent problem, continuing");
      return;
    }
  ///
  /// Next the pre and postshower clusters
  ///
  detector=stevent->emcCollection()->detector(kEndcapEmcPreShowerId);
  if ( !detector )
    {
      // meh Warning("fillStEvent","detector == NULL for pre/post, no clusters for you");
      return;
    }
  ///
  /// Finally the U&V smd clusters
  ///
  StDetectorId ids[]={ kEndcapSmdUStripId, kEndcapSmdVStripId };

  
  for ( Int_t iplane=0; iplane<2; iplane++ )
    {

      detector=stevent->emcCollection()->detector(ids[iplane]);
      if ( !detector )
        {
          // meh Warning("fillStEvent","detector == NULL for smd plane, no clusters for you");
	  return;
        }

    }


  return;
}

// ----------------------------------------------------------------------------
void StEEmcGenericClusterMaker::Clear(Option_t *opts)
{
  StMaker::Clear();
  // clears storage arrays
  for ( Int_t sector=0; sector<12; sector++ ) {
    for ( Int_t layer=0; layer<4; layer++ )
      mTowerClusters[sector][layer].clear();
    for ( Int_t plane=0; plane<2; plane++ )
      mSmdClusters[sector][plane].clear();
  }
  for ( Int_t i=0;i<6;i++ ) mNumberOfClusters[i]=0;

  // clear the cluster map
  mClusterMap.clear();
  mClusterTrackMap.clear();
  mBackgroundTrackMap.clear();
  
  // reset the cluster id
  mClusterId = 0;

  // clear the collection
  //  mCollection->Clear();
  

}

// ----------------------------------------------------------------------------
void StEEmcGenericClusterMaker::add(const StEEmcCluster &cluster)
{
  StEEmcCluster c = cluster;
  assert( c.towers().size() ); // cluster with no towers?

  Int_t key    = nextClusterId();     /* next available cluster id */
  Int_t sector = c.tower(0).sector(); /* sector of seed tower      */
  Int_t layer  = c.tower(0).layer();  /* layer of the cluster      */
  c.key(key);
  mTowerClusters[sector][layer].push_back(c);

  assert( mTowerClusters[sector][layer].back().towers().size() );
  
  // add this cluster to the cluster map
  EEmatch match;
  match.tower.push_back(c);
  mClusterMap[ key ] = match;

  // add this cluster to the cluster track map
  std::vector< StMuTrack* > v, u;
  mClusterTrackMap[ key ] = v;
  mBackgroundTrackMap[ key ] = u;

  mNumberOfClusters[c.tower(0).layer()]++;


}

// ----------------------------------------------------------------------------
Bool_t StEEmcGenericClusterMaker::match(const StEEmcCluster &c1, const StEEmcCluster &c2) const
{  
  return c1.tower(0).isNeighbor( c2.tower(0) );
}

Bool_t StEEmcGenericClusterMaker::match(const StEEmcCluster &c1, const StEEmcSmdCluster &c2) const
{
  StEEmcTower seed=c1.tower(0);
  if ( seed.sector() != c2.sector() ) return false;

  Int_t min[2],max[2];
  mESmdMap -> getRangeU( seed.sector(), seed.subsector(), seed.etabin(), min[0], max[0] );
  mESmdMap -> getRangeV( seed.sector(), seed.subsector(), seed.etabin(), min[1], max[1] );

  Int_t   plane = c2.plane();
  Float_t mean  = c2.mean();
  Int_t   mid   = (max[plane]+min[plane])/2;
  Float_t del   = mean - mid;
   
  return TMath::Abs(del)<mSmdMatchRange;
}

Bool_t StEEmcGenericClusterMaker::match(const StEEmcSmdCluster &c1, const StEEmcSmdCluster &c2) const
{

  Bool_t myMatch = false;
  if ( ( c1.energy() > 0.8 * c2.energy()   &&
	 c1.energy() < 1.2 * c2.energy() ) ||
       ( c2.energy() > 0.8 * c1.energy()   &&
	 c2.energy() < 1.2 * c1.energy() ) ) myMatch = true;

  if ( !myMatch ) return false;

  for ( Int_t sec=0;sec<12;sec++ )
    for ( UInt_t i=0; i < mTowerClusters[sec][0].size(); i++ )
      {

	const StEEmcCluster &c=mTowerClusters[sec][0][i];
	if ( match ( c, c1 ) && match( c, c2 ) ) return true;

      }

  return false;

}

Bool_t StEEmcGenericClusterMaker::match(const StEEmcCluster &cluster, const StMuTrack *track) const
{

  const StPhysicalHelixD helix = track -> outerHelix();
  const Float_t match_at_z[]={ 
    kEEmcZSMD,  // match tower clusters at smd
    kEEmcZPRE1, // match pre1 at pre1
    kEEmcZPRE2, // match pre2 at pre2
    kEEmcZPOST  // match post at post
  };

  Int_t layer = cluster.tower(0).layer();

  TVector3 r(0.,0.,-1.);
  if ( extrapolateToZ( helix, match_at_z[ layer ], r ) )
    {
      TVector3 position = cluster.position();
      Float_t dphi = fmod( position.Phi() - r.Phi(), TMath::TwoPi() );
      Float_t deta =       position.Eta() - r.Eta();
      Float_t dr=TMath::Sqrt(deta*deta+dphi*dphi);      

      if ( dr < mClusterTrackSeparation[layer] ) return true;

    }

  return false;
  
}

Bool_t StEEmcGenericClusterMaker::extrapolateToZ( const StPhysicalHelixD &helix, const double   z, TVector3 &r) const
{
  const double kMinDipAngle   = 1.0e-13;
  double             dipAng = helix.dipAngle();
  double             z0     = helix.origin().z();
  if(dipAng<kMinDipAngle) 
    return kFALSE;
  double s  = ( z - z0 ) / sin(dipAng)  ;
  StThreeVectorD hit = helix.at(s);
  r.SetXYZ(hit.x(),hit.y(),hit.z());
  return   kTRUE;   
}


Bool_t StEEmcGenericClusterMaker::matchBackgroundTrack(const StEEmcCluster &cluster, const StMuTrack *track ) const
{

  //
  // First and last point on the background tracks are stored as the
  // origin points of the inner and outer helix.
  //
  StPhysicalHelixD outer = track -> outerHelix();
  StPhysicalHelixD inner = track -> helix();
  StThreeVectorD p1 = inner.origin();
  StThreeVectorD p2 = outer.origin();

  Int_t layer = cluster.tower(0).layer();

  Double_t z1 = p1.z();
  Double_t z2 = p2.z();

  if ( z2 <= z1 ) return false;
  
  const Float_t match_at_z[]={ 
    kEEmcZSMD,  // match tower clusters at smd
    kEEmcZPRE1, // match pre1 at pre1
    kEEmcZPRE2, // match pre2 at pre2
    kEEmcZPOST  // match post at post
  };

  Double_t zmatch = match_at_z[ layer ];

  //
  // Compute the intersection point with the track (assume straight line)
  // extrapolated to the layer where we want to make the match
  //
  Double_t scale = zmatch - z1;
  scale /= z2 - z1;

  StThreeVectorD myposition = p1 + scale * (p2 - p1);

  // Compare position 
  TVector3 r(myposition.x(),myposition.y(),myposition.z());
  TVector3 position = cluster.position();

  Float_t dphi = fmod( position.Phi() - r.Phi(), TMath::TwoPi() );
  Float_t deta =       position.Eta() - r.Eta();
  Float_t dr=TMath::Sqrt(deta*deta+dphi*dphi);      
  
  if ( dr < mClusterTrackSeparation[layer] ) return true;
    
  return false;
 
}


// ----------------------------------------------------------------------------
Int_t StEEmcGenericClusterMaker::numberOfMatchingSmdClusters(const StEEmcCluster &cluster, Int_t plane ) const
{
  const EEmatch &matches = clusterMatch( cluster );
  return (plane==0)? (Int_t)matches.smdu.size() : (Int_t)matches.smdv.size();
}

StEEmcSmdCluster &StEEmcGenericClusterMaker::matchingSmdCluster (const StEEmcCluster &cluster, Int_t plane, Int_t index )
{
   EEmatch &matches = clusterMatch( cluster ); 
   return (plane==0)? matches.smdu[index] : matches.smdv[index];
}

const StEEmcSmdCluster &StEEmcGenericClusterMaker::matchingSmdCluster (const StEEmcCluster &cluster, Int_t plane, Int_t index ) const
{
   const EEmatch &matches = clusterMatch( cluster ); 
   return (plane==0)? matches.smdu[index] : matches.smdv[index];
}


