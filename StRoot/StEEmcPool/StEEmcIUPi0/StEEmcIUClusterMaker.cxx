/**
 * \class StEEmcIUClusterMaker
 * \brief A cluster maker for the EEMC.
 *
 * This class produces clusters of EEMC towers, pre- and postshower elements
 * and smd strips.
 *
 * \author Weihong He
 * $Date: 2014/06/24 21:11:20 $
 * $Revision: 1.2 $
 *
 * \section steemcclustermaker_towers Tower, pre- and postshower algorithm
 *
 * The clustering-algorithm used for tower clusters (as well as preshower 
 * and postshower clusters) is the 
 * <a href="http://www.star.bnl.gov/STAR/eemc/simulations/MineSweeper/">Minesweeper</a>
 * algorithm originally developed by Jan Balewski. 
 *
 * \section steemcclustermaker_smd The SMD Algorithm
 *
 * The SMD clustering algorithm starts by finding all strips which 
 * exceed a user-specified seed threshold.  These strips are sorted
 * by energy, and we work from the most energetic seed to the least
 * energetic seed.
 *
 * Strips adjacent to the smd seed are added to the cluster and removed
 * from the pool of seed strips.  The number of adjacent strips added
 * is specified by the user in the setMaxExtent(max) method and the default strip number is set to 3 to both sides around a seed strip.
 *
 * Once a cluster is identified, the seed threshold is raised in the
 * vicinity of that cluster, including a special floor setting described below.  For details, see buildSmdClusters().
 *
 * \section steemcclustermaker_smd_cuts SMD Cluster Cuts
 *
 * In order to form a cluster, we require a minimum of three smd strips.
 * Fewer strips suggests a MIP, although low-energy EM showers (<~ 1 GeV)
 * can deposit energy in only two consecutive smd strips.
 *
 * The user may also specify the suppress() option for the cluster
 * maker.  When this option is used, the two smd strips adjacent to
 * the cluster are removed from the pool of smd seed strips.  This
 * reduces the effects of false cluster splitting, i.e. where a 
 * fluctuation in the size of the EM shower combined with the fixed
 * size of the cluster leads to the formation of a false smd cluster.
 *
 * For Monte Carlo data, you need to switch on #define MonteCarloS
 * For real and pythia data, you need to switch off #define MonteCarloS
 */

#include "StEEmcIUClusterMaker.h"

#include <algorithm>
#include <iostream>

#include "TMath.h"

/* StEvent stuff */
#include "StEvent/StEvent.h"
#include "StEvent/StEmcCollection.h"
#include "StEvent/StEmcDetector.h"
#include "StEvent/StEmcModule.h"
#include "StEvent/StEmcClusterCollection.h"
#include "StEvent/StEmcCluster.h"

#define DEBUG 0
//#define MonteCarloS

ClassImp(StEEmcIUClusterMaker);

// ----------------------------------------------------------------------------
StEEmcIUClusterMaker::StEEmcIUClusterMaker(const Char_t *name):StMaker(name)
{

  /// by default, do not populate the StEmcClusterCollection
  mFillStEvent = 0;

  /// default, do not supress seeds adjacent to clusters 
  mSuppress = 0;

  mLoose=false;
  mSkip=true;
  //Meanclust=0;
  /// each cluster (tower, pre/post, smd) are assigned a unique id in
  /// the order in which they are collected
  mClusterId = 0;

  /// Set default seed energies
  const Float_t eseeds[] = { 0.6, 1.0/1000, 1.0/1000, 1.0/1000., 0.3/1000., 0.3/1000. };
  for ( Int_t i = 0; i < 6; i++ ) seedEnergy( eseeds[i], i );

  mMaxExtent = 3; 
  mSeedFloor = 2.0; 

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

  StEEmcIUClusterVec_t t;
  std::vector< StEEmcIUClusterVec_t > layers;
  for ( Int_t i = 0; i < 4; i++ ) layers.push_back(t);
  for ( Int_t i = 0; i < 12; i++ ) mTowerClusters.push_back(layers);

  StEEmcIUSmdClusterVec_t s;
  std::vector< StEEmcIUSmdClusterVec_t > planes;
  planes.push_back(s);
  planes.push_back(s);
  for ( Int_t i = 0; i < 12; i++ ) mSmdClusters.push_back(planes);
  for ( Int_t i = 0; i < 12; i++ ) TmSmdClusters.push_back(planes);

  StEEmcTowerVec_t tow;
  std::vector< StEEmcTowerVec_t > lay;
  for ( Int_t i = 0; i < 4; i++ ) lay.push_back(tow);
  for ( Int_t i = 0; i < 12; i++ ) mSeedTowers.push_back(lay);

  mEEtow=new EEmcGeomSimple();
  mEEsmd=EEmcSmdGeom::instance();
  mEEmap=EEmcSmdMap::instance();

  mMinStrips=3;
  
}

// ----------------------------------------------------------------------------
Int_t StEEmcIUClusterMaker::Init()
{
  mEEanalysis=(StEEmcA2EMaker*)GetMaker(mAnalysisName);
  assert(mEEanalysis);
  clusize=new TH1D("clustersize", "smd cluster size ",10,0,10);
  tclusize=new TH1D("tclustersize", "smd cluster size ",10,0,10);
  cludis=new TH1D("cludis", "cluster distance ",20,0,20);
  return StMaker::Init();
}

// ----------------------------------------------------------------------------
Int_t StEEmcIUClusterMaker::Make()
{

  /// Warn if we couldn't build tower clusters
  if ( !buildTowerClusters() ) return kStWarn;

  /// Warn if we couldn't build SMD clusters
  if ( !buildSmdClusters() ) return kStWarn; 
  //fillStEvent();

  /// If the option to fill StEvent is selected, do it
  if ( mFillStEvent ) fillStEvent();

  /// Verify that StEvent is a valid copy
  if ( mFillStEvent ) 
    if ( !verifyStEvent() ) Warning("Make","StEvent not properly copied");
  //print();  


  return kStOK;
}

// ----------------------------------------------------------------------------
void StEEmcIUClusterMaker::Clear( Option_t *opts )
{

  /// Clear cluster storage
  for ( Int_t sector=0; sector<12; sector++ ) {
    for ( Int_t layer=0; layer<4; layer++ )
      mTowerClusters[sector][layer].clear();    
    for ( Int_t plane=0; plane<2; plane++ )
      {
	mSmdClusters[sector][plane].clear();
	TmSmdClusters[sector][plane].clear();
      }
  }

  for ( Int_t i=0;i<6;i++ ) 
    {
      mNumberOfClusters[i]=0;
      TmNumberOfClusters[i]=0;
    }
  mClusterId = 0;

  return;
}


// ----------------------------------------------------------------------------
Bool_t StEEmcIUClusterMaker::buildTowerClusters()
{

  // This part is not important in the reconstruction of pi0. It only provides some fundamental checks and basic idea about the tower clusters.
  /// Current algo only populates tower clusters, using Jan's 
  /// "minesweeper" algorithm (apologies to Bill Gates).
  //static const Int_t layer=0;


  /// Loop over layers to go here...  maybe above init of weights

  eeen=0;
  ep1=0;
  ep2=0;
  ep3=0;
  for ( Int_t layer=0;layer<4;layer++)
  {

    /// Weights for dividing energy of towers among 
    /// the tower-only clusters, like Jan's algorithm.
    Float_t weights[720]; for ( Int_t i=0;i<720;i++ ) weights[i]=0.;

    /// Temp storage for clusters
    StEEmcIUClusterVec_t myClusters;


    /// Get list of towers
    StEEmcTowerVec_t towers = mEEanalysis -> towers( layer );

    

    /// Order according to energy (STL sort is fast, N log(N))
    std::sort(towers.begin(),towers.end());
    /// Order descending in energy
    std::reverse(towers.begin(),towers.end());
   

    /// Find the last tower which exceeds see threshold (actually,
    /// "last" will be the first tower below the seed threshold).
    StEEmcTowerVec_t::iterator last = towers.begin();
    while ( last != towers.end() ) {
      if(layer==0){
	eeen+=(*last).energy();
      }
      if(layer==1){
	ep1+=(*last).energy();
      }
      if(layer==2){
	ep2+=(*last).energy();
      }
      if(layer==3){
	ep3+=(*last).energy();
      }
      if ( (*last).energy() < mSeedEnergy[layer] ) break;
      /// store seed towers by layer, sector <<< this breaks, 
      /// gives an "abort", which I assume means that we're 
      /// trying to push a tower into a nonexistant vector...
      /// did I maybe get the order wrong???
      //$$$      mSeedTowers[layer][ (*last).sector() ].push_back((*last));
#if DEBUG
      std::cout << "-- Seed tower ----------------------" << std::endl;
      (*last).print();
#endif
      last++;
    }
    //std::cout<<"tower energy total = "<< eeen<< std::endl;
    
    StEEmcTowerVec_t::iterator iter = towers.begin();
    while ( iter != towers.end() ) {

      /// We have run out of seeds here
      if ( iter==last )	break;
      

      /// This seed tower is adjacent to another
      /// seed tower and will be clustered with
      /// that one.  No incrementation for now.
      /// (Future, we may allow this to be a seed
      /// if there's an SMD response beneath it,
      /// then use tower-sharing shape to divide...)
      if ( weights[ (*iter).index() ] > 0. ) {
	iter++;
	continue;
      }

      /// loop over neighboring towers and increment weight
      /// for this tower with the energy of the seed tower
      for ( Int_t in=0; in < (*iter).numberOfNeighbors(); in++ ) {
	StEEmcTower t=(*iter).neighbor(in);
	weights[ t.index() ] += (*iter).energy();
      }

     
      iter++;

    }// loop over towers to init weights


    /// Loop over towers again
    iter=towers.begin();
    while ( iter != towers.end() ) {

      /// We've run out of seeds
      if ( iter==last ) break;

      /// If the weight for this tower has been incremented,
      /// then it's adjacent to a more energetic tower and
      /// already is clustered with it.
      if ( weights[ (*iter).index() ] > 0. ) {
	iter++;
	continue;
      }

      StEEmcIUCluster cluster;
      StEEmcTower seed=(*iter);
#if DEBUG
      std::cout << "--- Clustering ----------------" << std::endl;
      seed.print();
#endif

      TVector3 momentum;
      cluster.add(seed,1.0);
      UInt_t sec,sub,eta;
      sec=(UInt_t)seed.sector(); 
      sub=(UInt_t)seed.subsector(); 
      eta=(UInt_t)seed.etabin();
      TVector3 d=mEEtow->getTowerCenter(sec,sub,eta).Unit();
      momentum += ( seed.energy() * d );

      for ( Int_t in=0; in<seed.numberOfNeighbors(); in++ ) {
	StEEmcTower t=seed.neighbor(in);
	sec=(UInt_t)t.sector(); 
	sub=(UInt_t)t.subsector(); 
	eta=(UInt_t)t.etabin();
	d=mEEtow->getTowerCenter(sec,sub,eta).Unit();
	Float_t weight = seed.energy() / weights[ t.index() ];
	momentum += ( t.energy() * d * weight );
	cluster.add(t, weight);
#if DEBUG
	std::cout << "adding " << t.name() << " E=" << t.energy() << " W=" << weight << std::endl;
#endif
      }


      /// set momentum and a unique key
      cluster.momentum( momentum );
      cluster.key( mClusterId++ );
#if DEBUG
      cluster.print();
#endif

      mTowerClusters[ seed.sector() ][ layer ].push_back( cluster );
      mNumberOfClusters[layer]++;
     
      iter++;

    }
          
  }// end of eventual loop over layers
  

  return true;

}

// ----------------------------------------------------------------------------
Bool_t StEEmcIUClusterMaker::buildSmdClusters()
{

  //this part is essential in the reconstruction of pi0 in the EEMC.

  Int_t max_extent = mMaxExtent;
  countUseed=0;
  countVseed=0;
  uswidth=0;
  vswidth=0;
  uamp=0;
  vamp=0;
  esmdu=0;
  esmdv=0;


  /// Loop over all sectors
  for ( Int_t sector=0; sector<12; sector++ ) {
    /// Loop over all planes

    for ( Int_t plane=0; plane<2; plane++ ) {
      float ampe=0;
        /// Try to cut down on noice around identified gammas
        Float_t floor[288]; for ( Int_t i=0; i<288; i++ ) floor[i]=0.;

	/// Energy of all strips in plane
	Float_t energy[288]; for ( Int_t i=0; i<288; i++ ) energy[i]=0.;
	//averaging energies
	//Float_t newenergy[288]; for ( Int_t i=0; i<288; i++ ) newenergy[i]=0.;
  	/// Get list of strips
    	StEEmcStripVec_t strips=mEEanalysis->strips(sector,plane);
	StEEmcStripVec_t seeds;
      	/// Sort by energy
	std::sort(strips.begin(),strips.end());
  	/// Order in descending energy
    	std::reverse(strips.begin(),strips.end());

	/// Copy energy of all hit strips into an array
	StEEmcStripVec_t::iterator istrip=strips.begin();

	while ( istrip != strips.end() ) {
	  if ( (*istrip).stat()||(*istrip).fail() ) {
	    istrip++;
	    continue;
	  }
	  ampe=(*(strips.begin())).energy();
	  energy[ (*istrip).index() ] = (*istrip).energy();
	  //cout<<"energy="<<(*istrip).energy()<<endl;
	  
	  if((*istrip).energy()>=0)
	    {
	      //fprintf(fout2,"data%02d%d %d  %f\n",sector,plane,(*istrip).index(),(*istrip).energy());
	      if(plane==0)
	      {
		esmdu+=(*istrip).energy();
	        uswidth++;
	        uamp=ampe;
	      }
	      if(plane==1)
	      {
		esmdv+=(*istrip).energy();
	        vswidth++;
	        vamp=ampe;
	      }
	    }
	  istrip++;
	}

	//averaging algo for strip energies. We decide not using the averaging algo for 2006 long pp data after analyses. But the algo is still kept here for future user reference.
	//istrip=strips.begin();
	//for(int i=2;i<=283;i++){
	//newenergy[i]=energy[i-1]*0.25+energy[i]*0.5+energy[i+1]*0.25;
	//}
	//while ( istrip != strips.end() ) {
	//if ( (*istrip).stat()||(*istrip).fail() ) {
	//istrip++;
	//continue;
	//}
	//(*istrip).energy(newenergy[(*istrip).index()]);
	  
	//istrip++;
	//}
	//end of averaging algo

	/// We'll flag each seed strip
	std::vector<Bool_t> seed_flags( strips.size(), false );


	/// Iterate over strips and find seeds
	Int_t nstrip=0;
	Int_t nseeds=0;
	istrip=strips.begin();
	while ( istrip!=strips.end() ) {

	  Int_t index=(*istrip).index();
	  Float_t eseed=(*istrip).energy();
	  
	  /// We always ignore the corners of the planes
	  if ( index <= 3 || index >= 283 ) {
	    istrip++;
	    nstrip++;
	    continue;
	  }

	  /// Verify that this is a good strip
	  if ( mSkip )
	    if ( (*istrip).fail() ) {
	      istrip++;
	      nstrip++;
	      continue;
	    }


	  /// Verify that this strip exceeds the floor by the
	  /// specified seed threshold.  If not, punt
	  if ( eseed < (mSeedFloor*floor[ index ] + mSeedEnergy[4+plane]) ) {
	    istrip++;
	    nstrip++;
	    continue;
	  }
	  //if(energy[index+1]<floor[index+1] && energy[index-1]<floor[index-1]){
	  //istrip++;
	  //nstrip++;
	  //continue;
	  //}
	  /// Flag this as a seed strip
	  //cout<<"succeed!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1"<<endl;
	  seeds.push_back( (*istrip) );
	  if(plane==0){countUseed++;}
	  if(plane==1){countVseed++;}
	  nstrip++;
	  nseeds++;

	  // Now we want to set up special floor values for the SMD shape based on the real data fluctuation. The floor setting, which was basically applied to minimize fluctuation around a used seed strip, is now re-set for real data. Based on the fluctuatin feature we found from real data analyses, the second plane of SMD layers always suppress twice fluctuations than the first plane by using two parameters floor_para1 and floor_para2 properly. This part works specifically for real data and pythia sample. For Monte-Carlo sample, we can use the same floor setting for both SMD planes because we don't find fluctuation from MC sample.
	  float floor_para1=0.0;
	  float floor_para2=0.0;
	  if(sector==0||sector==3||sector==6||sector==9)
	    {
	      if(plane==0)
		{
		  floor_para1=0.4;
		  floor_para2=0.2;
		}
	      else{
		floor_para1=0.2;
		floor_para2=0.1;
	      }
	    }
	  if(sector==2 || sector ==1 || sector ==5 || sector==4 || sector==8 || sector==7 || sector==11 || sector==10)
	    {
	      if(plane==1)
		{
		  floor_para1=0.4;
		  floor_para2=0.2;
		}
	      else{
		floor_para1=0.2;
		floor_para2=0.1;
	      }
	    }
#ifdef MonteCarloS
	  floor_para1=0.2;
	  floor_para2=0.1;
#endif
	  ///
	  /// LOOSE_CUTS is an adhoc shower shape, where we try to 
	  /// suppress clusters if they are likely a fluctuation
	  /// in the shower profile of an EM shower
	  ///
#ifndef LOOSE_CUTS	  
	  /// Set a "floor" around this strip	  
	  for ( Int_t i=0; i < 288; i++ ) 
	    {
	      Int_t dx=TMath::Abs(index-i);
	      /// Within +/- 2 strips, find no other seeds
	      if(dx<3) {floor[i]+=1.0*eseed;}
	      /// Within +/- 4 strips, Floor is 20 or 40% of seed
	      if(dx>=3&&dx<5){floor[i]+=floor_para1*eseed;}
	      /// Within +/- 10 strips, Floor i 10 or 20% of seed
	      if(dx>=5&&dx<11){floor[i]+=floor_para2*eseed;}
	      /// Within +/- 20 strips, floor is 5% of seed
	      if(dx>=11&&dx<21){floor[i]+=0.05*eseed;}
	    }
#else
	  /// Set a "floor" around this strip	  
	  for ( Int_t i=0; i < 288; i++ ) {

	    Int_t dx=TMath::Abs(index-i);

	    /// Within +/- 6 strips, Floor is  5% of seed
	    if ( dx<7) 
	      if ( 0.05 * eseed > floor[i] ) floor[i]+= 0.05 * eseed;

	  }
#endif
	  istrip++;

	}// found all seed strips in that plane of that sector

	/// Now for each seed strip, add all contiguous, 
	/// adjacent SMD strips out to +/- 3 strips on 
	/// either side.  Continuity is broken if strip
	/// falls below Nsigma threshold and is not marked
	/// as dead.
	Bool_t owned[288]; for (Int_t i=0;i<288;i++) owned[i]=false;
	Bool_t xseed[288]; for (Int_t i=0;i<288;i++) xseed[i]=false;


	StEEmcStripVec_t::iterator iseed=seeds.begin();
	while ( iseed != seeds.end() ) {

	  int flcount=0;
	  /// Verify that this seed is not owned by another cluster
	  Int_t index=(*iseed).index();
	  if ( owned[index] || (mSuppress&&xseed[index]) ) {
	    iseed++;
	    continue;
	  }

	  /// This seed is now the property of a cluster
	  owned[index]=true;
	  xseed[index]=true;
	  /// Create our cluster
	  StEEmcIUSmdCluster cluster;
	  /// And give it the strip
	  cluster.add( (*iseed) );
	  flcount++;

	  Int_t ind_max = TMath::Min(287,index+max_extent); 
	  Int_t ind_min = TMath::Max(0,  index-max_extent); 

	  /// Now go +/- 3 strips on either side and add them
	  /// to the cluster, respecting continuity etc...
	  for ( Int_t i=index+1; i<=ind_max; i++ ) {
	    /// Get the strip
	    StEEmcStrip strip=mEEanalysis->strip(sector,plane,i);

	    if ( strip.energy()<=0. ) strip.energy(0.0);

	    /// Mark this strip as owned
	    owned[ strip.index() ] = true;
	    xseed[ strip.index() ] = true;
	    /// Add to cluster
	    cluster.add(strip);
	    if(strip.energy()>0) flcount++;	
	    
	  }
	  for ( Int_t i=index-1; i>=ind_min; i-- ) {
	    /// Get the strip
	    StEEmcStrip strip=mEEanalysis->strip(sector,plane,i);
	    //strip.energy(newenergy[i]);

	    if (strip.energy()<=0.) strip.energy(0.0);

	    /// Mark this strip as owned
	    owned[ strip.index() ] = true;
	    xseed[ strip.index() ] = true;
	    /// Add to cluster
	    cluster.add(strip);
	    if(strip.energy()>0) flcount++;	  
  	    
	  }
	 
	  /// Push the cluster into our storage vector
	  
	  if ( cluster.size() >= mMinStrips && flcount>=3 ) {
	    tclusize->Fill(flcount);
	   
	    TmSmdClusters[ sector ][ plane ].push_back(cluster);
	    TmNumberOfClusters[4+plane]++;
	   
            // disallow strips on either side of the cluster
	    // from forming seeds
	    Int_t ns=cluster.numberOfStrips();
	    Int_t left=999,right=-999;
       	    for ( Int_t is=0;is<ns;is++ )
	    {
		StEEmcStrip s=cluster.strip(is);
		//printf("strip.energy=%f\n",s.energy()); 
		if ( s.index()<left ) left=s.index();
		if ( s.index()>right ) right=s.index();
	    }

	    for ( Int_t ii=0;ii<=mSuppress;ii++ )
	      {
		if ( left-ii>=0   ) xseed[left-ii] = true;
		if ( right+ii<288 ) xseed[right+ii]= true;
	      }

	  }
	  

	  iseed++;
	}//loop over seeds


    }}// loop over planes/sectors

  //overlapped SMD cluster area divided here, so that we get better resolution.
 
  for ( Int_t sector=0; sector<12; sector++ ) 
    {
      for(Int_t plane=0;plane<=1;plane++)
	{

	  Int_t para1[288];for (Int_t i=0;i<288;i++) para1[i]=0;
	  for ( UInt_t fclust=0; fclust<TmSmdClusters[sector][plane].size(); fclust++ ) 
	    {
	      StEEmcIUSmdCluster cllu = (TmSmdClusters[sector][plane])[fclust];
	      Int_t fnums=cllu.numberOfStrips();
	      for ( Int_t fis=0;fis<fnums;fis++ )
		{
		  StEEmcStrip stri1=cllu.strip(fis);
		  
		  int fstindex=stri1.index();
		  para1[fstindex]++;
		  
		}
	    }
	  int flag2;
	  float EI,EIII;
	  Int_t Narr=TmSmdClusters[sector][plane].size();
	  int flag3[288];for (Int_t i=0;i<Narr;i++) flag3[i]=0;
	  Int_t flag1[288];for (Int_t i=0;i<288;i++) flag1[i]=0;		
	  for ( UInt_t iclust=0; iclust<TmSmdClusters[sector][plane].size(); iclust++ ) 
	    {
	      flag2=0;
	      
	      
	      flag3[iclust]++;
	      StEEmcIUSmdCluster cl1 = (TmSmdClusters[sector][plane])[iclust];
	      Int_t nums1=cl1.numberOfStrips();
	      UInt_t oindex1=cl1.strip(0).index();

	      for ( UInt_t jclust=0; jclust<TmSmdClusters[sector][plane].size(); jclust++ ) 
		{
		  StEEmcIUSmdCluster cl2 = (TmSmdClusters[sector][plane])[jclust];
		  Int_t nums2=cl2.numberOfStrips();
		  UInt_t oindex2=cl2.strip(0).index();
		  if(oindex1==oindex2)
		    {continue;}
		  int seedds=abs(int(oindex1-oindex2));
		  cludis->Fill(seedds);
		  if(seedds>2*max_extent)
		    {continue;}
		  flag2++;
		  flag3[jclust]++;
		  //printf("flag2=%d\n",flag2);
		  if(flag3[iclust]>=2 && flag3[jclust]>=2)
		    {continue;}
		  EI=0;
		  EIII=0;
		  if(oindex1<oindex2)
		    {
		      for ( Int_t is=0;is<nums1;is++ )
			{
			  StEEmcStrip stri=cl1.strip(is); 
			  UInt_t sindex=stri.index();
			  if(sindex<=oindex1)
			    {
			      EI=EI+stri.energy();
			    }
			}
		      for ( Int_t ist=0;ist<nums2;ist++ )
			{
			  StEEmcStrip strit=cl2.strip(ist); 
			  UInt_t tindex=strit.index();
			  if(tindex>=oindex2)
			    {
			      EIII=EIII+strit.energy();
			    }
			}
		    }
		  else
		    {
		      for ( Int_t is=0;is<nums1;is++ )
			{
			  StEEmcStrip stri=cl1.strip(is); 
			  UInt_t sindex=stri.index();
			  if(sindex>=oindex1)
			    {
			      EIII=EIII+stri.energy();
			    }
			}
		      for ( Int_t ist=0;ist<nums2;ist++ )
			{
			  StEEmcStrip strit=cl2.strip(ist); 
			  UInt_t tindex=strit.index();
			  if(tindex<=oindex2)
			    {
			      EI=EI+strit.energy();
			    }
			}
		   
		    }
		  
		  StEEmcIUSmdCluster tclust1;
		  StEEmcIUSmdCluster tclust2;


		  if(oindex1<oindex2)
		    {
		      for ( Int_t is=0;is<nums1;is++ )
			{
			  StEEmcStrip stri=cl1.strip(is); 
			  UInt_t sindex=stri.index();
			  if(para1[sindex]==2 )
			    {
			      float tpe1=stri.energy()*EI/(EI+EIII);
			      stri.energy(tpe1);
			      tclust1.add(stri);
			      flag1[sindex]++;
			     
			    }
			  if(para1[sindex]==1 )
			    {
			      tclust1.add(stri);
			      flag1[sindex]++;
			    
			    }
			}
		      for ( Int_t ist=0;ist<nums2;ist++ )
			{
			  StEEmcStrip strit=cl2.strip(ist); 
			  UInt_t tindex=strit.index();
			  if(para1[tindex]==2 )
			    {
			      float tpe2=strit.energy()*EIII/(EI+EIII);
			      strit.energy(tpe2);
			      tclust2.add(strit);
			      flag1[tindex]++;
			      
			    }
			  if(para1[tindex]==1 )
			    {
			      tclust2.add(strit);
			      flag1[tindex]++;
			      
			    }
			}
		    } //end of if(oindex1<oindex2)

		  if(oindex1>oindex2)
		    {
		      for ( Int_t is=0;is<nums1;is++ )
			{
			  StEEmcStrip stri=cl1.strip(is); 
			  UInt_t sindex=stri.index();
			  if(para1[sindex]==2 )
			    {
			      float tpe2=stri.energy()*EIII/(EI+EIII);
			      stri.energy(tpe2);
			      tclust2.add(stri);
			      flag1[sindex]++;
			      
			    }
			  if(para1[sindex]==1 )
			    {
			      tclust2.add(stri);
			      flag1[sindex]++;
			      
			    }
			}
		      for ( Int_t ist=0;ist<nums2;ist++ )
			{
			  StEEmcStrip strit=cl2.strip(ist); 
			  UInt_t tindex=strit.index();
			  if(para1[tindex]==2 )
			    {
			      float tpe1=strit.energy()*EI/(EI+EIII);
			      strit.energy(tpe1);
			      tclust1.add(strit);
			      flag1[tindex]++;
			      
			    }
			  if(para1[tindex]==1 )
			    {
			      tclust1.add(strit);
			      flag1[tindex]++;
			      
			    }
			}
		    } //end of if(oindex1>oindex2)

		  // read the new clusters(originally overlapped) into storage
		  int seedind1=tclust1.strip(0).index();
		  int seedind2=tclust2.strip(0).index();
		  if ( flag1[seedind1]<2) 
		    {
		      tclust1.key( mClusterId++ );
		      mSmdClusters[ sector ][ plane ].push_back(tclust1); 
		      clusize->Fill(tclust1.size());
		      mNumberOfClusters[4+plane]++;
		    }
		  if ( flag1[seedind2]<2 ) 
		    {
		      tclust2.key( mClusterId++ );
		      mSmdClusters[ sector ][ plane ].push_back(tclust2); 
		      clusize->Fill(tclust2.size());
		      mNumberOfClusters[4+plane]++;
		    }

		}//end of first round looping clusters

	      if(flag2==0)
		{	  
		      
		      cl1.key( mClusterId++ );
		      mSmdClusters[ sector ][ plane ].push_back(cl1); 
		      clusize->Fill(cl1.size());	      
		      mNumberOfClusters[4+plane]++;
		    
		}
	    }//endo of second round looping clusters




	}//end of plane
      
    }//end of sector


  return true;

}



// ----------------------------------------------------------------------------

void StEEmcIUClusterMaker::fillStEvent()
{

  StEvent *stevent=(StEvent*)GetInputDS("StEvent");
  if ( !stevent ) {
    Warning("fillStEvent","called, but no StEvent to be found");
    return;
  }

  std::cout << "Adding tower clusters to StEvent at " << stevent << std::endl;

  ///
  /// First the eemc tower clusters  
  ///
  StEmcDetector *detector=stevent->emcCollection()->detector(kEndcapEmcTowerId);
  if ( !detector )
    {
      Warning("fillStEvent","detector == NULL, MAJOR StEvent problem, continuing");
      return;
    }
  

  ///
  /// If we found clusters, create a cluster collection for
  /// this detector.  Otherwise, nullify the cluster collection,
  /// otherwise we run into an assert somewhere out in "STAR" 
  /// land... or maybe not,... no comments in barrel virtual 
  /// finder...
  ///
  
  if ( mNumberOfClusters[0] > 0 )
    {


      ///
      /// Obtain the cluster collection from the detector, and
      /// prepare to fill it.  If it doesn't exist, create it.
      ///
      StEmcClusterCollection *collect = detector -> cluster();
      if ( !collect ) 
	{
	  //Warning("fillStEvent","StEmcClusterCollection (towers) was NULL, so I'm creating one.");
	  collect = new StEmcClusterCollection();
	  detector->setCluster( collect );
	}

      assert(collect);
      collect->setDetector( kEndcapEmcTowerId );
      collect->setClusterFinderId( 123 );
      collect->setClusterFinderParamVersion( 123 );//123 or 321?
  
      /// Loop over all EEMC sectors and fill collection
      for ( Int_t isector=0; isector<12; isector++ )
	{

	  /// Loop over all clusters in this sector
	  for ( UInt_t iclust=0; iclust<mTowerClusters[isector][0].size(); iclust++ ) 
	    {

	      StEEmcIUCluster cl=(mTowerClusters[isector][0])[iclust]; 
	      
	      /// for some reason, this code doesn't work when I
	      /// call StEEmcIUCluster::stemc(), but works just fine
	      /// here....
	      StEmcCluster *emccluster = new StEmcCluster();
	      emccluster->setEta( cl.momentum().Eta() );
	      emccluster->setPhi( cl.momentum().Phi() );
	      emccluster->setSigmaEta(-1.);
	      emccluster->setSigmaPhi(-1.);
	      emccluster->setEnergy( cl.energy() );
	      emccluster->SetUniqueID( cl.key() );
#if 1
	      for ( Int_t i=0; i< cl.numberOfTowers(); i++ ) 
		{
		  StEmcRawHit *hit=cl.tower(i).stemc();
		  assert( hit );         
		  emccluster->addHit( hit );
		}
#endif
	    
	      collect->addCluster( emccluster );

	      
	      /// association between StEmcCluster and my class
	      mEtoEE[ emccluster ] = cl;
	      cl.stemc( emccluster );

	    }
	  
	}

    }

  else // if ( mNumberOfClusters[0] == 0 )
    {

      detector->setCluster( NULL );

    }




  /// now set the collection in the detector
  //  detector->setCluster( collect );

  ///
  /// Next the pre and postshower clusters
  ///
  detector=stevent->emcCollection()->detector(kEndcapEmcPreShowerId);
  if ( !detector )
    {
      Warning("fillStEvent","detector == NULL for pre/post, no clusters for you");
    }
  else if ( mNumberOfClusters[1] > 0 ||
	    mNumberOfClusters[2] > 0 || 
	    mNumberOfClusters[3] > 0 )
    {

      StEmcClusterCollection *pqr = detector -> cluster();
      if ( !pqr )
	{
	  //Warning("fillStEvent","StEmcClusterCollection (pre/post) was NULL, so I'm creating one.");
	  pqr = new StEmcClusterCollection();
	  detector->setCluster( pqr );
	}
      assert(pqr); 
      pqr -> setDetector( kEndcapEmcPreShowerId );
      pqr -> setClusterFinderId( 123 );
      pqr -> setClusterFinderParamVersion( 321 );
  
      /// Loop over all EEMC sectors and fill collection
      for ( Int_t isector=0; isector<12; isector++ )
    
      /// Loop over PQR
      for ( Int_t ilayer=1; ilayer<4; ilayer++ ) 
    
      /// Loop over all clusters in this sector //and layer
      for ( UInt_t iclust=0; iclust<mTowerClusters[isector][ilayer].size(); iclust++ ) 
	{

	  StEEmcIUCluster cl=(mTowerClusters[isector][ilayer])[iclust];

	  /// for some reason, this code doesn't work when I
	  /// call StEEmcIUCluster::stemc(), but works just fine
	  /// here....
	  StEmcCluster *emccluster = new StEmcCluster();
	  emccluster->setEta( cl.momentum().Eta() );
	  emccluster->setPhi( cl.momentum().Phi() );
	  emccluster->setSigmaEta(-1.);
	  emccluster->setSigmaPhi(-1.);
	  emccluster->setEnergy( cl.energy() );
	  emccluster->SetUniqueID( cl.key() );
#if 1
	  for ( Int_t i=0; i< cl.numberOfTowers(); i++ ) 
	    {
	      StEmcRawHit *hit=cl.tower(i).stemc();
	      assert( hit );         
	      emccluster->addHit( hit );
	    }
#endif


	  pqr->addCluster( emccluster );

	  mEtoEE[ emccluster ] = cl;
	  cl.stemc( emccluster );

	}

    }
  else // if ( mNumberOfClusters[1, 2 OR 3] == 0 )
    {

      detector->setCluster( NULL );

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
	  Warning("fillStEvent","detector == NULL for smd plane, no clusters for you");
	}
      else if ( mNumberOfClusters[4+iplane] > 0 )
	{
	  
	  StEmcClusterCollection *smdc = detector -> cluster();
	  if ( !smdc ) 
	    {
	      //Warning("fillStEvent","StEmcClusterCollection (smd) was NULL, so I'm creating one.");
	      smdc = new StEmcClusterCollection();
	      detector->setCluster( smdc );
	    }
	  
	  smdc->setDetector( ids[iplane] );
	  smdc->setClusterFinderId( 123 );
	  smdc->setClusterFinderParamVersion( 321 );

	  
	  for ( Int_t isector=0; isector<12; isector++ ) 
	    {
	      
	      for ( UInt_t iclust=0; iclust<mSmdClusters[isector][iplane].size(); iclust++ ) 
		{
		  
		  StEEmcIUSmdCluster cl = (mSmdClusters[isector][iplane])[iclust];
		  

		  StEmcCluster *emccluster = new StEmcCluster();
		  emccluster->setEta( cl.mean() );
		  emccluster->setPhi( (Float_t)iplane );
		  emccluster->setSigmaEta(-1.);
		  emccluster->setSigmaPhi(-1.);
		  emccluster->setEnergy( cl.energy() );
		  emccluster->SetUniqueID( cl.key() );
		  printf("fjaoshjaojgnfaj----------------------------");
		  printf("uniq id=%d\n",cl.key());
		  for ( Int_t i=0; i< cl.numberOfStrips(); i++ ) 
		    {
		      StEmcRawHit *hit=cl.strip(i).stemc();
		      assert( hit );         
		      emccluster->addHit( hit );
		    }		  
		  smdc->addCluster( emccluster );

		  mEtoEEsmd[ emccluster ] = cl;
		  cl.stemc( emccluster );
		  
		}
	  
	      
	    }
	  
	}

      else
	{
	  detector -> setCluster( NULL );
	}



    }


}



// ----------------------------------------------------------------------------
Bool_t StEEmcIUClusterMaker::verifyStEvent()
  {

  /// verify tower clusters
  StEvent *stevent=(StEvent*)GetInputDS("StEvent");
  if ( !stevent ) {
    Warning("verifyStEvent","No StEvent found.");
    return true;
  }
  //StEmcCollection *emccollection=stevent->emcCollection();

  Bool_t go = true;
 
  StEmcDetector *detector=stevent->emcCollection()->detector(kEndcapEmcTowerId);
  if ( !detector )
    {
      Warning("verifyStEvent","detector == NULL for towers");
      return false;
    }

  
  StEmcClusterCollection *cc=detector->cluster();
  if ( cc )
    {


      /// ------------------------------------------------------------------------
      ///
      /// Tower-cluster checksum
      ///
      
      Float_t emc_sum_towers = 0.;
      Float_t eemc_sum_towers = 0.;
  
      StSPtrVecEmcCluster &emcClusters=cc->clusters();
      for ( UInt_t i=0; i<emcClusters.size(); i++ ) 
	{
	  StEmcCluster *cl=emcClusters[i];
	  assert(cl);
	  emc_sum_towers += cl->energy();
	}
      
      for ( Int_t sec=0; sec<12; sec++ ) 
	{
	  
	  for ( Int_t i=0; i<numberOfClusters(sec,0); i++ )
	    eemc_sum_towers += cluster(sec,0,i).energy();
	}
      
      std::cout << "StEEmcIUClusterMaker tower checksum: ";
      if ( emc_sum_towers == eemc_sum_towers ) {
	std::cout << "passed";
      }
      else {
	std::cout << "FAILED"; go=false;
      }
      std::cout << std::endl;

    }
  else {

    std::cout << "StEEmcIUClusterMaker tower checksum: NULL collection, nclust=" << mNumberOfClusters[0] << std::endl;
    go &= (mNumberOfClusters[0]==0);

  }

  /// ------------------------------------------------------------------------
  ///
  /// SMD-cluster checksum
  ///
  Float_t emc_sum_smdu = 0.;
  Float_t eemc_sum_smdu = 0.;

  detector=stevent->emcCollection()->detector(kEndcapSmdUStripId);
  if ( !detector )
    {
      Warning("verifyStEvent","detector == NULL for smdu");
      return false;
    }

  cc=detector->cluster();
  if ( cc ) 
    {

      StSPtrVecEmcCluster &smduClusters=cc->clusters();

      for ( UInt_t i=0; i<smduClusters.size(); i++ ) 
	{
	  StEmcCluster *cl=smduClusters[i];
	  assert(cl);
	  emc_sum_smdu += cl->energy();
	}
      
  
      for ( Int_t sec=0; sec<12; sec++ ) 
	{

	  for ( Int_t i=0; i<numberOfSmdClusters(sec,0); i++ )
	
	    {
	      eemc_sum_smdu += smdcluster(sec,0,i).energy();

	    }

	}

      std::cout << "StEEmcIUClusterMaker smdu checksum: ";
      if ( emc_sum_smdu == eemc_sum_smdu ) {
	std::cout << "passed";
      }
      else {
	std::cout << "FAILED"; go=false;
      }
      std::cout << std::endl;

    }  
  else 
    {
      std::cout << "StEEmcIUClusterMaker smdu checksum: NULL collection, nclust=" << mNumberOfClusters[4] << std::endl;
      go &= (mNumberOfClusters[4]==0);
    }


  /// -----------------------

  Float_t emc_sum_smdv = 0.;
  Float_t eemc_sum_smdv = 0.;

  detector=stevent->emcCollection()->detector(kEndcapSmdVStripId);
  if (!detector)
    {
      Warning("verifyStEvent","detector == NULL for smdv");
      return false;
    }

  cc=detector->cluster();

  if ( cc )
    {

      StSPtrVecEmcCluster &smdvClusters=cc->clusters();
      
      for ( UInt_t i=0; i<smdvClusters.size(); i++ ) 
	{
	  StEmcCluster *cl=smdvClusters[i];
	  assert(cl);
	  emc_sum_smdv += cl->energy();
	}
      
      
      for ( Int_t sec=0; sec<12; sec++ ) 
	{
	  
	  for ( Int_t i=0; i<numberOfSmdClusters(sec,1); i++ )
	    
	    {
	      eemc_sum_smdv += smdcluster(sec,1,i).energy();
	      
	    }
	  
	}
      
      std::cout << "StEEmcIUClusterMaker smdv checksum: ";
      if ( emc_sum_smdv == eemc_sum_smdv ) {
	std::cout << "passed";
      }
      else {
	std::cout << "FAILED"; go=false;
      }
      std::cout << std::endl;

    }
  else 
    {
      std::cout << "StEEmcIUClusterMaker smdv checksum: NULL collection, nclust=" << mNumberOfClusters[5] << std::endl;
      go &= (mNumberOfClusters[5]==0);
    }
  

  
  return go;
}



// ----------------------------------------------------------------------------
void StEEmcIUClusterMaker::print()
{

  std::cout << "StEEmcIUClusterMaker::print()" << std::endl;
  const Char_t *names[] = { "tower", "pre1", "pre2", "post", "smdu", "smdv" };
  for ( Int_t i=0;i<6;i++ )
    {

      std::cout << "Number of " << names[i] 
		<< " clusters = " << mNumberOfClusters[i] 
		<< std::endl;

    }

  std::cout << "printout of tower clusters follows:" << std::endl;
  for ( Int_t sec=0;sec<12;sec++){
  for ( Int_t i=0;i<numberOfSmdClusters(sec,0);i++ )
    {
      StEEmcIUSmdCluster clust=(mSmdClusters[sec][0])[i];
      clust.print();
      //std::cout << "cluster.key()=" << clust.key() << std::endl;
      //      std::cout << "cluster.eta()=" << clust.momentum().Eta() << std::endl;
      //      std::cout << "cluster.phi()=" << clust.momentum().Phi() << std::endl;      
      //      std::cout << "cluster.energy()=" << clust.energy() << std::endl;
    }
 for ( Int_t i=0;i<numberOfSmdClusters(sec,1);i++ )
    {
      StEEmcIUSmdCluster clust=(mSmdClusters[sec][1])[i];
      clust.print();
    }
  }

}
