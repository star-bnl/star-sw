#include "StEEmc2x2ClusterMaker.h"

#include "StEEmcPool/StEEmcA2EMaker/StEEmcA2EMaker.h"

#include <algorithm>

#include "StMuDSTMaker/COMMON/StMuTrack.h"

#include "StMcEvent/StMcEvent.hh"
#include "StMcEvent/StMcTrack.hh"
#include "StMcEvent/StMcVertex.hh"

#include "StMessMgr.h"

ClassImp(StEEmc2x2ClusterMaker);

// ----------------------------------------------------------------------------
StEEmc2x2ClusterMaker::StEEmc2x2ClusterMaker(const Char_t *name, const StEEmcA2EMaker *a2e, StMuDstMaker * /*mumk*/ ) 
    : StEEmcGenericClusterMaker(name,a2e), StEEmc2x2ClusterParams()
{
  mSmdSeedEnergy  = 4.0 / 1000.0;
  mSmdMinEnergy   = 0.5 / 1000.0;

  mMaxExtent      = 40;
  mTruncateRatio  = 1.2;
  mMinStrips      = 3;

  mSeedEnergy[0]=0.8;        mMinEnergy[0]=0.1;
  mSeedEnergy[1]=1.0/1000.0; mMinEnergy[1]=0.1/1000.0;
  mSeedEnergy[2]=1.0/1000.0; mMinEnergy[2]=0.1/1000.0;
  mSeedEnergy[3]=1.0/1000.0; mMinEnergy[3]=0.1/1000.0;

  mEtaCut = 999;
  mPhiCut = 999;

  //mMuDst=mumk;
  //  assert(mumk);

  setDoBreakInflection(0);
  setDoBreakTruncation(0);
  setDoBreakSize(0);

  mUseFloor=false;
  mFloorParams[0]=0.;mFloorParams[1]=0.;
  
}

// ----------------------------------------------------------------------------
Int_t StEEmc2x2ClusterMaker::Make()
{

  buildTowerClusters();
  buildPreshowerClusters();
  //$$$  buildSmdClusters();
  buildPostshowerClusters();
  Int_t result = StEEmcGenericClusterMaker::Make();
  return result;
}

// ----------------------------------------------------------------------------
Int_t StEEmc2x2ClusterMaker::buildLayer(Int_t layer )
{

  const Char_t *clayers[]={"T","P","Q","R","U","V"};

  LOG_INFO << " build clusters for layer=" << clayers[layer] << endm;

  // get list of hit towers
  StEEmcTowerVec_t hits=mEEanalysis->towers(layer);

  // sort towers ascending in energy
  std::sort( hits.begin(), hits.end());
  // and reverse so it's descending
  std::reverse( hits.begin(), hits.end());

  LOG_DEBUG<<"searching for seeds in layer "<<clayers[layer]<<endm;

  // loop over all hit towers and find seeds
  StEEmcTowerVec_t seeds;
  for ( UInt_t i=0;i<hits.size();i++ ) 
    {      
      if ( hits[i].fail() ) continue;
      if ( hits[i].energy() > mSeedEnergy[layer] ) 
	seeds.push_back(hits[i]);
      else
	break;
      LOG_DEBUG<<"+ "<<hits[i].name()<<" "<<hits[i].energy()<<endm;
    }
  LOG_DEBUG<<"clustering"<<endm;


  // Define the four clustering positions relative to each seed tower
  struct Cluster {
    int quad;
    int *etabins;
    int *phibins;
    float et;
  };

  int etaA[]={0, 0, 1, 1};
  int etaB[]={0, 0,-1,-1};
  int phiA[]={0, 1, 0, 1};
  int phiB[]={0,-1, 0,-1};

  Cluster Cluster0 = { 0, etaA, phiA, 0. };
  Cluster Cluster1 = { 1, etaA, phiB, 0. };
  Cluster Cluster2 = { 2, etaB, phiB, 0. };
  Cluster Cluster3 = { 3, etaB, phiA, 0. };
  Cluster Quads[]={Cluster0,Cluster1,Cluster2,Cluster3};

  // next we form clusters around seeds.  clusters are 2x2 in size.

  Bool_t used[720];
  for ( Int_t i=0;i<720;i++ ) used[i]=false;

  for ( UInt_t i=0;i<seeds.size();i++ )
    {

      StEEmcTower tow=seeds[i];      
      if ( tow.fail() ) continue; // bad hardware
      if ( used[tow.index()] ) continue; // used by another cluster

      Int_t etabin_seed = tow.etabin();
      Int_t phibin_seed = tow.phibin();


      //      std::cout << Form("----------------------- clustering around %s -----------------------",tow.name()) << std::endl;
      StEEmcCluster c;      
      Int_t ntowers = 0;
      // Loop over the 4 possible clustering positions and find the
      // highest E_T cluster

      for ( Int_t j=0;j<4;j++ )
	{

	  StEEmcCluster temp;
	  Int_t ntow=0;
	  for ( Int_t k=0;k<4;k++ ) // loop over the 4 towers 
	    {

	      Int_t myeta = etabin_seed + Quads[j].etabins[k];
	      Int_t myphi = phibin_seed + Quads[j].phibins[k];
	      if ( myeta < 0 || myeta > 11 ) continue; // off the doughnut
	      Int_t myindex = myeta + 12 * ( ( myphi + 60 ) % 60 );
	      if ( used[myindex] ) continue; // Skip used towers
	
	      const StEEmcTower &t = mEEanalysis->tower(myindex,layer);
	      if ( t.energy() > mMinEnergy[layer] ) {
		temp.add(t);
		ntow++;			      
	      }

	    }// loop over the 4 towers
	  
	  if ( ntow > 0 && temp.energy() > c.energy() )
	    {
	      c=temp;
	      ntowers = ntow;
	    }

	} // loop over the 4 2x2 combinations of towers



      // double check validity of cluster
      if ( ntowers ) {
	add(c); // add to the list of clusters
	for ( Int_t ii=0;ii<c.numberOfTowers();ii++ )
	  {
	    //    std::cout << Form("flagging %s",c.tower(ii).name()) << std::endl;
	    used[ c.tower(ii).index() ] = true; // flag tower as used
	  }

	LOG_DEBUG<<"+ key="<<c.key()<<" seed="<<c.tower(0).name()<<" energy="<<c.energy()<<endm;
      }

    }

  return kStOk;

}

// ----------------------------------------------------------------------------
Int_t StEEmc2x2ClusterMaker::buildTowerClusters()
{
  return buildLayer(0);
}

// ----------------------------------------------------------------------------
Int_t StEEmc2x2ClusterMaker::buildPreshowerClusters()
{
  buildLayer(1);
  buildLayer(2);
  return kStOK;
}

// ----------------------------------------------------------------------------
Int_t StEEmc2x2ClusterMaker::buildPostshowerClusters()
{
  return buildLayer(3);
}

// ----------------------------------------------------------------------------
Int_t StEEmc2x2ClusterMaker::buildSmdClusters()
{

  LOG_INFO << " building SMD clusters" << endm;
    
  for ( Int_t sector=0;sector<12;sector++ ) {

    Float_t etmax = 0.;
    for ( UInt_t ii=0;ii<mTowerClusters[sector][0].size();ii++ )
      {
	const StEEmcCluster &c = mTowerClusters[sector][0][ii];
	if ( c.momentum().Perp() > etmax ) etmax = c.momentum().Perp();
      }


    if ( etmax >= 4.5 ) {
      mSmdSeedEnergy = ( seed_threshold + seed_slope * (etmax-4.5) ) / 1000.0;
      mSmdMinEnergy  = 0.1 * mSmdSeedEnergy;
    }
    else {
      mSmdSeedEnergy = seed_threshold / 1000.0;
      mSmdMinEnergy  = 0.1 * mSmdSeedEnergy;
    }

    LOG_INFO<<GetName()<<" sector="<<sector<<" seed energy="<<mSmdSeedEnergy<<endm;

    for ( Int_t plane=0;plane<2;plane++ ) 
      {


	// get number of smd strips in this plane
	Int_t nstrips = (Int_t)mESmdGeom -> getEEmcSector( plane, sector ).stripPtrVec.size();
	
	// Array denoting which smd strips are in use
	Bool_t used[288]; for (Int_t i=0;i<288;i++ ) used[i]=false;

	// Additional energy cut imposed upon seed strips after identifiying an SMD cluster
	Float_t floor[288]; for ( Int_t i=0;i<288;i++ ) floor[i]=0.;
      
	// Get list of hit smd strips      
	StEEmcStripVec_t strips=mEEanalysis->strips(sector,plane);
	// Sort by energy
	std::sort(strips.begin(),strips.end());
	// Order in descending energy
	std::reverse(strips.begin(),strips.end());


	LOG_DEBUG << " sector=" << sector 
		  << " plane=" << plane 
		  << " nhit strips=" << strips.size() 
		  << endm;

	StEEmcStripVec_t seeds;

	Float_t emax_strip = 0.;
	for ( UInt_t i=0;i<strips.size();i++ ) 
	  {

	    // Get the index of the strip
	    Int_t myindex = strips[i].index();

	    // Bail out if the strip is below seed energy
	    if ( strips[i].energy() < mSmdSeedEnergy ) break;

	    // Skip if near edge of the plane
	    if ( myindex < 4 || myindex > nstrips-4 ) continue;
	    
	    // Skip if fail bit is set
	    if ( strips[i].fail() ) continue;

	    // Require signal in adjacent smd strips
	    const StEEmcStrip *stripL = &mEEanalysis->strip(sector,plane,myindex-1);
	    const StEEmcStrip *stripR = &mEEanalysis->strip(sector,plane,myindex+1);
	    if ( stripL->fail() ) stripL=&mEEanalysis->strip(sector,plane,myindex-2);
	    if ( stripR->fail() ) stripR=&mEEanalysis->strip(sector,plane,myindex+2);
	    if ( stripL->energy() < mSmdMinEnergy || stripR->energy() < mSmdMinEnergy ) continue;
	    
	    // Smd strip passes minimum requirements for a seed
	    seeds.push_back( strips[i] );

	    if ( strips[i].energy() > emax_strip )
	      {
		emax_strip = strips[i].energy();
	      }

	  }

	
	
	LOG_DEBUG << " sector=" << sector
		  << " plane=" << plane
		  << " n seed strips=" << seeds.size()
		  << endm;



	StEEmcSmdClusterVec_t clusters;


	for ( UInt_t i=0;i<seeds.size(); i++ )
	  {

	    // Get the current smd seed
	    const StEEmcStrip &seed=seeds[i];
	    Int_t myindex=seed.index();

	    //printf("seed sec=%i plane=%i index=%i energy=%6.3f\n",seed.sector(),seed.plane(),seed.index(),seed.energy());
	   
	    // Skip if seed is in use by another cluster
	    if ( used[myindex] ) continue;
	    used[myindex]=true;

	    // Skip if seed energy too low given floor
	    if ( seed.energy() < mSmdSeedEnergy + floor[myindex] ) continue;

	    StEEmcSmdCluster cluster;
	    cluster.add( seed );

	    Int_t break_inflection = 0;

	    Int_t break_energy = 0;

	    // add smd strips
	    for ( Int_t j=1;j<=mMaxExtent;j++ )
	      {

		Float_t ecluster = cluster.energy();
		Int_t fail_count = 0;

		Int_t istrip;
		//
		// add strip on the right hand side of the seed
		//		
		istrip = myindex+j;
		if ( istrip < nstrips ) { // strip is in the plane

		  if ( used[istrip] ) break; /* we ran into another cluster */

		  const StEEmcStrip &strip=mEEanalysis->strip(sector,plane,istrip);
		  Float_t energy=strip.energy();

		  // break at inflection points
		  if ( istrip > 1 && istrip < nstrips-1 ) {
		    const StEEmcStrip &strip_left=mEEanalysis->strip(sector,plane,istrip-1);
		    const StEEmcStrip &strip_right=mEEanalysis->strip(sector,plane,istrip+1);
		    if ( strip_right.energy() > strip_left.energy() ) break_inflection++;
		  }

		  if ( energy > mSmdMinEnergy ) 
		    {
		      if ( !strip.fail() ) {
			used[istrip]++;
			cluster.add(strip);			
		      }
		      else
			fail_count++;
		    }
		  else
		    break_energy++;


		} // to the right


		//
		// add strip on the left hand side of the seed
		//
		istrip = myindex-j;
		if ( istrip >=0 ) { // strip is in the plane

		  if ( used[istrip] ) break; /* we ran into another cluster */

                  const StEEmcStrip &strip=mEEanalysis->strip(sector,plane,istrip);
                  Float_t energy=strip.energy();

		  // break at inflection points
		  if ( istrip > 1 && istrip < nstrips-1 ) {
		    const StEEmcStrip &strip_left=mEEanalysis->strip(sector,plane,istrip-1);
		    const StEEmcStrip &strip_right=mEEanalysis->strip(sector,plane,istrip+1);
		    //if ( strip_right.energy() < strip_left.energy() ) break;
		    if ( strip_right.energy() < strip_left.energy() ) break_inflection++;
		    
		  }

                  if ( energy > mSmdMinEnergy )
                    {
                      if ( !strip.fail() ) {
			used[istrip]++;
			cluster.add(strip);
		      }
		      else
			fail_count++;
                    }
		  else
		    break_energy++;

		  
		}

		if ( break_energy ) break;


		//
		// terminate cluster when it doesn't grow enough
		//
		Float_t etruncate = ecluster;
		etruncate *= ( mTruncateRatio - ( mTruncateRatio-1.0 ) * ( fail_count ) );
		if ( cluster.energy() < etruncate && mBreakTruncation ) break;		

		//
		// truncate the cluster if it reaches an inflection point
		//
		if ( mBreakInflection && break_inflection ) break;

	      }// done adding smd strips

	    
	    // add smd cluster to list of clusters
	    if ( cluster.size() >= mMinStrips ) {
	      add(cluster);
	      LOG_INFO << " adding " << cluster << endm;
	      Int_t index=seed.index();
	      // for narrow clusters, no additional seeds w/in +/- 3 smd strips are allowed
	      for ( Int_t ii=index-3;ii<=index+3; ii++ ) 
		if ( ii>=0 && ii<288 ) used[ii]=1;

	      
	      // add a long distance "floor" 
	      if ( mUseFloor )
		for ( Int_t ii=0;ii<288;ii++ )
		  {
		    Float_t f = mFloorParams[0] * cluster.energy() * TMath::Gaus( seed.index()-ii, 0., mFloorParams[1] * cluster.sigma(), true );
		    floor[ii]+=f;
		  }
	      

	    }

	  }


      }
  }


  return kStOK;
}

// ----------------------------------------------------------------------------


