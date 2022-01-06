#include "StMyClusterMaker.h"

#include "StEEmcPool/StEEmcA2EMaker/StEEmcA2EMaker.h"

#include <algorithm>

#include "StMuDSTMaker/COMMON/StMuTrack.h"

#include "StMcEvent/StMcEvent.hh"
#include "StMcEvent/StMcTrack.hh"
#include "StMcEvent/StMcVertex.hh"

#include "StMessMgr.h"

ClassImp(StMyClusterMaker);

// ----------------------------------------------------------------------------
StMyClusterMaker::StMyClusterMaker(const Char_t *name, const StEEmcA2EMaker *a2e, StMuDstMaker * /*mumk*/ ) 
    : StEEmcGenericClusterMaker(name,a2e), StMyClusterParams()
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
Int_t StMyClusterMaker::Make()
{

  buildTowerClusters();
  buildPreshowerClusters();
  buildSmdClusters();
  buildPostshowerClusters();
  Int_t result = StEEmcGenericClusterMaker::Make();
  return result;
  
}

// ----------------------------------------------------------------------------
Int_t StMyClusterMaker::buildLayer(Int_t layer )
{

  const Char_t *clayers[]={"T","P","Q","R","U","V"};

  LOG_DEBUG << " build clusters for layer=" << clayers[layer] << endm;

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

  // next we form clusters around seeds.  clusters grow to
  // absorb all continuous energized towers.
  Bool_t used[720];
  for ( Int_t i=0;i<720;i++ ) used[i]=false;
  for ( UInt_t i=0;i<seeds.size();i++ )
    {

      const StEEmcTower &tow=seeds[i];      
      if ( tow.fail() ) continue; // bad hardware
      if ( used[tow.index()] ) continue; // used by another cluster

      Int_t etabin_seed = tow.etabin();
      Int_t phibin_seed = tow.phibin();

      StEEmcCluster c;
      c.add(tow);
      used[tow.index()]=true;

      // loop over all hits.  if a given is adjacent to
      // the cluster and hasn't been used, grab it and
      // mark it as used.

    CLUSTERING:
      Int_t size_old = c.numberOfTowers();
      for ( UInt_t j=0;j<hits.size();j++ )
	{
	  const StEEmcTower &tow=hits[j];
	  
	  if ( used[ tow.index() ] ) continue; // next hit
	  if ( tow.fail()          ) continue; // bad channel

	  if ( tow.energy() < mMinEnergy[layer] ) break; // rest are not considered hit

	  Int_t deta=TMath::Abs(etabin_seed - tow.etabin());
	  if ( deta > mEtaCut ) continue; 

	  Int_t phi1=TMath::Max(phibin_seed,tow.phibin());
	  Int_t phi2=TMath::Min(phibin_seed,tow.phibin());
	  Int_t diff1 = phi1-phi2;
	  Int_t diff2 = phi2-phi1+60;
	  Int_t dphi=TMath::Min(diff1,diff2);
	  if ( dphi > mPhiCut ) continue;
  
	  if ( c.isNeighbor( tow ) ) {
	    c.add(tow);
	    used[tow.index()]=true;
	  }

	}
      if ( c.numberOfTowers() > size_old ) goto CLUSTERING;

      add(c); // add to the list of clusters
      LOG_DEBUG<<"+ key="<<c.key()<<" seed="<<c.tower(0).name()<<" energy="<<c.energy()<<endm;

    }

  return kStOk;

}

// ----------------------------------------------------------------------------
Int_t StMyClusterMaker::buildTowerClusters()
{
  buildLayer(0);
  return kStOK;
}

// ----------------------------------------------------------------------------
Int_t StMyClusterMaker::buildPreshowerClusters()
{
  buildLayer(1);
  buildLayer(2);
  return kStOK;
}

// ----------------------------------------------------------------------------
Int_t StMyClusterMaker::buildPostshowerClusters()
{
  buildLayer(3);
  return kStOK;
}

// ----------------------------------------------------------------------------
Int_t StMyClusterMaker::buildSmdClusters()
{

  LOG_DEBUG << " building SMD clusters" << endm;
    
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

    LOG_DEBUG<<GetName()<<" sector="<<sector<<" seed energy="<<mSmdSeedEnergy<<endm;

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
	    if ( stripL->fail() ) stripL = &mEEanalysis->strip(sector,plane,myindex-2);
	    if ( stripR->fail() ) stripR = &mEEanalysis->strip(sector,plane,myindex+2);
	    if ( stripL->energy() < mSmdMinEnergy ||
		 stripR->energy() < mSmdMinEnergy ) continue;
	    
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
			used[istrip] = true;
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
			used[istrip] = true;
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
	      LOG_DEBUG << " adding " << cluster << endm;
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


