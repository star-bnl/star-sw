#include "StEEmcDisplayMaker.h"

#include "StEEmcPool/StEEmcA2EMaker/StEEmcA2EMaker.h"
#include "StEEmcPool/StEEmcClusterMaker/StEEmcGenericClusterMaker.h"
#include "StEEmcPool/StEEmcPointMaker/StEEmcGenericPointMaker.h"
#include "StEEmcPool/StEEmcPi0Mixer/StEEmcPi0Maker.h"

#include "StMessMgr.h"


#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTriggerIdCollection.h"

#include "StEvent/StEvent.h"
#include "StEvent/StTriggerIdCollection.h"
#include "StEvent/StTriggerId.h"

#include "StEEmcDisplay.h"

#include "TString.h"
#include "TTree.h"

ClassImp(StEEmcDisplayMaker);

StEEmcDisplayMaker::StEEmcDisplayMaker( const Char_t *name ) : StMaker(name)
{
  mDisplay=new StEEmcDisplay();
  mFileLocal=true;
  mFile=0;
  mTree=0;
  mCheckTrigger=false;
}

// ----------------------------------------------------------------------------
Int_t StEEmcDisplayMaker::Init()
{

  // create TTree, but only if a file is specified
  if ( mFile )
    {
      LOG_INFO<<"creating TTree, resident in file="<<mFile->GetName()<<endm;
      mTree=new TTree("mTree","StEEmcDisplayMaker TTree");
      mTree->Branch("display",&mDisplay,32000,2); 
      mTree->SetDirectory( mFile ); /* directory resides on disk */
    }

  return StMaker::Init();
}

// ----------------------------------------------------------------------------
Int_t StEEmcDisplayMaker::Make()
{

  
  Int_t run   = GetRunNumber();
  Int_t event = GetEventNumber();
  
  LOG_INFO<<"run="<<run<<" event="<<event<<endm;

  Int_t triggerid = checkTrigger();
  if ( mCheckTrigger && !triggerid ) return kStOK;

  LOG_INFO << "triggers "<<triggerList()<<endm;


  /*
   * User event selection.
   *
   * The way the code is configured by default, all events which satisfy
   * the trigger conditions will be saved.  This can get expensive in terms
   * of disk space.  If you're looking for "rare" events, this is a good
   * place to select them.
   *
   * The example below checks the pi0 finder for a pi0 or eta candidate
   *
   */

#if 0

  if ( mEEpairs )
    {
      Bool_t go = false;
      StEEmcPairVec_t pairs = mEEpairs->pairs();
      for ( UInt_t ii=0;ii<pairs.size();ii++ )
	{
	  if ( pair.mass() > 0.11 && pair.mass() < 0.16 ) go = true;
	  if ( pair.mass() > 0.45 && pair.mass() < 0.65 ) go = true;
	}
      if ( !go ) return kStOK; 
    }

#endif


  /*
   * 
   */



  // form title and name for display
  TString name="R";name+=run;name+="-";name+=event;
  TString title="run=";title+=run;
  title+=" event=";title+=event;
  title+=" triggers=";title+=triggerList();

  title+=" ntow/pre1/pre2/post=";
  title+=mEEanalysis->numberOfHitTowers(0);title+="/";
  title+=mEEanalysis->numberOfHitTowers(1);title+="/";
  title+=mEEanalysis->numberOfHitTowers(2);title+="/";
  title+=mEEanalysis->numberOfHitTowers(3);

  mDisplay->SetName(name);
  mDisplay->SetTitle(title);

  /*
   * ADC to Energy
   */

  if ( mEEanalysis ) 
    {

      // add hit towers to display (0=towers, ..., 3=postshower)
      for ( Int_t layer=0;layer<4;layer++ )
	for ( Int_t i=0;i<mEEanalysis->numberOfHitTowers(layer);i++ )
	  mDisplay->add( mEEanalysis->hittower(i,layer) );
      
      // add hit smd strips to display
      for ( Int_t sec=0;sec<12;sec++ )
	for ( Int_t plane=0;plane<2;plane++ )
	  for ( Int_t i=0;i<mEEanalysis->numberOfHitStrips(sec,plane);i++ )
	    mDisplay->add( mEEanalysis->hitstrip(sec,plane,i) );

    }
  else
    LOG_WARN<<" adc-to-energy maker not in chain"<<endm;

  /*
   ***********************************************************
   *
   * SMD Clusters
   *
   *********************************************************** 
   */
  StEEmcSmdClusterVec_t uclusters; // flat array of smd-u clusters
  StEEmcSmdClusterVec_t vclusters; // flat array of smd-v clusters
  std::vector<Int_t>    uflags;    // flags for smd-u clusters
  std::vector<Int_t>    vflags;    // flags for smd-v clusters
  std::map<Int_t,Int_t> umap;      // map for key -> index of smd-u cluster in above arrays
  std::map<Int_t,Int_t> vmap;      // map for key -> index of smd-v cluster in above arrays

  Int_t nu=0;
  Int_t nv=0;

  if ( mEEclusters )
    {
      
      for ( Int_t sec=0;sec<12;sec++ ) 
	{
	  StEEmcSmdClusterVec_t u=mEEclusters->smdclusters(sec,0);
	  for ( UInt_t ii=0;ii<u.size();ii++ )
	    {
	      uclusters.push_back( u[ii] );
	      uflags.push_back(0);
	      umap[ u[ii].key() ] =  nu;
	      LOG_INFO<<"++++ ukey="<<u[ii].key()<<" uid="<<nu << endm;
	      nu++;
	    }

	  StEEmcSmdClusterVec_t v=mEEclusters->smdclusters(sec,1);
	  for ( UInt_t ii=0;ii<v.size();ii++ )
	    {
	      vclusters.push_back( v[ii] );
	      vflags.push_back(0);
	      vmap[ v[ii].key() ] = nv;
	      LOG_INFO<<"++++ vkey="<<v[ii].key()<<" vid="<<nv << endm;
	      nv++;
	    }

	}

    }
  else
    LOG_WARN<<" cluster maker not in chain"<<endm;


  /*
   ******************************************************
   *
   * Points
   *
   ******************************************************
   */
  StEEmcPointVec_t   points;
  std::vector<Int_t> pflags;
  std::vector<Int_t> point_uid;
  std::vector<Int_t> point_vid;

  if ( mEEpoints )
    {
      points = mEEpoints->points();
      for ( UInt_t ii=0;ii<points.size();ii++ )
	{
	  StEEmcPoint p=points[ii];
	  StEEmcSmdCluster u=p.cluster(0);
	  StEEmcSmdCluster v=p.cluster(1);
	  Int_t ukey=u.key();
	  Int_t vkey=v.key();
	  pflags.push_back( 0 );
	  Int_t uid=umap[ukey];
	  Int_t vid=vmap[vkey];
	  point_uid.push_back( uid );
	  point_vid.push_back( vid );
	  LOG_INFO<<"++++" << " point key="<<p.key()<<" ukey="<<ukey<<" vkey="<<vkey << " uid="<<uid<<" vid="<<vid << endm;
	}

    }
  else
    LOG_WARN<<" point maker not in chain" << endm;
  

  /*
   ******************************************************
   *
   * Pairs 
   *
   ******************************************************
   */

  StEEmcPairVec_t pairs;
  if ( mEEpairs )
    {
      pairs = mEEpairs->pairs();      
    }
  else
    LOG_WARN<<" pi0 maker is not in chain"<<endm;



  /*
   ******************************************************
   *
   * Build display data
   *
   ******************************************************
   */

  // loop over points and add to display

  for ( UInt_t i = 0; i < points.size(); i++ )
    {


      Int_t icol = 20 + 10 * ( i%3 ) + i/3;

      StEEmcPoint p = points[i];
      mDisplay->add(p,icol);

      StEEmcSmdCluster u=uclusters[ point_uid[i] ];
      StEEmcSmdCluster v=vclusters[ point_vid[i] ];


      
      mDisplay->add(u, icol, 1001);
      mDisplay->add(v, icol, 1001);

      uflags[ point_uid[i] ] = 1; // flag cluster as already displayed
      vflags[ point_vid[i] ] = 1; // flag cluster as already displayed
            
    }
   
  // loop over unassociated u clusters and add to display
  
  for ( UInt_t i = 0; i < uclusters.size(); i++ )
    {

      if ( uflags[i] ) continue; // cluster is claimed by a point
      mDisplay->add( uclusters[i], 13+i%5, 3425 );

    }

  for ( UInt_t i = 0; i < vclusters.size(); i++ )
    {

      if ( vflags[i] ) continue; // cluster is claimed by a point
      mDisplay->add( vclusters[i], 13+i%5, 3425 );

    }  	

  if ( mTree )
    {
      mTree->Fill();
    }
  else
    LOG_WARN<<" tree doesn't exist, no data saved"<<endm;


  return kStOK;
}

// ----------------------------------------------------------------------------
void StEEmcDisplayMaker::Clear( Option_t *opts )
{
  mDisplay->clear();
}

// ----------------------------------------------------------------------------
TFile *StEEmcDisplayMaker::setFile( const Char_t *file, const Option_t *opts )
{
  mFile=new TFile(file,opts);
  mFileLocal=true;
  return mFile;
}
TFile *StEEmcDisplayMaker::setFile( TFile *file )
{
  mFile=file;
  mFileLocal=false;
  return mFile;
}


// ----------------------------------------------------------------------------
Int_t StEEmcDisplayMaker::checkTrigger()
{

  /// If user didn't specify a trigger ID, always return true
  if ( mTriggerList.size() == 0 ) return 1;

  StTriggerId nominal;

  /// Get Trigger from MuDst if available, fallback to StEvent if not                                                               
  StMuDstMaker *mumk = (StMuDstMaker*)GetMaker("MuDst");
  StEvent *event = (StEvent*)GetInputDS("StEvent");

  if ( mumk )
    {
      nominal = mumk->muDst()->event()->triggerIdCollection().nominal();
      goto CHECK_TRIGGER;
    }

  /// Get Trigger from StEvent if available                                                                                         

  if ( event )
    {
      nominal=*event->triggerIdCollection()->nominal();
      goto CHECK_TRIGGER;
    }

  /// Bail out here because we don't have anything to do!                                                                           
  goto NO_DATA;


 CHECK_TRIGGER:

  for ( UInt_t ii=0;ii<mTriggerList.size();ii++ )
    {
      if ( nominal.isTrigger( mTriggerList[ii] ) ) return mTriggerList[ii];
    }
  return 0;

 NO_DATA:
  assert(2+2==5); // noooo data                                                                                                     
  return 0;

}

const
Char_t *StEEmcDisplayMaker::triggerList()
{

  static TString triggers="";

  StTriggerId nominal;
  /// Get Trigger from MuDst if available, fallback to StEvent if not 
  StMuDstMaker *mumk = (StMuDstMaker*)GetMaker("MuDst");
  StEvent *event = (StEvent*)GetInputDS("StEvent"); 
  if ( mumk )
    {
      nominal = mumk->muDst()->event()->triggerIdCollection().nominal();
      goto CHECK_TRIGGER;
    }
  
  /// Get Trigger from StEvent if available
  if ( event )
    {
      nominal=*event->triggerIdCollection()->nominal();
      goto CHECK_TRIGGER;
    }
  
  /// Bail out here because we don't have anything to do!
  goto NO_DATA;
  
  
 CHECK_TRIGGER:

  triggers="";

  for ( UInt_t ii=0;ii<mTriggerList.size();ii++ )
    {
      if ( nominal.isTrigger( mTriggerList[ii] ) ) {
	triggers+=mTriggerList[ii];
	triggers+=" ";
      }
    }

  return triggers.Data();
  
 NO_DATA:
  assert(2+2==5); // noooo data
  return 0;
    
}
