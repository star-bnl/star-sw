#include "StGammaTreeMaker.h"

#include <TFile.h>
#include <TTree.h>

#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"

#include "StGammaEventMaker.h"
#include   "StGammaEvent.h"

#include "StGammaRawMaker.h"



ClassImp(StGammaTreeMaker);

// ---------------------------------------------------------------------
StGammaTreeMaker::StGammaTreeMaker(const Char_t *name):StMaker(name)
{
  mFilename="gamma_tree.root";
}

// ---------------------------------------------------------------------
Int_t StGammaTreeMaker::Init()
{
  if ( !mGammaFile )
    mGammaFile=new TFile(mFilename,"RECREATE");
  if ( !mGammaTree ) {
    mGammaTree=new TTree("gammas","Gamma TTree $Id: StGammaTreeMaker.cxx,v 1.3 2007/06/08 16:15:40 jwebb Exp $ built "__DATE__" "__TIME__);    
    mGammaTree->SetDirectory(mGammaFile);
  }

  /***
  mGammaEvent=new StGammaEvent();

  ***/
  
  StGammaEventMaker *gemaker = (StGammaEventMaker*)GetMaker("gemaker");
  assert(gemaker);

  mGammaEvent = gemaker->event();
  mGammaTree->Branch("GammaEvent",&mGammaEvent,32000,99);
  mGammaTree->BranchRef(); // to ensure reference table is saved

  assert(mGammaFile);
  assert(mGammaTree);

  return StMaker::Init();

}

Int_t StGammaTreeMaker::Finish()
{
  mGammaFile->cd();
  mGammaTree->Write();
  return kStOK;
}

// ---------------------------------------------------------------------



Int_t StGammaTreeMaker::Make()
{

  if ( !GetDataSet("MuDst") ) 
    {
      LOG_DEBUG<<" +++++ MuDst is missing from the chain +++++" << endm;
      return kStFatal;
    }


  // copy raw information into the event
  StGammaRawMaker *rawmk = (StGammaRawMaker*)GetMaker("grawmaker");
  if ( !rawmk ) 
    {
      LOG_WARN<<"StGammaRawMaker not in chain"<<endm;
    }
  else 
    {

      // copy tracks
      StGammaTrackVec_t tracks = rawmk->tracks();
      for ( UInt_t i=0;i<tracks.size();i++ )
	{
	  StGammaTrack *track = mGammaEvent->newTrack();
	  *track = tracks[i];
	}

      // copy towers
      StGammaTowerVec_t towers = rawmk->towers();
      for ( UInt_t i=0;i<towers.size();i++ )
	{
	  StGammaTower *tower = mGammaEvent->newTower();
	  *tower = towers[i];
	}

      // preshower1 
      StGammaTowerVec_t preshower1 = rawmk->preshower1();
      for ( UInt_t i=0;i<preshower1.size();i++ )
	{
	  StGammaTower *pre1 = mGammaEvent->newPreshower1();
	  *pre1 = preshower1[i];
	}

      // preshower2
      StGammaTowerVec_t preshower2 = rawmk->preshower2();
      for ( UInt_t i=0;i<preshower2.size();i++ )
	{
	  StGammaTower *pre2 = mGammaEvent->newPreshower2();
	  *pre2 = preshower2[i];
	}

      // postshower
      StGammaTowerVec_t postshower = rawmk->postshower();
      for ( UInt_t i=0;i<postshower.size();i++ )
	{
	  StGammaTower *pre1 = mGammaEvent->newPostshower();
	  *pre1 = postshower[i];
	}

      // smd strips
      StGammaStripVec_t strips = rawmk->strips();
      for ( UInt_t i=0;i<strips.size();i++ )
	{
	  StGammaStrip *strip = mGammaEvent->newStrip();
	  *strip = strips[i];
	}


    }


  // copy primary vertex

  StMuPrimaryVertex *pv = StMuDst::primaryVertex();
  if ( pv )
    {
      mGammaEvent->SetVertex(TVector3(pv->position().x(),pv->position().y(),pv->position().z()));
      mGammaEvent->mFlags |= TPC_VERTEX;
    }
  else
    {
      mGammaEvent->SetVertex(TVector3(0.,0.,0.));
      mGammaEvent->mFlags |= !(TPC_VERTEX);
    }
  

  
    
    
  // write out the tree
  if ( mGammaTree ) mGammaTree->Fill();
  
  return kStOK;



  // write out the tree
  if ( mGammaTree ) mGammaTree->Fill();

  return kStOK;

}

// ---------------------------------------------------------------------
void StGammaTreeMaker::Clear(Option_t *opts)
{
  mGammaEvent->Clear();
}
