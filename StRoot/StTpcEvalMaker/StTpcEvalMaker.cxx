//-----------------------------------------------------------------------
// * $Id: StTpcEvalMaker.cxx,v 1.10 2003/09/02 17:59:13 perev Exp $
// * $Log: StTpcEvalMaker.cxx,v $
// * Revision 1.10  2003/09/02 17:59:13  perev
// * gcc 3.2 updates + WarnOff
// *
// * Revision 1.9  2001/08/10 12:14:24  flierl
// * correct call to number of tracks
// *
// * Revision 1.8  2001/07/10 09:21:47  flierl
// * add posibility to cut on vertex z-positions
// *
// * Revision 1.7  2001/06/19 12:49:37  flierl
// * add l3 option
// *
// * Revision 1.6  2001/04/25 19:08:04  perev
// * HPcorrs
// *
// * Revision 1.5  2001/04/06 22:27:20  flierl
// * add zillion of comments
// *
// * Revision 1.4  2000/08/07 03:25:15  snelling
// * Added selection on tracks
// *
// * Revision 1.3  2000/05/25 20:38:09  snelling
// * Added TPC evaluation histograms
// *
// * Revision 1.2  2000/05/24 19:20:52  snelling
// * Fixed distance std function for Solaris
// *
// * Revision 1.1.1.1  2000/05/23 00:25:03  snelling
// * Milton's and Manuel's version
// *
//-----------------------------------------------------------------------
// class definition of StTpcEvalMaker
//-----------------------------------------------------------------------

#include <Stiostream.h>
#include <stdlib.h>
#include <string>
#include <vector>
#include <algorithm>
#include <iterator>
using std::distance;
#include <math.h>

#include "StGlobals.hh"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StPhysicalHelixD.hh"
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"

#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "TFile.h"
#include "TH2.h"
#include "TProfile.h"
#include "TProfile2D.h"

#include "StTpcDb/StTpcDbMaker.h"
#include "StMessMgr.h"

#include "StAssociationMaker/StAssociationMaker.h"
#include "StAssociationMaker/StMcParameterDB.h"
#include "StAssociationMaker/StTrackPairInfo.hh"

#include "StEventTypes.h"

#include "StMcContainers.hh"
#include "StMcEvent.hh"
#include "StMcTpcHit.hh"
#include "StMcTpcHitCollection.hh"
#include "StMcTrack.hh"
#include "StMcVertex.hh"

#include "StEventMaker/StEventMaker.h"
#include "StMcEventMaker/StMcEventMaker.h"

#include "StTpcEvalMaker.h"
#include "StTpcEvalEvent.h"
#include "StTpcEvalHistograms.h"

static const char rcsid[] = "$Id: StTpcEvalMaker.cxx,v 1.10 2003/09/02 17:59:13 perev Exp $";
ClassImp(StTpcEvalMaker)

//-------------------------------------------------
  // TO DO:
  // inclusion of vertex information
  // id number for StGlobalTrack (may not be necessary)
  // and....
//-------------------------------------------------

//-----------------------------------------------------------------------
StTpcEvalMaker::StTpcEvalMaker(const char *name, const char *title):StMaker(name,title)
{
  mL3TriggerOn = false ;
  mVertexConstraint = 0 ;
}
//-----------------------------------------------------------------------
StTpcEvalMaker::~StTpcEvalMaker()
{
  // empty
}
//-----------------------------------------------------------------------
void StTpcEvalMaker::Clear(const char*)
{
    StMaker::Clear();
}
//-----------------------------------------------------------------------
Int_t StTpcEvalMaker::Finish()
{
    cout << " StTpcEvalMaker:  Writing out histograms..." <<endl;
    mOutputFile->Write();

    cout << " StTpcEvalMaker:  Closing output file..." <<endl;
    mOutputFile->Close();

    return StMaker::Finish();
}
//-----------------------------------------------------------------------
Int_t StTpcEvalMaker::Init()
{
    // Create the event structure.
    mTpcEvalEvent = new StTpcEvalEvent();
    
    cout << " StTpcEvalMaker:  Opening output file..." << endl;
    
    // Create the ROOT file.
    // to do: pass on string to replace StTpcEval.hist.root
    mOutputFile = new TFile("StTpcEval.hist.root","RECREATE","StTpcEvalMaker Results");
    mOutputFile->SetCompressionLevel(1);

    // Create the ROOT Tree
    mTrackPairTree = new TTree("trackTree", "Track Pair Tree");
    mTrackPairTree->SetAutoSave(100000000); // autosave every 100 Mbytes

    // Split event into sub-branches
    Int_t splitlevel = 1;
    Int_t bufsize = 64000;
    mTrackPairTree->Branch("StTpcEvalBranch","StTpcEvalEvent",&mTpcEvalEvent, bufsize, splitlevel);

    // Book histograms
    histograms.Book();
    return StMaker::Init();
}
//-----------------------------------------------------------------------
Int_t StTpcEvalMaker::Make()
{
    // get pointers to global variables 
    
    // TPC database
    mStTpcDb = ((StTpcDbMaker*) GetMaker("tpcDb"))->tpcDbInterface();
    if (!mStTpcDb)  
	{
	    cout<<" StTpcDb NOT FOUND!!!"<<endl;
	    return kStErr;
	}
    // StEvent    
    mStEvent = (StEvent*) GetInputDS("StEvent");
    mStMcEvent =  ((StMcEventMaker*) GetMaker("StMcEvent"))->currentMcEvent();
    if (!(mStEvent && mStMcEvent)) 
	{
	    cout<<" NO EVENT DATA FOUND!!!"<<endl;
	    return kStWarn;
	}

    // l3
    if(mL3TriggerOn)
	{
	    ml3TriggerEvent = mStEvent->l3Trigger() ;
	    if (!(ml3TriggerEvent)) {
		cout<<" NO l3 DATA FOUND!!!"<<endl;
		return kStWarn;
	    }
	}  

    // vertex constraint for l3
    if(mVertexConstraint)
      {
	Double_t mMcVertexZ = mStMcEvent->primaryVertex()->position().z() ;
	if( fabs(mMcVertexZ) > mVertexConstraint )
	  {
	    cout << "Event rejected due to z vertex consraint : z-vertex :" 
		 <<  mMcVertexZ << "\t limit :" << mVertexConstraint << endl ;
	    return kStWarn ;
	  }
      }
    
    // StAssociationMaker multimaps
    StAssociationMaker* assoc =  (StAssociationMaker*) GetMaker("StAssociationMaker");
    if (!assoc) 
	{
	    cout<<" NO StAssocationMaker DATA!!!"<<endl;
	    return kStWarn;
	}
    mmcTpcHitMap = assoc->mcTpcHitMap();
    mmcTrackMap  = assoc->mcTrackMap();
    mrcTpcHitMap = assoc->rcTpcHitMap();
    mrcTrackMap  = assoc->rcTrackMap();
    if (!mrcTpcHitMap || !mrcTrackMap || !mmcTpcHitMap || !mmcTrackMap) 
	{
	    gMessMgr->Warning() << "Missing multimaps!!! " << endm;
	    return kStWarn;
	}
    
    /////
    // fill the matching information
    ////
    // fill matched hit info
    if (mHitIteration) HitIteration();
    // fill distance to remaining hits
    //if (mHitSeparation) HitSeparation();
    // fill matched track info with mc hits as key
    mcTrackIteration();
    // fill matched track info with Rc hits as key
    //rcTrackIteration();


    // fill tree with StTpcEvalEvent
    //fillHeader();
    //mTrackPairTree->Fill();
    //mTpcEvalEvent->Clear();
    
    // go home
    return kStOK;
}
//-----------------------------------------------------------------------
void StTpcEvalMaker::fillHeader() 
{
    // fill header of StTpcEvalEvent object
    UInt_t globTrks, geantPrimaries, recoPrimaries, geantTpcHits, recoTpcHits;
    globTrks = geantPrimaries = recoPrimaries = geantTpcHits = recoTpcHits = 0;

    // Only count g2t tracks
    StMcTrackIterator mcTrkIter2 = mStMcEvent->tracks().begin();
    while (mcTrkIter2 != mStMcEvent->tracks().end() && !((*mcTrkIter2)->key())) ++mcTrkIter2;
    //changed to old style distance function because of sun compiler
    unsigned int geantTrks = 0;
//VP    distance(mcTrkIter2, mStMcEvent->tracks().end(), geantTrks);
      geantTrks = mStMcEvent->tracks().end()-mcTrkIter2;
//    geantTrks = distance(mcTrkIter2, mStMcEvent->tracks().end());
    
    // get some numbers
    globTrks = mStEvent->trackNodes().size();
    geantPrimaries = mStMcEvent->primaryVertex()->daughters().size();
    recoPrimaries  = mStEvent->trackNodes().size(); 
    geantTpcHits   = mStMcEvent->tpcHitCollection()->numberOfHits();
    recoTpcHits    = mStEvent->tpcHitCollection()->numberOfHits();

    // fill them
    mTpcEvalEvent->SetHeader(geantTrks, globTrks, geantPrimaries, recoPrimaries, geantTpcHits, recoTpcHits);
}
//-----------------------------------------------------------------------
void StTpcEvalMaker::HitIteration() 
{
    ///////////
    // loop over mc hits get matches and fill histos
    // loop over rc hits get matches and fill histos
    ///////////
    cout << "_ TpcHit iteration __________________" << endl;

    // get mc hits
    StMcTpcHitCollection* mcTpcHitCollection = mStMcEvent->tpcHitCollection();
    if (!mcTpcHitCollection) 
	{
	    cout << "--> StTpcEvalMaker warning: no StMcTpcHits found!" << endl;
	    return;
	}

    // create hitpair and initialize transformations using database
    MatchedHitPair hitPair(mStTpcDb);

    // some counters
    unsigned int nAssociatedHits[45];
    unsigned int nReconstructedHits[45];
    unsigned int nGeneratedHits[45];
    {for (unsigned int i = 0; i < 45; i++) 
	{
	    nAssociatedHits[i] = 0;
	    nReconstructedHits[i] = 0;
	    nGeneratedHits[i] = 0;
	}}

    /////
    // loop over mc hits    
    /////
    {for (unsigned int isec = 0; isec < mcTpcHitCollection->numberOfSectors(); isec++) 
	{
	    for (unsigned int irow = 0; irow < mcTpcHitCollection->sector(isec)->numberOfPadrows(); irow++) 
		{
		    for (StMcTpcHitIterator hitIter = mcTpcHitCollection->sector(isec)->padrow(irow)->hits().begin();
			 hitIter !=mcTpcHitCollection->sector(isec)->padrow(irow)->hits().end();
			 hitIter++) 
			{
			    // skip certain monte carlo Hits
			    //	if ((*hitIter)->parentTrack()->momentum().mag() < .1) continue;
			    //	if ((*hitIter)->parentTrack()->startVertex() != 
			    //	    mStMcEvent->primaryVertex()) continue;

			    // count the number of Generated Hits per padrow
			    nGeneratedHits[irow]++;
			    // access StAssociationMaker mc tpc hit map information
			    StMcTpcHit* mapKey = *hitIter;
			    pair<mcTpcHitMapIter,mcTpcHitMapIter> mapBounds = mmcTpcHitMap->equal_range(mapKey);
			    
			    // fill histos in StTpcEvalHistogramm object
			    histograms.mcHitPositionRad->Fill(mapKey->padrow());
			    histograms.mcHitPositionZ->Fill(mapKey->position().z());
			    
			    // iterate over matched rc hits
			    int nMatches = 0;
			    for (mcTpcHitMapIter mapIter = mapBounds.first; mapIter != mapBounds.second; mapIter++)
				{
				    // count number of associated hits per padrow
				    if (nMatches==0) nAssociatedHits[irow]++ ;
				    // count number of matches per hit
				    nMatches++;
				    // calculate (pos of mc) - (pos of matched rc) 
				    hitPair.resolution((*mapIter).first,(*mapIter).second);
				    // fill histos in StTpcEvalHistogramm object
				    histograms.tpcHitResX->Fill(hitPair.resolution().x());
				    histograms.tpcHitResY->Fill(hitPair.resolution().y());
				    histograms.tpcHitResZ->Fill(hitPair.resolution().z());
				}
			    //
			    if (!nMatches) 
				{
				    // fill position of mc hits which where not matched at all
				    histograms.mcUnmatchedHitPositionSector->Fill(mapKey->sector());
				    histograms.mcUnmatchedHitPositionRad->Fill(mapKey->padrow());
				    histograms.mcUnmatchedHitPositionZ->Fill(mapKey->position().z());
				}
			    // fill number of matches
			    histograms.matchesToRcHits->Fill(nMatches);
			}
		}
	}}// loop over mc hits
    
    ////
    // loop over rc hits    
    ////
    // get rc hits
    StTpcHitCollection* rcTpcHitCollection = (!mL3TriggerOn) ? mStEvent->tpcHitCollection() : ml3TriggerEvent->tpcHitCollection() ;
    if (!rcTpcHitCollection) 
	{
	    cout << "--> StTpcEvalMaker warning: no StRcTpcHits found!" << endl;
	    return;
	}
  
    // loop over rc hits
    {for (unsigned int isec=0; isec<rcTpcHitCollection->numberOfSectors(); isec++) 
	{
	    for (unsigned int irow=0;irow<rcTpcHitCollection->sector(isec)->numberOfPadrows(); irow++) 
		{
		    for (StSPtrVecTpcHitIterator hitIter = rcTpcHitCollection->sector(isec)->padrow(irow)->hits().begin();
			 hitIter != rcTpcHitCollection->sector(isec)->padrow(irow)->hits().end();
			 hitIter++) 
			{
			    
			    // calculate the number of Reconstructed Hits per padrow
			    nReconstructedHits[irow]++;
			    // access StAssociationMaker recon tpc hit map information
			    StTpcHit* mapKey = *hitIter;
			    // fill histos in StTpcEvalHistogramm object
			    histograms.rcHitPositionRad->Fill(mapKey->padrow());
			    histograms.rcHitPositionZ->Fill(mapKey->position().z());
			    // count matches 
			    unsigned int nMatches = mrcTpcHitMap->count(mapKey);
	
			    // fill histograms
			    if (nMatches) 
				{
				    if (nMatches==1) 
					{ 
					    // 1 to 1 matches
					    histograms.mc1to1HitPositionRad->Fill(mapKey->padrow());
					    histograms.mc1to1HitPositionZ->Fill(mapKey->position().z());
					}
				    else if (nMatches>1) 
					{
					    // Merged hits
					    histograms.mcMergedHitPositionRad->Fill(mapKey->padrow());
					    histograms.mcMergedHitPositionZ->Fill(mapKey->position().z());
					}
				}
			    else 
				{
				    histograms.rcUnmatchedHitPositionSector->Fill(mapKey->sector());
				    histograms.rcUnmatchedHitPositionRad->Fill(mapKey->padrow());
				    histograms.rcUnmatchedHitPositionZ->Fill(mapKey->position().z());
				}
			    // fill number of matches
			    histograms.matchesToMcHits->Fill(nMatches);
			}
		}
	}} // loop over rc hits
    
    
    ///
    // Fill efficiency and purity histograms
    ///
    float HitEfficiency = 0 ;
    float HitPurity = 0 ;

    for (unsigned int i = 0; i < 45; i++) 
	{
	    // calculate efficiency and purity per padrow
	    HitEfficiency = (float)nAssociatedHits[i] / (float)nGeneratedHits[i];
	    HitPurity = (float)nAssociatedHits[i] / (float)nReconstructedHits[i];
	    // fill histos
	    if (  HitEfficiency <1.2 &&  HitEfficiency>0.1 )
	      {
		histograms.mHitEfficiency->Fill((float)i+1, HitEfficiency);
	      } ;
	    histograms.mHitPurity->Fill((float) i+1, HitPurity);
	}
}
//-----------------------------------------------------------------------
void StTpcEvalMaker::mcTrackIteration() 
{
    //
    // loop over mc tracks and fill histos
    //
    cout << "_ StMcTrack iteration ___________________" << endl;
    
    // get mc tracks
    StMcVertex* mcPrimaryVertex = mStMcEvent->primaryVertex() ; 
    StSPtrVecMcTrack mcTrackContainer = mStMcEvent->tracks() ;
    // fill mc multiplicity
    histograms.mMcMultiplicity = mcTrackContainer.size() ;
    histograms.mPairMultiplicity = mmcTrackMap->size() ;

    // loop over mc tracks
    for (unsigned int iMcTrack = 0; iMcTrack < mcTrackContainer.size(); iMcTrack++) 
	{
	    // create empty track pair object	    
	    MatchedTrackPair trackPair;
	    // get mc track
	    StMcTrack* mcTrack = mcTrackContainer[iMcTrack];

	    // use the primary key from the g2t tables for the mc track id
	    // use id = 0 for shower tracks
	    // set sign to negative for non-primary vertex tracks
	    signed long trackId = mcTrack->key();
	    if (mcTrack->startVertex() != mcPrimaryVertex) 
		{
		    trackId *= -1;
		}
	    if (mcTrack->isShower())
		{
		    trackId = 0;
		}
	    trackPair.mcInfo()->setId(trackId);
	    
	    // add monte carlo information to the trackpair object
	    addMcTrack(mcTrack,trackPair.mcInfo());
	    
	    // Count the number of recontstructed tracks associated with the mc track.
	    unsigned int nMatchedRcTracks=mmcTrackMap->count(mcTrack);
	    trackPair.mcInfo()->setMatchedTracks(nMatchedRcTracks);
	    
	    pair<mcTrackMapIter,mcTrackMapIter> mcMapBounds = mmcTrackMap->equal_range(mcTrack);

	    // loop over associated rc tracks 
	    for (mcTrackMapIter mcMapIter = mcMapBounds.first; mcMapIter != mcMapBounds.second; ++mcMapIter)
		{
		    
		    //(*mcMapIter).first is the StMcTrack used as the map key
		    StTrackPairInfo* assocPair = (*mcMapIter).second; //StAssociationMaker
		    StGlobalTrack* rcTrack = assocPair->partnerTrack();
		    
		    // add rc information to the trackpair object
		    addRcTrack(rcTrack,trackPair.rcInfo());
		    
		    // add number of common hits to the trackpair object
		    trackPair.setCommonHits(assocPair->commonTpcHits());
		    
		    // how many StMcTrack are matched to rcTrack?
		    unsigned int nMatchedMcTracks=mrcTrackMap->count(rcTrack);
		    trackPair.rcInfo()->setMatchedTracks(nMatchedMcTracks);

		    // defined below...
		    scanTrackPair(&trackPair, mcTrack, rcTrack) ; 
		    
		    // ntuple row for the pair...
		    histograms.fillTrackNtuple(&trackPair); 

		    // fill also StTpcEvalEvent object
		    mTpcEvalEvent->addTrackPair(trackPair);
		    
		} // iterate over matched StGlobalTrack
	}   // iterate over StMcTrack   
}
//-----------------------------------------------------------------------
void StTpcEvalMaker::rcTrackIteration() 
{
    //
    // if we don't look at emmbedded events, but pure monte carlo :
    // this pass should be used to pick up ghost tracks
    // (the StGlobalTrack not matched to any StMcTrack)
    //     
    // loop over rc tracks and find mc matches
    cout << "_ StGlobalTrack iteration ___________________" << endl;
    
    // get rc tracks
    StSPtrVecTrackNode& rcTrackNodes = (!mL3TriggerOn) ? mStEvent->trackNodes() : ml3TriggerEvent->trackNodes() ;

    // fill histo
    histograms.mRcMultiplicity = rcTrackNodes.size();
    
    // loop over rc tracks
    for (StSPtrVecTrackNodeIterator nodeIter = rcTrackNodes.begin(); nodeIter != rcTrackNodes.end(); ++nodeIter) 
	{
	    // get rc track
	    StTrackNode* trackNode = *nodeIter ;
	    StGlobalTrack* rcTrack = dynamic_cast<StGlobalTrack*>(trackNode->track(global)) ;
	    
	    if (!rcTrack) continue;
	    
	    // number of matched mc tracks
	    unsigned int nMatchedMcTracks = mrcTrackMap->count(rcTrack);
	    
	    // fill histos in case we didn't find a match ( = this is a ghost in pure monte carlo events )
	    if (nMatchedMcTracks == 0) 
		{
		    // dummy pair
		    MatchedTrackPair trackPair;
		    // fill info into trackpair object
		    addRcTrack(rcTrack,trackPair.rcInfo());
		    // fill StTpcEvalHiso with track pair object
		    histograms.fillTrackNtuple(&trackPair);
		    // fill eval event with track pair object
		    mTpcEvalEvent->addTrackPair(trackPair);
		}
	} // iterate over track nodes   
}
//-----------------------------------------------------------------------
void StTpcEvalMaker::addMcTrack(StMcTrack* mcTrack, mcTrackInfo* mcInfo) 
{
    //
    // Fill mcTrackInfo
    //
    if (!mcTrack || !mcInfo) return;
    
    // momentum vector at start vertex
    mcInfo->setFourMomentum(mcTrack->fourMomentum());
    
    // number of StMcTpcHit
    StPtrVecMcTpcHit hitContainer = mcTrack->tpcHits();
    mcInfo->setHits(hitContainer.size());

    // count the track hits StMcTpcHit matched to any StTpcHit
    unsigned int matchedHits = 0;
    for (StMcTpcHitIterator i = hitContainer.begin(); i != hitContainer.end(); i++) 
	{
	    if ( mmcTpcHitMap->count(*i) ) matchedHits++;
	}

    mcInfo->setMatchedHits(matchedHits);
}
//-----------------------------------------------------------------------
void StTpcEvalMaker::addRcTrack(StGlobalTrack* rcTrack, rcTrackInfo* rcInfo)
{
    //
    // Fills rcTrackInfo
    //
    if (!rcTrack || !rcInfo) return;
    
    
    // To compare the right momentum, use primary tracks.
    // Use the information directly from the primary track.
    // If it is not a primary track, change the sign of the
    // primary key and use the information from the global track.
    StPrimaryTrack* pTrk = dynamic_cast<StPrimaryTrack*>(rcTrack->node()->track(primary));
    if (pTrk) 
	{
	    rcInfo->setId(pTrk->key());
	    rcInfo->setMomentum(pTrk->geometry()->momentum());
	}
    else 
	{
	    rcInfo->setId(-1*rcTrack->key());
	    rcInfo->setMomentum(rcTrack->geometry()->momentum());     
	}

    // The detector info. is shared by the primary & global,
    // so it doesn't matter which one we use.
    
    // number of StTpcHit hits
    rcInfo->setHits(rcTrack->detectorInfo()->numberOfPoints(kTpcId));
    // number of Fit StTpcHit hits
    rcInfo->setFitHits(rcTrack->fitTraits().numberOfFitPoints(kTpcId));
    
    // count the track's StTpcHits that are matched to any StMcTpcHit
    // Note that here we just check whether the track's hit is matched,
    // but we don't accumulate the number of matches each hit has.
    StPtrVecHit hitContainer = rcTrack->detectorInfo()->hits(kTpcId);
    int matchedHits = 0;
    for (StPtrVecHitIterator i = hitContainer.begin(); i != hitContainer.end(); i++) 
	{
	    StTpcHit* hitKey = dynamic_cast<StTpcHit*>(*i);
	    if (hitKey && mrcTpcHitMap->count(hitKey) ) matchedHits++;
	}
    rcInfo->setMatchedHits(matchedHits);
}
//-----------------------------------------------------------------------
void StTpcEvalMaker::scanTrackPair(MatchedTrackPair* trackPair, StMcTrack* mcTrack, StGlobalTrack* rcTrack) 
{
    //
    // calculate momentum resolution
    // count common, matched hits between mcInfo.track() and rcInfo.track()
    // calculate hit-averaged spatial separation between the track pair
    //
    if (!trackPair) return;
    
    if (!(mcTrack && rcTrack)) return;
    
    // note: StTrack momentum is a StThreeVectorD, StMcTrack
    // momentum is a StThreeVectorF...
    StThreeVectorF momRes, &dp=momRes;
    momRes = trackPair->mcInfo()->fourMomentum().vect() - trackPair->rcInfo()->momentum();
    trackPair->setMomentumResolution(dp);
    
    // iterate over the StTpcHit of rcTrack and check if the
    // matched StMcTpcHit belongs to mcTrack
    // (This means I do it the other way around from what Milton was doing,
    // in order to use the fact that the navigation from StMcTpcHit to
    // StMcTrack is unambiguous, whereas the navigation from StTpcHit to
    // StGlobalTrack is not.)
    
    const StMcTpcHit* mcHit;
    const StTpcHit* rcHit;
    MatchedHitPair hitPair(mStTpcDb);
    
    StPtrVecHit rcTrackHitContainer = rcTrack->detectorInfo()->hits(kTpcId);
    
    for (StHitIterator hitIter = rcTrackHitContainer.begin();hitIter != rcTrackHitContainer.end(); hitIter++)
	{
	    rcHit = dynamic_cast<StTpcHit*>(*hitIter);
	    if (!rcHit) continue;
	
	    pair<rcTpcHitMapIter,rcTpcHitMapIter> mapBounds = mrcTpcHitMap->equal_range(rcHit);
	    
	    // iterate over the StMcTpcHits matched to rcHit...
	    for (rcTpcHitMapIter mapIter = mapBounds.first; mapIter != mapBounds.second; mapIter++)
		{
		    
		    mcHit = (*mapIter).second; 
		    
		    // does this mcHit belong to the StMcTrack stored in trackPair?
		    
		    StMcTrack* mcTrkFromHit = mcHit->parentTrack();
		    
		    if (mcTrkFromHit == mcTrack) 
			{
			    trackPair->addHitResolution(hitPair.resolution(mcHit,rcHit));
			    break;
			}
		}
	} // iterate over StTpcHit of StGlobalTrack
}
//-----------------------------------------------------------------------
void StTpcEvalMaker::HitSeparation() 
{
    //////
    /// loop over mc hits and find distance to remaining hits
    /// loop over rc hits and find distance to remaining hits
    //////
    cout << " In Hit Separation module: Note very time consuming can be disabled in script" << endl;
    
    ////
    //  mc hits
    ///
    // get mc hits
    StMcTpcHitCollection* mcTpcHitCollection = mStMcEvent->tpcHitCollection();
    if (!mcTpcHitCollection) 
	{
	    cout << "--> StTpcEvalMaker warning: no StMcTpcHits found!" << endl;
	    return;
	}
    
    // create hitpair and initialize transformations using database
    MatchedHitPair hitPair(mStTpcDb);
 
    // loop over hits
    {for (unsigned int isec = 0; isec < mcTpcHitCollection->numberOfSectors(); isec++) 
	{
	    cout << "In Sector: " << isec + 1 << endl;
	    cout << "In Row: ";
	    for (unsigned int irow = 0; irow < mcTpcHitCollection->sector(isec)->numberOfPadrows(); irow++) 
		{
		    cout << irow + 1 << " "; flush(cout);
		    for (StMcTpcHitIterator hitIter = mcTpcHitCollection->sector(isec)->padrow(irow)->hits().begin();
			 hitIter != mcTpcHitCollection->sector(isec)->padrow(irow)->hits().end();
			 hitIter++) 
			{
			    // loop over remaining mc hits in this sector and padrow
			    for (StMcTpcHitIterator hitIter2 = hitIter; ++hitIter2 != mcTpcHitCollection->sector(isec)->padrow(irow)->hits().end();)
				{	
				    // access StAssociationMaker mc tpc hit map information
				    StMcTpcHit* mcTpcHit1 = *hitIter;
				    StMcTpcHit* mcTpcHit2 = *hitIter2;
				    // calculate distance
				    hitPair.resolution(mcTpcHit1, mcTpcHit2);
				    // fill histo
				    histograms.mcPadSepEfficiencyOuter->Fill(fabs(hitPair.resolution().x()), fabs(hitPair.resolution().z()));
				}
			}
		}
	    cout << " " << endl;
	}}
    
    /////
    //  rc hits
    ////
    // get rc hits
    StTpcHitCollection* rcTpcHitCollection = (!mL3TriggerOn) ? mStEvent->tpcHitCollection() : ml3TriggerEvent->tpcHitCollection() ;
  
    if (!rcTpcHitCollection) 
	{
	    cout << "--> StTpcEvalMaker warning: no StRcTpcHits found!" << endl;
	    return;
	}
    // loop over rc hits
    {for (unsigned int isec = 0; isec < rcTpcHitCollection->numberOfSectors(); isec++)
	{
	    cout << "In Sector: " << isec + 1 << endl;
	    cout << "In Row: ";
	    for (unsigned int irow = 0; irow < rcTpcHitCollection->sector(isec)->numberOfPadrows(); irow++) 
		{
		    cout << irow + 1 << " "; flush(cout);
		    for (StSPtrVecTpcHitIterator hitIter = rcTpcHitCollection->sector(isec)->padrow(irow)->hits().begin();
			 hitIter != rcTpcHitCollection->sector(isec)->padrow(irow)->hits().end();
			 hitIter++) 
			{
			    // loop over remaining rc hits in this sector and padrow
			    for (StSPtrVecTpcHitIterator hitIter2 = hitIter; ++hitIter2 != rcTpcHitCollection->sector(isec)->padrow(irow)->hits().end();)
				{
				    
				    // access StAssociationMaker recon tpc hit map information
				    StTpcHit* rcTpcHit1 = *hitIter;
				    StTpcHit* rcTpcHit2 = *hitIter2;
				    // calculate distance
				    hitPair.resolution(rcTpcHit1, rcTpcHit2);
				    // fill histogram
				    histograms.rcPadSepEfficiencyOuter->Fill(fabs(hitPair.resolution().x()), fabs(hitPair.resolution().z()));
				}
			}
		}
	    cout << " " << endl;
	}}
}
//-----------------------------------------------------------------------
