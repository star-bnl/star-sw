// include files 
#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif
#include "StRichSimpleUst.h"

#include "StEventTypes.h"
#include "StEvent/StRichPixel.h"
#include "StEvent/StRichHit.h"
#include "StEvent/StRichCluster.h"
#include "StEvent/StRichPidTraits.h"
#include "StEvent/StRichPid.h"
#include "StEvent/StTrackTopologyMap.h"
#include "StEvent/StRichPhotonInfo.h"

#include "StThreeVector.hh"
#include "StRrsMaker/StGlobalCoordinate.h"
#include "StRrsMaker/StRichRawCoordinate.h"
#include "StRrsMaker/StRichCoordinateTransform.h"
#include "StRrsMaker/StRichLocalCoordinate.h"
#include "StRrsMaker/StRichMomentumTransform.h"
#include "StRrsMaker/StRichGeometryDb.h"

#include "StRichUstStruct.h"
#include "StRichUstTrack.h"
#include "StRichUstPixel.h"
#include "StRichUstHit.h"
#include "StRichUstPhoton.h"
#include "StEventUtilities/StuRefMult.hh"

#include "StIOMaker/StIOMaker.h"
#include "StTreeMaker/StTreeMaker.h"

#include "StStrangeMuDstMaker/StStrangeMuDstMaker.h"
#include "StStrangeMuDstMaker/StV0MuDst.hh"
#include <math.h>
#include <vector>
#include <map>
#include <algorithm>
#include <functional>

static const char rcsid[] = "$Id: StRichSimpleUst.cxx,v 1.2 2003/04/30 20:38:12 perev Exp $";

Int_t StRichSimpleUst::Make() 
{ 
    StIOMaker* IO=(StIOMaker*)GetMaker("IO");
    if(IO) mEventFile=strrchr(IO->GetFile(),'/')+1;
    else
    {
      //try to get from StTreeMaker
      StTreeMaker *tree=(StTreeMaker*)GetMaker("outputStream");
      if(tree)
      {
        StTree *t = tree->GetTree();;
        mEventFile=t->GetBaseName();
      }
    }
    
    // if the input event file has changed, close the Micro dst file and creates another one
    if(mEventFile!=mEventFileOld)
    {
	if(mFile && mFile->IsOpen())
	{
	    mFile->Write(0,TObject::kOverwrite);
	    mFile->Close();
	}
//	cout << "Deleting mTrack" << endl;
	
//	if (mTrack)   delete mTrack;
	cout << "Deleting mFile" << endl;
	
	if (mFile) delete mFile;
	mSimpleUstTree=0;
	mFile=0;
	initMicroEventFile();
	mEventFileOld=mEventFile;
    }

    cout << "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" << endl;
    cout << "inside StRichSimpleUst ..." << endl;
    cout << "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" << endl;

    //  StRichGeometryDb* myGeometryDb = StRichGeometryDb::getDb();
    StRichCoordinateTransform* trans = StRichCoordinateTransform::getTransform(mGeometryDb);

    StRichMomentumTransform *ptrans = StRichMomentumTransform::getTransform(mGeometryDb);
    // grab StEvent
    StEvent* mEvent;
    mEvent = (StEvent *) GetInputDS("StEvent");

    if (!mEvent) {
	cout << "No mEvent! Can not continue. " << endl;
	return kStOk; // If no event, we're done
    }
    
    if (!(mEvent->primaryVertex())) {
	cout << "No primary vertex." << endl;
//	return kStOk;
	
    }
    const StPrimaryVertex* theVertex = mEvent->primaryVertex();
//    if (fabs(theVertex->position().z()) > 60.) {
    
    if (theVertex) {
	
	cout << "Vertex: " << theVertex->position().z() <<
	    endl;
    }

    Long_t theEventId = mEvent->id();
    Long_t theRunId = mEvent->runId();
    Long_t theTriggerMask = mEvent->triggerMask();
    
    const StL0Trigger * theL0 = mEvent->l0Trigger();
    Long_t theTriggerActionWord = -999;
    Long_t theTriggerWord = -999;
    
    if (theL0) {
	theTriggerActionWord = theL0->triggerActionWord();
	theTriggerWord = theL0->triggerWord();
	
	cout << "theTriggerWord = " << hex << theTriggerWord 
	     << "theTriggerActionWord = " << hex << theTriggerActionWord 
	     << "theTriggerMask = " << hex << theTriggerMask << dec << endl;
	
    }
    // Get the CTB's
    Float_t ctbsum = 0;
    Float_t zdcsum = 0;
    Float_t ctbpre = 0;
    
    if (mEvent->triggerDetectorCollection()) {
      
	StCtbTriggerDetector& ctb = mEvent->triggerDetectorCollection()->ctb();
      
	for (unsigned int i=0; i< ctb.numberOfTrays(); ++i) {
	    for (unsigned int j=0; j< ctb.numberOfSlats(); ++j) {
		ctbsum += ctb.mips(i,j,0);
		ctbpre += ctb.mips(i,j,ctb.numberOfPreSamples());
	    }
	}
    	StZdcTriggerDetector& theZdc = mEvent->triggerDetectorCollection()->zdc();
	zdcsum = theZdc.adc(10)+theZdc.adc(13);
  
    }
    else {
	cout << "No trigger detectors " << endl;
    }
    cout << "ctb sum =" << ctbsum << endl;

    Int_t l3Triggered = -999;
    Int_t l3Available = 0;
    
    // Get the l3
    if(mEvent->l3Trigger()) {
	if (mEvent->l3Trigger()->l3EventSummary()) {
	    l3Available = 1;
	    cout << "Found L3 " << endl;

	    if (mEvent->l3Trigger()->l3EventSummary()->unbiasedTrigger()) {
		l3Triggered = 0;
		
	    }
	    else {
		l3Triggered = 1;
	    }
	}
    }
    if (l3Triggered == 1)  cout << "l3 triggered " << endl;
    
    // define RICH position
    double norm_x = mGeometryDb->normalVectorToPadPlane().x();
    double norm_y = mGeometryDb->normalVectorToPadPlane().y();
    double norm_z = mGeometryDb->normalVectorToPadPlane().z();
    StThreeVectorD richNormalTest(norm_x,norm_y,norm_z);
    richNormalTest.setMag(1.0);
    StThreeVectorD richNormal(norm_x,norm_y,norm_z);
 
// Pad plane
    StRichLocalCoordinate richPadLocal(0.,0.,0.);
    StGlobalCoordinate richPadGlobal;
    (*trans)(richPadLocal,richPadGlobal);
    StThreeVectorD richPad(richPadGlobal.position().x(),
			   richPadGlobal.position().y(),
			   richPadGlobal.position().z());
    
			   
// Anode wires
    StRichLocalCoordinate richAnodeLocal(0.,0.,mGeometryDb->anodeToPadSpacing());
    StGlobalCoordinate richAnodeGlobal;
    (*trans)(richAnodeLocal,richAnodeGlobal);

  
    double origin_x = richAnodeGlobal.position().x();
    double origin_y = richAnodeGlobal.position().y();
    double origin_z = richAnodeGlobal.position().z();

    StThreeVectorD richAnode(origin_x,origin_y,origin_z);
// Radiator
    StRichLocalCoordinate richRadiatorLocal(0,0,
					    mGeometryDb->proximityGap() +
					    mGeometryDb->quartzDimension().z()+
					    mGeometryDb->radiatorDimension().z());
    StGlobalCoordinate richRadiatorGlobal;
    (*trans)(richRadiatorLocal,richRadiatorGlobal);
    double radx = richRadiatorGlobal.position().x();
    double rady = richRadiatorGlobal.position().y();
    double radz = richRadiatorGlobal.position().z();
	
    StThreeVectorD richRadiator(radx,rady,radz);
  
    StRichCollection* theCollection = mEvent->richCollection();

    if (!theCollection) {
	cout << "No Rich Collection!" << endl;
	return kStOk;
    }

    const StSPtrVecRichPixel& thePixels =
	theCollection->getRichPixels();
  
    const StSPtrVecRichHit& theHits =
	theCollection->getRichHits();

    const StSPtrVecRichCluster& theClusters =
	theCollection->getRichClusters();
 
    cout << "Pixels size" << thePixels.size() << endl;
    cout << "Clusters size" << theClusters.size() << endl;
    cout << "Hits size" << theHits.size() << endl;
    if (!(thePixels.size())) return kStOk;
    
    
    const StSPtrVecTrackNode& theNodes = mEvent->trackNodes();
    cout << "Got the trackNodes" << endl;
    
    

    vector<StTrack*> gvec;

    vector<StTrack*> pvec;
   
    vector<int> gindexforprimary;
    vector<int> pindexforglobal;
    vector<int> hindexforprimary;
    
    typedef map<unsigned int, StV0MuDst*> lambdaMapType;
    
    lambdaMapType lambdaProtonDaughters = 
	this->getLambdas(mEvent);

    
// Kludge	
    double mMagField    = 2.49117;    
    if (mEvent->summary()) {
	mMagField  = mEvent->summary()->magneticField();
	cout << "  B field = " << mMagField << endl;
    } 
    else {
	cout << "\tField not found.  Use B= " << mMagField << endl;
    }
    double Bmag = mMagField*kilogauss; // kludge
		    

// Push back the globals and primaries
// Filtering changed
// Assumes we keep the globals!
    if (theVertex) {
	
	
	for (size_t nodeIndex=0; nodeIndex<theNodes.size(); ++nodeIndex) {
	    int foundGlobal = -999;
	    int foundPrimary = -999;
//VPunused  size_t numberOfGlobalsInNode = theNodes[nodeIndex]->entries(global);
	    size_t numberOfPrimariesInNode = theNodes[nodeIndex]->entries(primary);

	    for (size_t primaryIndex = 0; primaryIndex<numberOfPrimariesInNode;
		 ++primaryIndex) {
		
		StTrack* theTrack = 
		    theNodes[nodeIndex]->track(primary,primaryIndex);
		StRichPidTraits* lastPidTrait = 0;
		
		StSPtrVecTrackPidTraits& traits = theTrack->pidTraits();
		for (StSPtrVecTrackPidTraitsIterator traitIter=traits.begin();
		     traitIter!=traits.end();++traitIter) {
		    
		    StRichPidTraits* pidTrait = dynamic_cast<StRichPidTraits*>(*traitIter);
		    
		    if (pidTrait) {
			lastPidTrait = pidTrait;
		    }
		}

		if (lastPidTrait) {
		    pvec.push_back(theTrack);
		    foundPrimary = pvec.size();
		    gvec.push_back(theTrack->node()->track(global,0));
		    foundGlobal = gvec.size();
		}
		
	    } // loop over primaries
	    if (foundPrimary>0 && foundGlobal>0) {
		pindexforglobal.push_back(foundPrimary);
		gindexforprimary.push_back(foundGlobal);
		
		
		cout << "Found primary+global" << endl;
	    }
	    if (foundPrimary<0 && foundGlobal>0) {
		pindexforglobal.push_back(foundPrimary);
		cout << "Found global, no primary" << endl;
		
		
	    }
	    if (foundPrimary>0 && foundGlobal<0) {
		gindexforprimary.push_back(foundGlobal);
		cout << "Found primary, no global" << endl;
	    }
	} // end of track loop
    } // Vertex
    
    cout << "Found event:  "<< theEventId << " nglobals = " << gvec.size()  
	 << " nprimaries = " << pvec.size() <<endl;
    
    // Fill up the tree
    
    mRichUstStruct->SetL3Triggered(l3Triggered);
    mRichUstStruct->SetL3Available(l3Available);
    
//    mRichUstStruct->SetNPrimaries(uncorrectedNumberOfPrimaries(*mEvent));
//    mRichUstStruct->SetNNegPrimaries(uncorrectedNumberOfNegativePrimaries(*mEvent));
    if (mEvent->summary() && mEvent->summary()->numberOfExoticTracks()<0) {
	mRichUstStruct->SetNPrimaries(mEvent->summary()->numberOfGoodTracks());
	mRichUstStruct->SetNNegPrimaries(mEvent->summary()->numberOfGoodTracks(negative));
    }
    else {
	mRichUstStruct->SetNPrimaries(-999);
	
	mRichUstStruct->SetNNegPrimaries(-999);
	
    }
    if (mEvent->runInfo()) {
	mRichUstStruct->SetBackgroundRate(mEvent->runInfo()->backgroundRate());
	mRichUstStruct->SetZDCEastRate(mEvent->runInfo()->zdcEastRate());
	mRichUstStruct->SetZDCWestRate(mEvent->runInfo()->zdcWestRate());
	mRichUstStruct->SetZDCCoincidenceRate(mEvent->runInfo()->zdcCoincidenceRate());

    }
    else {
	
	mRichUstStruct->SetBackgroundRate(-999);
	
	mRichUstStruct->SetZDCEastRate(-999);
	
	mRichUstStruct->SetZDCWestRate(-999);
	mRichUstStruct->SetZDCCoincidenceRate(-999);

	

    }

    
	
    mRichUstStruct->SetNCTB((int)ctbsum);
    mRichUstStruct->SetNCTBpre((int)ctbpre);

    mRichUstStruct->SetZdcSum(zdcsum);
    mRichUstStruct->SetTriggerWord(theTriggerWord);
    mRichUstStruct->SetTriggerActionWord(theTriggerActionWord);
    mRichUstStruct->SetTriggerMask(theTriggerMask);
    if (theVertex) {
	
	mRichUstStruct->SetVertX(theVertex->position().x());
	mRichUstStruct->SetVertY(theVertex->position().y());
	mRichUstStruct->SetVertZ(theVertex->position().z());
    }
    else {
	mRichUstStruct->SetVertX(-999);
	
	mRichUstStruct->SetVertY(-999);
	mRichUstStruct->SetVertZ(-999);
    }
    
    mRichUstStruct->SetEvID(theEventId  );
    mRichUstStruct->SetRunID(theRunId       );
// Pack up the pixels
    mRichUstStruct->GetFPixels()->Expand(thePixels.size());
    mRichUstStruct->SetNPixels(0);
    
    
    for (StSPtrVecRichPixelConstIterator iter = thePixels.begin();
	 iter != thePixels.end(); ++iter) {
	StRichUstPixel blah;
	blah.SetCharge((*iter)->adc());
	blah.SetRawX((*iter)->pad());
	blah.SetRawY((*iter)->row());
	StRichRawCoordinate theRawPixel((*iter)->pad(),
					(*iter)->row());
	StRichLocalCoordinate theLocalPixel;
	(*trans)(theRawPixel,theLocalPixel);
	blah.SetLocalX(theLocalPixel.position().x());
	blah.SetLocalY(theLocalPixel.position().y());
	mRichUstStruct->AddPixel(blah);
    }
    

    vector <StRichUstPhoton> theUstPhotons;
    mRichUstStruct->GetFRichGlobals()->Expand(gvec.size());
    mRichUstStruct->SetNRichGlobals(0);
    
    for (size_t iTrack = 0; iTrack < gvec.size(); ++iTrack) {
	
	StTrack* theTrack = gvec[iTrack];

	
	StRichUstTrack blah;
	StTrackGeometry* theGeometry = (theTrack->outerGeometry()) ?
	    theTrack->outerGeometry() : theTrack->geometry();
	
	StThreeVectorD theMomentum;
	if (fabs(Bmag)>0.1*kilogauss) {
	    theMomentum = theGeometry->momentum();
	}
	else {
	    theMomentum = theGeometry->helix().at(1) -
		theGeometry->helix().at(0);
	}
	

	blah.SetTrackFlag(theTrack->flag());
	
	blah.SetTrackQ(theGeometry->charge());
	blah.SetNFitPoints(theTrack->fitTraits().numberOfFitPoints());

	blah.SetInnerPx(theTrack->geometry()->momentum().x());
	blah.SetInnerPy(theTrack->geometry()->momentum().x());
	blah.SetInnerPz(theTrack->geometry()->momentum().x());
	blah.SetInnerP(theTrack->geometry()->momentum().mag());
	blah.SetInnerPt(theTrack->geometry()->momentum().perp());
	
	blah.SetGlobalPx(theMomentum.x());
	
	blah.SetGlobalPy(theMomentum.y());
	blah.SetGlobalPz(theMomentum.z());
	blah.SetP(theGeometry->momentum().mag());
	
	blah.SetDCA(theTrack->impactParameter());
	blah.SetEta(theMomentum.pseudoRapidity());
	

	blah.SetOriginX(theGeometry->origin().x());
	blah.SetOriginY(theGeometry->origin().y());
	blah.SetOriginZ(theGeometry->origin().z());
	blah.SetCurvature(theGeometry->curvature());
	blah.SetPsi(theGeometry->psi());
	blah.SetOtherIndex(pindexforglobal[iTrack]);

	blah.SetIsLambda(0);
	
	lambdaMapType::iterator lamIter = lambdaProtonDaughters.find(theTrack->key());
	if (lamIter != lambdaProtonDaughters.end()) {
	    blah.SetAlphaV0((*lamIter).second->alphaV0());
	    if ((*lamIter).second->alphaV0() > 0) {
		blah.SetLambdaMass((*lamIter).second->massLambda());
	    }
	    else {
		blah.SetLambdaMass((*lamIter).second->massAntiLambda());
	    }
	    blah.SetIsLambda(1);
	}
	
	// get the first and last points
	const StThreeVectorF& theFirstPoint = theTrack->detectorInfo()->firstPoint();
	blah.SetFirstX(theFirstPoint.x());
	blah.SetFirstY(theFirstPoint.y());
	blah.SetFirstZ(theFirstPoint.z());

	const StThreeVectorF& theLastPoint = theTrack->detectorInfo()->lastPoint();
	blah.SetLastX(theLastPoint.x());
	blah.SetLastY(theLastPoint.y());
	blah.SetLastZ(theLastPoint.z());
	
	Double_t vertPath = theGeometry->helix().pathLength(theVertex->position());
	blah.SetDCAx(theGeometry->helix().x(vertPath));
	blah.SetDCAy(theGeometry->helix().y(vertPath));
	blah.SetDCAz(theGeometry->helix().z(vertPath));
	blah.SetDCA3d(theGeometry->helix().distance(theVertex->position()));
	// Set the sign
	double theVS = theGeometry->helix().pathLength(
	    theVertex->position().x(),theVertex->position().y());
	StThreeVectorD DCA2d_pos = theGeometry->helix().at(theVS);
	DCA2d_pos.setZ(0);
	StThreeVectorD vert2d(theVertex->position().x(),
			      theVertex->position().y(),0);
	
	StThreeVectorD DCAVec = (DCA2d_pos-vert2d);
	StThreeVectorD momVec;
	
	if (fabs(theGeometry->helix().curvature()) <=
	    static_cast<double>(0) || fabs(Bmag)<0.1*kilogauss) {
	    momVec = theMomentum;
	    momVec.setZ(0);
	    
	}
	else {
	    momVec =
		theGeometry->helix().momentumAt(
		    theVS,Bmag);
	    momVec.setZ(0);
	    
	}
	
	
	double cross = DCAVec.x()*momVec.y() -
	    DCAVec.y()*momVec.x();
	
	
	double theSign = (cross>=0) ? 1. : -1.;
	
	double DCA2d = theSign*DCAVec.perp();
	
	if (DCA2d<0) {
	    theSign = -1;
	}
	else {
	    theSign = 1;
	}
	blah.SetGSDCA(theSign*(theGeometry->helix().distance(theVertex->position())));
	blah.SetGSDCA2d(DCA2d);
	

	double anodePath = theGeometry->helix().pathLength(richAnode,richNormal);
	

	StThreeVectorD gaP;
	if (fabs(Bmag)>0.1*kilogauss) {
	    gaP =  theGeometry->helix().momentumAt(anodePath,Bmag);
	}
	else {
	    gaP = theMomentum;
	}
	
	StThreeVector<double> gaPstl(gaP.x(),gaP.y(),gaP.z());
	StThreeVector<double> laP;
	    
	ptrans->localMomentum(gaPstl,laP);
	blah.SetLocalPadPx(laP.x());
	blah.SetLocalPadPy(laP.y());
	blah.SetLocalPadPz(laP.z());
	
	double anode_x =theGeometry->helix().x(anodePath);
	double anode_y =theGeometry->helix().y(anodePath);
	double anode_z =theGeometry->helix().z(anodePath);
	StGlobalCoordinate anodeImpactGlobal(anode_x,anode_y,anode_z);
	StRichLocalCoordinate anodeImpactLocal;
	(*trans)(anodeImpactGlobal,anodeImpactLocal);

	blah.SetLocalPadX(anodeImpactLocal.position().x());
	blah.SetLocalPadY(anodeImpactLocal.position().y());
	blah.SetLocalPadZ(anodeImpactLocal.position().z());
	blah.SetGlobalPadX(anode_x);
	blah.SetGlobalPadY(anode_y);
	blah.SetGlobalPadZ(anode_z);
	

// Do the radiator thing.  Tweak it for now
	double radPath =  
	    theGeometry->helix().pathLength(richRadiator,richNormal);
	StThreeVectorD rP;
	if (fabs(Bmag)>0.1*kilogauss) {
	    rP = theGeometry->helix().momentumAt(radPath,Bmag);
	}
	else {
	    rP = theMomentum;
	}
	
	StThreeVector<double> rPstl(rP.x(),rP.y(),rP.z());
	StThreeVector<double> lrP;
	    
	ptrans->localMomentum(rPstl,lrP);
	blah.SetLocalRadPx(lrP.x());
	blah.SetLocalRadPy(lrP.y());
	blah.SetLocalRadPz(lrP.z());


	double rx =theGeometry->helix().x(radPath);
	double ry =theGeometry->helix().y(radPath);
	double rz =theGeometry->helix().z(radPath);
	StGlobalCoordinate rG(rx,ry,rz);
	StRichLocalCoordinate rlr;
	(*trans)(rG,rlr);

	blah.SetLocalRadX(rlr.position().x());
	blah.SetLocalRadY(rlr.position().y());
	blah.SetLocalRadZ(rlr.position().z());
// Get LAST pidtrait 

	StRichPidTraits* lastPidTrait = 0;
	
	StSPtrVecTrackPidTraits& traits = theTrack->pidTraits();
	for (StSPtrVecTrackPidTraitsIterator traitIter=traits.begin();
	     traitIter!=traits.end();++traitIter) {
	    
	    StRichPidTraits* pidTrait = dynamic_cast<StRichPidTraits*>(*traitIter);
	    
	    if (pidTrait) {
		lastPidTrait = pidTrait;
	    }
	}

	if (lastPidTrait) {
	    // Find Mip
	    const StRichHit* theMip = lastPidTrait->associatedMip();
	    const StThreeVectorF& theMipResidual = lastPidTrait->mipResidual();
	    blah.SetResidX(theMipResidual.x());
	    blah.SetResidY(theMipResidual.y());
	    blah.SetResidZ(theMipResidual.z());
	    blah.SetResid(theMipResidual.perp());

	    for (size_t iHit = 0; iHit < theHits.size(); ++iHit) {
		if (theHits[iHit] == theMip) blah.SetMipIndex(iHit);
	    }

	    if (lastPidTrait->getRichSpectra()) {
		StRichSpectra *theSpectra = lastPidTrait->getRichSpectra();
		blah.SetSpX(theSpectra->getExtrapolatedX());
		blah.SetSpY(theSpectra->getExtrapolatedY());
		blah.SetSpDx(theSpectra->getExtrapolatedXResidual());
		blah.SetSpDy(theSpectra->getExtrapolatedYResidual());
		blah.SetSpCdx(theSpectra->getCorrectedExtrapolatedXResidual());
		blah.SetSpCdy(theSpectra->getCorrectedExtrapolatedYResidual());
		blah.SetSpMass2(theSpectra->getMassSquared());
		blah.SetSpCherenkovAngle(theSpectra->getCherenkovAngle());
		blah.SetSpNphotons(theSpectra->getCherenkovPhotons());
	    }
	} // if pidtrait

	mRichUstStruct->AddRichGlobal(blah);
	
    }
    cout << "Added globals" << endl;
    
    mRichUstStruct->GetFRichPrimaries()->Expand(pvec.size());
    mRichUstStruct->SetNRichPrimaries(0);

    
    for (size_t iTrack = 0; iTrack < pvec.size(); ++iTrack) {
	
	StTrack* theTrack = pvec[iTrack];
	StTrackGeometry* theGeometry = (theTrack->outerGeometry()) ?
	    theTrack->outerGeometry() : theTrack->geometry();
	
	StRichUstTrack blah;
	StThreeVectorD theMomentum;
	if (fabs(Bmag)>0.1*kilogauss) {
	    theMomentum = theGeometry->momentum();
	}
	else {
	    theMomentum = theGeometry->helix().at(1) -
		theGeometry->helix().at(0);
	}

	blah.SetTrackFlag(theTrack->flag());
	
	blah.SetTrackQ(theGeometry->charge());
	blah.SetNFitPoints(theTrack->fitTraits().numberOfFitPoints());
	blah.SetInnerPx(theTrack->geometry()->momentum().x());
	blah.SetInnerPy(theTrack->geometry()->momentum().x());
	blah.SetInnerPz(theTrack->geometry()->momentum().x());
	blah.SetInnerP(theTrack->geometry()->momentum().mag());
	blah.SetInnerPt(theTrack->geometry()->momentum().perp());

	blah.SetGlobalPx(theMomentum.x());
	
	blah.SetGlobalPy(theMomentum.y());
	blah.SetGlobalPz(theMomentum.z());
	blah.SetP(theMomentum.mag());
	
	
	blah.SetDCA(theTrack->impactParameter());
	blah.SetEta(theMomentum.pseudoRapidity());
	
	blah.SetOriginX(theGeometry->origin().x());
	blah.SetOriginY(theGeometry->origin().y());
	blah.SetOriginZ(theGeometry->origin().z());
	blah.SetCurvature(theGeometry->curvature());
	blah.SetPsi(theGeometry->psi());
	blah.SetOtherIndex(gindexforprimary[iTrack]);
	if (gindexforprimary[iTrack]>=0) {
	    TClonesArray* theGlobals = mRichUstStruct->GetFRichGlobals();
	    
	    StRichUstTrack* theUstTrack = static_cast<StRichUstTrack*>((*theGlobals)[gindexforprimary[iTrack]-1]);
	    Float_t theSDCA = theUstTrack->GetGSDCA();
	    
	    
	    blah.SetGSDCA(theSDCA);
	    Float_t theSDCA2d = theUstTrack->GetGSDCA2d();
	    blah.SetGSDCA2d(theSDCA2d);
	    
	}
	blah.SetIsLambda(0);
	
	lambdaMapType::iterator lamIter = lambdaProtonDaughters.find(theTrack->key());
	if (lamIter != lambdaProtonDaughters.end()) {
	    blah.SetAlphaV0((*lamIter).second->alphaV0());
	    if ((*lamIter).second->alphaV0() > 0) {
		blah.SetLambdaMass((*lamIter).second->massLambda());
	    }
	    else {
		blah.SetLambdaMass((*lamIter).second->massAntiLambda());
	    }
 
	    blah.SetIsLambda(1);
	}

	
	// get the first and last points
	const StThreeVectorF& theFirstPoint = theTrack->detectorInfo()->firstPoint();
	blah.SetFirstX(theFirstPoint.x());
	blah.SetFirstY(theFirstPoint.y());
	blah.SetFirstZ(theFirstPoint.z());

	const StThreeVectorF& theLastPoint = theTrack->detectorInfo()->lastPoint();
	blah.SetLastX(theLastPoint.x());
	blah.SetLastY(theLastPoint.y());
	blah.SetLastZ(theLastPoint.z());

	Double_t vertPath = theGeometry->helix().pathLength(theVertex->position());
	blah.SetDCAx(theGeometry->helix().x(vertPath));
	blah.SetDCAy(theGeometry->helix().y(vertPath));
	blah.SetDCAz(theGeometry->helix().z(vertPath));
	blah.SetDCA3d(theGeometry->helix().distance(theVertex->position()));

       

	double anodePath = theGeometry->helix().pathLength(richAnode,richNormal);
	
	StThreeVectorD gaP;
	if (fabs(Bmag)>0.1*kilogauss) {
	    gaP =  theGeometry->helix().momentumAt(anodePath,Bmag);
	}
	else {
	    gaP = theMomentum;
	}
	
	StThreeVector<double> gaPstl(gaP.x(),gaP.y(),gaP.z());
	StThreeVector<double> laP;
	    
	ptrans->localMomentum(gaPstl,laP);
	blah.SetLocalPadPx(laP.x());
	blah.SetLocalPadPy(laP.y());
	blah.SetLocalPadPz(laP.z());

	double anode_x =theGeometry->helix().x(anodePath);
	double anode_y =theGeometry->helix().y(anodePath);
	double anode_z =theGeometry->helix().z(anodePath);
	StGlobalCoordinate anodeImpactGlobal(anode_x,anode_y,anode_z);
	StRichLocalCoordinate anodeImpactLocal;
	(*trans)(anodeImpactGlobal,anodeImpactLocal);

 
	blah.SetLocalPadX(anodeImpactLocal.position().x());
	blah.SetLocalPadY(anodeImpactLocal.position().y());
	blah.SetLocalPadZ(anodeImpactLocal.position().z());
	blah.SetGlobalPadX(anode_x);
	blah.SetGlobalPadX(anode_y);
	blah.SetGlobalPadX(anode_z);
	
	
// Do the radiator thing.  Tweak it for now
	double radPath =  
	    theGeometry->helix().pathLength(richRadiator,richNormal);
	StThreeVectorD rP;
	if (fabs(Bmag)>0.1*kilogauss) {
	    rP = theGeometry->helix().momentumAt(radPath,Bmag);
	}
	else {
	    rP = theMomentum;
	}
	
	StThreeVector<double> rPstl(rP.x(),rP.y(),rP.z());
	StThreeVector<double> lrP;
	    
	ptrans->localMomentum(rPstl,lrP);
	blah.SetLocalRadPx(lrP.x());
	blah.SetLocalRadPy(lrP.y());
	blah.SetLocalRadPz(lrP.z());


	double rx =theGeometry->helix().x(radPath);
	double ry =theGeometry->helix().y(radPath);
	double rz =theGeometry->helix().z(radPath);
	StGlobalCoordinate rG(rx,ry,rz);
	StRichLocalCoordinate rlr;
	(*trans)(rG,rlr);

	blah.SetLocalRadX(rlr.position().x());
	blah.SetLocalRadY(rlr.position().y());
	blah.SetLocalRadZ(rlr.position().z());
			
// Get LAST pidtrait 

	StRichPidTraits* lastPidTrait = 0;
	
	StSPtrVecTrackPidTraits& traits = theTrack->pidTraits();
	for (StSPtrVecTrackPidTraitsIterator traitIter=traits.begin();
	     traitIter!=traits.end();++traitIter) {
	    
	    StRichPidTraits* pidTrait = dynamic_cast<StRichPidTraits*>(*traitIter);
	    
	    if (pidTrait) {
		lastPidTrait = pidTrait;
	    }
	}

	if (lastPidTrait) {
	    // Find Mip
	    const StRichHit* theMip = lastPidTrait->associatedMip();
	    const StThreeVectorF& theMipResidual = lastPidTrait->mipResidual();
	    blah.SetResidX(theMipResidual.x());
	    blah.SetResidY(theMipResidual.y());
	    blah.SetResidZ(theMipResidual.z());
	    blah.SetResid(theMipResidual.perp());

	    for (size_t iHit = 0; iHit < theHits.size(); ++iHit) {
		if (theHits[iHit] == theMip) blah.SetMipIndex(iHit);
	    }

	    if (lastPidTrait->getRichSpectra()) {
		StRichSpectra *theSpectra = lastPidTrait->getRichSpectra();
		blah.SetSpX(theSpectra->getExtrapolatedX());
		blah.SetSpY(theSpectra->getExtrapolatedY());
		blah.SetSpDx(theSpectra->getExtrapolatedXResidual());
		blah.SetSpDy(theSpectra->getExtrapolatedYResidual());
		blah.SetSpCdx(theSpectra->getCorrectedExtrapolatedXResidual());
		blah.SetSpCdy(theSpectra->getCorrectedExtrapolatedYResidual());
		blah.SetSpMass2(theSpectra->getMassSquared());
		blah.SetSpCherenkovAngle(theSpectra->getCherenkovAngle());
		blah.SetSpNphotons(theSpectra->getCherenkovPhotons());
	    }
	    

	} // if pidtrait
	
	mRichUstStruct->AddRichPrimary(blah);
	
    }

    
    // pack up the rich hits 
    mRichUstStruct->GetFHits()->Expand(theHits.size());
    mRichUstStruct->SetNHits(0);
		
    for (StSPtrVecRichHitConstIterator iter = theHits.begin();
	 iter != theHits.end(); ++iter) {
	StRichUstHit blah;
	blah.SetCharge((int)(*iter)->charge());
	blah.SetMaxAdc((int)(*iter)->maxAmplitude());
	blah.SetNPads((*iter)->numberOfPads());
	blah.SetFlag((*iter)->reservedLong());
	
	UInt_t theClusterNumber = (*iter)->clusterNumber();
	blah.SetClusterNumber(theClusterNumber);
	blah.SetClusterFirstPad(theClusters[theClusterNumber]->firstPad());
	blah.SetNClusterPads(theClusters[theClusterNumber]->numberOfPads());
	blah.SetNClusterLocalMax(theClusters[theClusterNumber]->numberOfLocalMax());
	blah.SetClusterMinAmp((int)theClusters[theClusterNumber]->minimumAmplitudeOfLocalMax());
	

	blah.SetRawX((*iter)->internal().x());
	blah.SetRawY((*iter)->internal().y());
	blah.SetLocalX((*iter)->local().x());
	blah.SetLocalY((*iter)->local().y());
	blah.SetGlobalX((*iter)->position().x());
	blah.SetGlobalY((*iter)->position().y());
	blah.SetGlobalZ((*iter)->position().z());
	mRichUstStruct->AddHit(blah);
	
    }

    
    cout << "Filling tree" << endl;
    
    mSimpleUstTree->Fill();
    
    cout << "Clearing tracks" << endl;
    
    mRichUstStruct->ClearTracksAndPixels();
    cout << "Tracks cleared" << endl;
    
    
    return kStOk;
    
}




StRichSimpleUst::StRichSimpleUst(const Char_t *name) : StMaker(name) { 

    mEventCounter = 0;

}

StRichSimpleUst::~StRichSimpleUst() {}

Int_t StRichSimpleUst::Init() {
    cout << "In simpleUst" << endl;
    
    mGeometryDb = StRichGeometryDb::getDb();

    return 0; 
}

void StRichSimpleUst::Clear(Option_t *opt) 
{ 
    StMaker::Clear();
}

Int_t StRichSimpleUst::Finish()            
{ 
    if (mFile) {
	
	mFile->Write(0,TObject::kOverwrite);
	mFile->Close();
    }
    
    return kStOK; 
}

//const char* StRichSimpleUst::GetCVS() {static const char cvs[]="Tag $Name:  $ $Id: StRichSimpleUst.cxx,v 1.2 2003/04/30 20:38:12 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}




map<unsigned int,StV0MuDst*> StRichSimpleUst::getLambdas(StEvent* mEvent) 
{
    cout << " Getting Lambdas";
    
    //StSPtrVecTrackNode& nodes = mEvent->trackNodes();
    StStrangeMuDstMaker* strangeDst =
	(StStrangeMuDstMaker*) GetMaker("strangeMuDst");
    
      
    map<unsigned int,StV0MuDst*> ret;
    if (!strangeDst) return ret;
    cout << "Got the strangeMuDst" << endl;
    
    multimap<unsigned int,StV0MuDst*> theLambdas;
    vector<unsigned int> keys;
    double mLambda = 1.115684;
    
    for( Int_t j=0; j<strangeDst->GetNV0(); j++ ) {
	StV0MuDst *v0m = strangeDst->GetV0(j);
	
	if  (v0m &&
	     (v0m->dcaV0ToPrimVertex()<0.7)
	     &&
// Widen up the cut for now
//	     (abs(v0m->massLambda()-1.115684)<0.010) &&
	     (v0m->decayLengthV0()>5.0) &&
	     (v0m->topologyMapPos().numberOfHits(kTpcId)>15) &&
	     (v0m->topologyMapNeg().numberOfHits(kTpcId)>15) &&
	     (v0m->dcaV0Daughters()<0.75) 
	    )
	{
	    if (
		(v0m->alphaV0()>0) &&
		(fabs(v0m->massLambda()-mLambda)<0.010) &&
		(v0m->dcaNegToPrimVertex()>2.85) 
		)
	    {
		theLambdas.insert(make_pair(v0m->keyPos(),v0m));
		keys.push_back(v0m->keyPos());
	    }
	    if (
		(v0m->alphaV0()<0) &&
		(fabs(v0m->massAntiLambda()-mLambda)<0.010) &&
		(v0m->dcaPosToPrimVertex()>2.85) 
		)
	    {
		// find node
		theLambdas.insert(make_pair(v0m->keyNeg(),v0m));
		keys.push_back(v0m->keyNeg());
		
	    }
	}
    }
	
    
// Now unroll and sort
    sort(keys.begin(),keys.end());
    
    vector<unsigned int> sortkeys;
    unique_copy(keys.begin(),keys.end(),back_inserter(sortkeys));
    
    for (vector<unsigned int>::iterator keyIter = sortkeys.begin();
	 keyIter != sortkeys.end(); ++keyIter) {
	typedef multimap<unsigned int,StV0MuDst*>::iterator I;
	pair<I,I> result = theLambdas.equal_range(*keyIter);
	vector<StV0MuDst*> them;
       
	for (I mmiter = result.first; mmiter != result.second; ++mmiter) {
	    them.push_back((*mmiter).second);
	}
	if (them.size()>1) cout << "Found two lambdas for one track.  Keeping one closest to lambda mass" << endl;
	
	double theMass = 10e10;
	StV0MuDst* theV0 = 0;
	
	for (vector<StV0MuDst*>::iterator viter = them.begin();
	     viter != them.end(); ++viter) 
	{
	    double tempMass = (*viter)->massLambda() - mLambda;
	    if (tempMass < theMass) {
		theV0 = *viter;
	        theMass = tempMass;
	    }
	}
	cout << "lambda Mass = " << theMass << endl;
	
	ret.insert(make_pair(*keyIter,theV0));
    }
	
    return ret;
    
}

void StRichSimpleUst::initMicroEventFile() {
    //
    //  Output file.
    //  mFileName should contain a valid filename. Here
    //  we dump the file into the null device.
    //
    cout <<"Input file = "<<mEventFile.Data()<<endl;
    TString* filestring = new TString(mEventDir);
    filestring->Append(mEventFile);
    
    if(filestring->EndsWith(".event.root"))
    {
	Int_t size=filestring->Sizeof();
	filestring->Remove(size-12,11);
    }
    if(filestring->EndsWith(".evtsel.root"))
    {
	Int_t size=filestring->Sizeof();
	filestring->Remove(size-13,12);
    }
    if(filestring->EndsWith(".richtof.root"))
    {
	Int_t size=filestring->Sizeof();
	filestring->Remove(size-14,13);
    }

    
    filestring->Append(".richUst.root");
    cout <<"Output file = "<<filestring->Data()<<endl;
    
    
    //
    //  Define Ntuple.
    //  Keep the order TFile -> TNtuple
    //  

    mFile = new TFile(filestring->Data(),"RECREATE");
    mFile->SetCompressionLevel(9);
//    mFile->SetFormat(1);
    mRichUstStruct = new StRichUstStruct();
    
    mSimpleUstTree = new TTree("RichTree","Big Tree");
// Only set branch style if root version is greater than 3.01.05
#if ROOT_VERSION_CODE >= ROOT_VERSION(3,01,05)
    mSimpleUstTree->SetBranchStyle(0);
#endif 
    

    
    Int_t bufsize = 20000;
    Int_t split = 1;
    cout << "Making branches" << endl;
    
    mSimpleUstTree->Branch("StRichUstStruct","StRichUstStruct",
			   &mRichUstStruct,
			   bufsize,split);
    cout << "Branches Made" << endl;
    
    



    
    //
    //  Call Init() of the base class.
    //  Always leave this in.
    //
    cout << "Created mTrack" << endl;
    
}

ClassImp(StRichSimpleUst)








   
	    
    
