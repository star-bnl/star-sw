// include files 
#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif
#include "StRichLambdaSimpleUst.h"

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

#include "StRichLambdaUstStruct.h"
#include "StRichLambdaUstTrack.h"
#include "StRichLambdaUstPixel.h"
#include "StRichLambdaUstHit.h"
#include "StRichLambdaUstPhoton.h"
#include "StEventUtilities/StuRefMult.hh"

//#include "StMiniRichMultiplicity/StMiniRichMultiplicity.h"

#include "StStrangeMuDstMaker/StStrangeMuDstMaker.h"
#include "StStrangeMuDstMaker/StV0MuDst.hh"
#include <math.h>
#include <vector>
#include <map>
#include <algorithm>
#include <functional>
typedef pair<unsigned int, double> candidate;
bool operator<(const candidate &a, const candidate &b) {return (a.second<b.second);};

static const char rcsid[] = "$Id: StRichLambdaSimpleUst.cxx,v 1.1 2002/11/19 18:32:02 dunlop Exp $";

Int_t StRichLambdaSimpleUst::Make() 
{ 

    cout << "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" << endl;
    cout << "inside StRichLambdaSimpleUst ..." << endl;
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

// Don't worry about L3.  Doesn't exist in P00hm    
//      // Get the l3
//      if(mEvent->l3Trigger()) {
//  	if (mEvent->l3Trigger()->l3EventSummary()) {
//  	    l3Available = 1;
//  	    cout << "Found L3 " << endl;
	    
//  	    const StPtrVecL3AlgorithmInfo& theL3Algos = 
//  		mEvent->l3Trigger()->l3EventSummary()
//  		->algorithmsAcceptingEvent();
//  	    l3Triggered = 0;
//  	    for (StPtrVecL3AlgorithmInfoConstIterator theIter = 
//  		     theL3Algos.begin(); 
//  		 theIter != theL3Algos.end(); ++theIter) {
//  		if ((*theIter)->id() == 4) l3Triggered=1;
//  	    }
//  	}
//      }
//      if (l3Triggered == 1)  cout << "l3 triggered " << endl;
    
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
    lambdaMapType antiLambdaProtonDaughters = 
	this->getAntiLambdas(mEvent);
    lambdaMapType k0ShortDaughters = 
	this->getK0Shorts(mEvent);
    
    
// Kludge	
    double mMagField    = 2.49117;    
    if (mEvent->summary()) {
	mMagField  = mEvent->summary()->magneticField();
	cout << "  B field = " << mMagField << endl;
    } 
    else {
	cout << "\tField not found.  Use B= " << mMagField << endl;
    }
    double Bmag = mMagField; // kludge
		    

// Push back the globals and primaries
// Assumes we keep the globals!
    if (theVertex) {
	
	
	for (size_t nodeIndex=0; nodeIndex<theNodes.size(); ++nodeIndex) {
	    int foundGlobal = -999;
	    int foundPrimary = -999;
	    size_t numberOfGlobalsInNode = theNodes[nodeIndex]->entries(global);
	    size_t numberOfPrimariesInNode = theNodes[nodeIndex]->entries(primary);
	    
// Don't Filter	
	    for (size_t globalIndex = 0; globalIndex<numberOfGlobalsInNode;
		 ++globalIndex) {
		
		StTrack* gt = theNodes[nodeIndex]->track(global,globalIndex);
		gvec.push_back(gt);
		foundGlobal = gvec.size();
	    }
	    

	    for (size_t primaryIndex = 0; primaryIndex<numberOfPrimariesInNode;
		 ++primaryIndex) {
		StTrack* gt = theNodes[nodeIndex]->track(primary,primaryIndex);
		pvec.push_back(gt);
		foundPrimary = pvec.size();
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

//      StSPtrVecObject& theContent = mEvent->content();

//      StMiniRichMultiplicity* theMult = new StMiniRichMultiplicity;
//      StMiniRichMultiplicity* foundMult = 0;
    
    
//      cout << "Checking for MiniRich" << endl;
    
//      for (unsigned int i =0; i< theContent.size(); ++i) {
	
//  	if (theContent[i]) cout << theContent[i]->GetName();
	
//  	if (theContent[i] && 
//  	    strstr(theContent[i]->GetName(),theMult->GetName())
//  	    ){
	    
//  	    cout << "Found StMiniRichMultiplicity"<<endl;
//  	    foundMult = static_cast<StMiniRichMultiplicity*>(theContent[i]);
//  	    break;
//  	}

//      }
//      delete(theMult);
//      theMult = 0;
	      
//  //    mEvent->_lookup(theMult,mEvent->content());
//      if (foundMult) {
//  	mRichUstStruct->SetNNegPrimaries(foundMult->numberOfNegativePrimaries());
//  	mRichUstStruct->SetNPrimaries(foundMult->numberOfPrimaries());
//  	mRichUstStruct->SetFlowMult(foundMult->flowMult());
//      }
    if (mEvent->summary() && mEvent->summary()->numberOfExoticTracks() < 0) {
	mRichUstStruct->SetNPrimaries(mEvent->summary()->numberOfGoodTracks());
	mRichUstStruct->SetNNegPrimaries(mEvent->summary()->numberOfGoodTracks(negative));
    }
	
    else{
	
	mRichUstStruct->SetNPrimaries(uncorrectedNumberOfPrimaries(*mEvent));
	mRichUstStruct->SetNNegPrimaries(uncorrectedNumberOfNegativePrimaries(*mEvent));
    }
    
    mRichUstStruct->SetNCTB(ctbsum);
    mRichUstStruct->SetNCTBpre(ctbpre);

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
	StRichLambdaUstPixel blah;
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
    

    vector <StRichLambdaUstPhoton> theUstPhotons;
    mRichUstStruct->GetFRichGlobals()->Expand(gvec.size());
    mRichUstStruct->SetNRichGlobals(0);
    
    for (size_t iTrack = 0; iTrack < gvec.size(); ++iTrack) {
	
	StTrack* theTrack = gvec[iTrack];
	
	
	StRichLambdaUstTrack blah;
	/*	
///// begin
    // Get the PID traits, if there is an StrichPIDTrait:
	//    for (StSPtrVecTrackNodeConstIterator titer = theNodes.begin();
	//         titer != theNodes.end(); ++titer) {
	//       for (unsigned int ientry = 0; ientry < (*titer)->entries(); ++titer) {
	//	    StTrack *track = (*titer)->track(ientry);  
	    const StPtrVecTrackPidTraits& theRichPidTraits = theTrack->pidTraits(kRichId);
	    if(!theRichPidTraits.size()) continue;

//   	cout << " (" << theRichPidTraits.size() << ") Pid Traits.   p= ";
	// info from the traits	   //  Change to get productionVersion = 2

            StTrackPidTraits *theSelectedTrait = 0;
	    for (StPtrVecTrackPidTraitsConstIterator traititer=theRichPidTraits.begin(); 
                traititer != theRichPidTraits.end(); ++traititer) {
	        StRichPidTraits *p = dynamic_cast<StRichPidTraits*>(*traititer);
	        if (!p) continue;
	        if (!(p->productionVersion() == 2)) continue;
	        theSelectedTrait = *traititer;
		cout << "\n\n  ...  interator ...\n \n" <<  << endl;
	    }
	    
	    if(!theSelectedTrait) {
	       cout << "Error in the Selected Trait\nContinuing..." << endl;
	       continue;
	    }	    
	    StRichPidTraits *richPidTrait =
	    dynamic_cast<StRichPidTraits*>(theSelectedTrait);
	    StRichSpectra *theSpectra = richPidTrait->getRichSpectra();

	    if (theSpectra) {
	       blah.SetCherenkovAngle(theSpectra->getCherenkovAngle());
	       //blah.SetMassSquared(theSpectra->getMassSquared());
	       // blah.SetNPhotons(theSpectra->getNPhotons());
	    }
	    //       }
	    //    } 
///////  end
	*/       

       StTrackGeometry* theGeometry = theTrack->geometry();
// Not defined for P00hm
//(theTrack->outerGeometry()) ?
//	    theTrack->outerGeometry() : theTrack->geometry();
	
	StThreeVectorD theMomentum;
	if (fabs(Bmag)>0.1) {
	    theMomentum = theGeometry->momentum();
	}
	else {
	    theMomentum = theGeometry->helix().at(1) -
		theGeometry->helix().at(0);
	}
	

	blah.SetTrackFlag(theTrack->flag());
	
	blah.SetTrackQ(theGeometry->charge());
	blah.SetNFitPoints(theTrack->fitTraits().numberOfFitPoints());
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
	    static_cast<double>(0) || fabs(Bmag)<0.1) {
	    momVec = theMomentum;
	    momVec.setZ(0);
	    
	}
	else {
	    momVec =
		theGeometry->helix().momentumAt(
		    theVS,Bmag*kilogauss);
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
	if (fabs(Bmag)>0.1) {
	    gaP =  theGeometry->helix().momentumAt(anodePath,Bmag*kilogauss);
	}
	else {
	    gaP = theMomentum;
	}
	
	StThreeVector<double> gaPstl(gaP.x(),gaP.y(),gaP.z());
	StThreeVector<double> laP;
	    
	ptrans->localMomentum(gaPstl,laP);

	blah.SetGlobalPadPx(gaP.x());
	blah.SetGlobalPadPy(gaP.y());
	blah.SetGlobalPadPz(gaP.z());

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
	

// Do the radiator thing.  
	double radPath =  
	    theGeometry->helix().pathLength(richRadiator,richNormal);
	StThreeVectorD rP;
	if (fabs(Bmag)>0.1) {
	    rP = theGeometry->helix().momentumAt(radPath,Bmag*kilogauss);
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

	blah.SetGlobalRadPx(rP.x());
	blah.SetGlobalRadPy(rP.y());
	blah.SetGlobalRadPz(rP.z());


	double rx =theGeometry->helix().x(radPath);
	double ry =theGeometry->helix().y(radPath);
	double rz =theGeometry->helix().z(radPath);
	StGlobalCoordinate rG(rx,ry,rz);
	StRichLocalCoordinate rlr;
	(*trans)(rG,rlr);

	blah.SetLocalRadX(rlr.position().x());
	blah.SetLocalRadY(rlr.position().y());
	blah.SetLocalRadZ(rlr.position().z());
// Set if in Rich
	Int_t crossedRich = 0;
	
	if ( 
	    (
		(fabs(anodeImpactLocal.position().x()) < 
		 mGeometryDb->radiatorDimension().x()-2.) &&
		(fabs(anodeImpactLocal.position().y()) < 
		 mGeometryDb->radiatorDimension().y()-2.) &&
		(fabs(anodeImpactLocal.position().x()) > 
		 mGeometryDb->quadrantGapInX()/2.+2.0) &&
		(fabs(anodeImpactLocal.position().y()) > 
		 mGeometryDb->quadrantGapInY()/2.+2.0) )
	    &&
	    (
		(fabs(rlr.position().x()) < 
		 mGeometryDb->radiatorDimension().x()-2.) &&
		(fabs(rlr.position().y()) < 
		 mGeometryDb->radiatorDimension().y()-2.) &&
		(fabs(rlr.position().x()) > 
		 mGeometryDb->quadrantGapInX()/2.+2.0) &&
		(fabs(rlr.position().y()) > 
		 mGeometryDb->quadrantGapInY()/2.+2.0) ) )
	{
	    crossedRich = 1;
	}
	blah.SetCrossedRich(crossedRich);
	
// Now check if it's a lambda
	{
	    
	    blah.SetIsLambda(0);
	    
	    lambdaMapType::iterator lamIter = lambdaProtonDaughters.find(theTrack->key());
	    if (lamIter != lambdaProtonDaughters.end()) {
		blah.SetIsLambda(1);
		StV0MuDst* v0m = (*lamIter).second;
	    
		blah.SetLambdaMass(v0m->massLambda());
		blah.SetAlphaLambda(v0m->alphaV0());
		blah.SetPtArmLambda(v0m->ptArmV0());
		blah.SetGlobalXLambda(v0m->decayVertexV0X());
		blah.SetGlobalYLambda(v0m->decayVertexV0Y());
		blah.SetGlobalZLambda(v0m->decayVertexV0Z());
		blah.SetGlobalPxLambda(v0m->momV0X());
		blah.SetGlobalPyLambda(v0m->momV0Y());
		blah.SetGlobalPzLambda(v0m->momV0Z());
		// Find the other daughter
		UShort_t keyNeg = v0m->keyNeg();
		Int_t otherDaughter = -999;
		for (size_t idaught =0; idaught < gvec.size(); ++idaught) {
		    if (gvec[idaught]->key() == keyNeg) {
			otherDaughter = idaught;
			break;
		    }
		}
		blah.SetOtherLambdaDaughter(otherDaughter);
	    }
	}
	
// Now check if it's an antilambda
	{
	    
	    blah.SetIsAntiLambda(0);
	    
	    lambdaMapType::iterator lamIter = antiLambdaProtonDaughters.find(theTrack->key());
	    if (lamIter != antiLambdaProtonDaughters.end()) {
		blah.SetIsAntiLambda(1);
		StV0MuDst* v0m = (*lamIter).second;
	    
		blah.SetAntiLambdaMass(v0m->massAntiLambda());
		blah.SetAlphaAntiLambda(v0m->alphaV0());
		blah.SetPtArmAntiLambda(v0m->ptArmV0());
		blah.SetGlobalXAntiLambda(v0m->decayVertexV0X());
		blah.SetGlobalYAntiLambda(v0m->decayVertexV0Y());
		blah.SetGlobalZAntiLambda(v0m->decayVertexV0Z());
		blah.SetGlobalPxAntiLambda(v0m->momV0X());
		blah.SetGlobalPyAntiLambda(v0m->momV0Y());
		blah.SetGlobalPzAntiLambda(v0m->momV0Z());
		// Find the other daughter
		UShort_t keyPos = v0m->keyPos();
		Int_t otherDaughter = -999;
		for (size_t idaught =0; idaught < gvec.size(); ++idaught) {
		    if (gvec[idaught]->key() == keyPos) {
			otherDaughter = idaught;
			break;
		    }
		}
		blah.SetOtherAntiLambdaDaughter(otherDaughter);
	    }
	}
	
// Now check if it's a K0Short
	{
	    
	    blah.SetIsK0Short(0);
	    
	    lambdaMapType::iterator lamIter = k0ShortDaughters.find(theTrack->key());
	    if (lamIter != k0ShortDaughters.end()) {
		blah.SetIsK0Short(1);
		StV0MuDst* v0m = (*lamIter).second;
		
		blah.SetK0ShortMass(v0m->massK0Short());
		blah.SetAlphaK0Short(v0m->alphaV0());
		blah.SetPtArmK0Short(v0m->ptArmV0());
		blah.SetGlobalXK0Short(v0m->decayVertexV0X());
		blah.SetGlobalYK0Short(v0m->decayVertexV0Y());
		blah.SetGlobalZK0Short(v0m->decayVertexV0Z());
		blah.SetGlobalPxK0Short(v0m->momV0X());
		blah.SetGlobalPyK0Short(v0m->momV0Y());
		blah.SetGlobalPzK0Short(v0m->momV0Z());
		
		// Find the other daughter
		UShort_t otherKey;
		if (theGeometry->charge() < 0) {
		    otherKey = v0m->keyPos();
		}
		else {
		    otherKey = v0m->keyNeg();
		}
		
		Int_t otherDaughter = -999;
		for (size_t idaught =0; idaught < gvec.size(); ++idaught) {
		    if (gvec[idaught]->key() == otherKey) {
			otherDaughter = idaught;
			break;
		    }
		}
		blah.SetOtherK0ShortDaughter(otherDaughter);
	    }
	}
	

	/*

///// begin
    // Get the PID traits, if there is an StrichPIDTrait:
	//    for (StSPtrVecTrackNodeConstIterator titer = theNodes.begin();
	//         titer != theNodes.end(); ++titer) {
	//       for (unsigned int ientry = 0; ientry < (*titer)->entries(); ++titer) {
	//	    StTrack *track = (*titer)->track(ientry);  
	    const StPtrVecTrackPidTraits& theRichPidTraits = theTrack->pidTraits(kRichId);
	    if(!theRichPidTraits.size()) continue;
	    cout << "\n\n  ...  there were tracks  ...\n \n" << endl;
//   	cout << " (" << theRichPidTraits.size() << ") Pid Traits.   p= ";
	// info from the traits	   //  Change to get productionVersion = 2

            StTrackPidTraits *theSelectedTrait = 0;
	    for (StPtrVecTrackPidTraitsConstIterator traititer=theRichPidTraits.begin(); 
                traititer != theRichPidTraits.end(); ++traititer) {
	      cout << "\n\n  ... begin the loop ...\n \n" << endl;
	        StRichPidTraits *p = dynamic_cast<StRichPidTraits*>(*traititer);
	        if (!p) continue;
	        if (!(p->productionVersion() == 2)) continue;
	        theSelectedTrait = *traititer;
		cout << "\n\n  ...  interator ...\n \n" << traititer << endl;		
	    }

	    cout << "\n\n  ...  end the loop  ...\n \n" << endl;
	    if(!theSelectedTrait) {
	       cout << "Error in the Selected Trait\nContinuing..." << endl;
	       continue;
	    }
	    StRichPidTraits *richPidTrait =
	      dynamic_cast<StRichPidTraits*>(theSelectedTrait); 
	    StRichSpectra *theSpectra = richPidTrait->getRichSpectra();

	    if (theSpectra) {
	      cout << "\n\n  ...  got information of RICH  ...\n \n" << endl;
	       blah.SetCherenkovAngle(theSpectra->getCherenkovAngle());
	       //blah.SetMassSquared(theSpectra->getMassSquared());
	       // blah.SetNPhotons(theSpectra->getNPhotons());
	    }
	    //       }
	    //    } 
///////  end
	*/

// Don't deal with traits.
// But create the residual.
	//
	// proximity matching between TPC track's predicted MIP and 
	// RICH pad plane MIP 
	//
    
	double smallestResidual=10.e10;
	double testThisResidual=0;
	
	
	vector<candidate> candidateHits;
    
	StThreeVectorD mProjectedMIP(anodeImpactLocal.position().x(),
				     anodeImpactLocal.position().y(),
				     anodeImpactLocal.position().z())
	    ;
	int mAssociatedMIP = -999;
	
	for (size_t hitIndex=0; hitIndex < theHits.size(); ++hitIndex) { 
	    testThisResidual = ((theHits[hitIndex])->local() - mProjectedMIP).perp();      

	    if(testThisResidual>5.*centimeter) continue;
	    
	    if(testThisResidual < 2.*centimeter)
		candidateHits.push_back( candidate(hitIndex, testThisResidual) );
	    
	    if (testThisResidual<smallestResidual) {
		smallestResidual = testThisResidual;   
		mAssociatedMIP   = hitIndex;
	    }
	}
    

	//
	// if there is more than 1 candidate
	// take the one with the highest charge
	//
	
	if(candidateHits.size()>1) {
	    mAssociatedMIP = -999;
	    
//	    cout << "associateMIP()\n";
	    //    cout << "\tMore than 1 hit *CAN BE* associated ";
	    //cout << "(" << candidateHits.size() << ")\n";
	    //cout << "\tTake highest amplitude with smallest residual\n";
	    
	    //
	    // sort from smallest residual to highest
	    // see operator< defined at the top of this file
	    //
	    sort(candidateHits.begin(), candidateHits.end());

	    double highestAmplitude = 0;
	    for(size_t ii=0; ii<candidateHits.size(); ii++) {
		if( (theHits[candidateHits[ii].first])->isSet(eMip) ) {
		    mAssociatedMIP = candidateHits[ii].first;
		    highestAmplitude = (theHits[candidateHits[ii].first])->charge();
		    break;
		}
	    }
	    
	    if(mAssociatedMIP<0) {
		//cout << "StRichTrack::assignMIP()\n";
		//cout << "\tTake smallest Residual" << endl;
		mAssociatedMIP = candidateHits[0].first;
		highestAmplitude = (theHits[candidateHits[0].first])->charge();
	    }
	    
	}
	
	blah.SetMipIndex(mAssociatedMIP);
       
	if (mAssociatedMIP>=0) {
	    StThreeVectorD theResid = ((theHits[mAssociatedMIP])->local() - mProjectedMIP);
	

	    blah.SetResidX(theResid.x());
	    blah.SetResidY(theResid.y());
//	    blah.SetResidZ(theResid.z());
	    blah.SetResid(theResid.perp());
	}
	
// Deal with traits

      const StSPtrVecTrackPidTraits&
            theRichPidTraits = theTrack->pidTraits();
       StRichSpectra *theSpectra = 0;
       
        for (StSPtrVecTrackPidTraitsConstIterator piter = theRichPidTraits.begin();
             piter != theRichPidTraits.end(); ++piter) {
            if (dynamic_cast<StRichPidTraits*>(*piter)) {
		StRichPidTraits *tempTrait = dynamic_cast<StRichPidTraits*>(*piter);
		if (tempTrait->productionVersion() == 20013) {
		    StRichSpectra *tempSpectra = tempTrait->getRichSpectra();
		    if (tempSpectra && tempSpectra->getVersion() == 20013) {
			theSpectra = tempSpectra;
		    }
		    
		}
	    }
        }
	
	if (theSpectra) {
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
	

	
	
	mRichUstStruct->AddRichGlobal(blah);
	
    }
    cout << "Added globals" << endl;
    
    mRichUstStruct->GetFRichPrimaries()->Expand(pvec.size());
    mRichUstStruct->SetNRichPrimaries(0);

    
    for (size_t iTrack = 0; iTrack < pvec.size(); ++iTrack) {
	
	StTrack* theTrack = pvec[iTrack];
	StTrackGeometry* theGeometry = 
//(theTrack->outerGeometry()) ?
//	    theTrack->outerGeometry() : 
	    theTrack->geometry();
	
	StRichLambdaUstTrack blah;
	StThreeVectorD theMomentum;
	if (fabs(Bmag)>0.1) {
	    theMomentum = theGeometry->momentum();
	}
	else {
	    theMomentum = theGeometry->helix().at(1) -
		theGeometry->helix().at(0);
	}

	blah.SetTrackFlag(theTrack->flag());
	
	blah.SetTrackQ(theGeometry->charge());
	blah.SetNFitPoints(theTrack->fitTraits().numberOfFitPoints());
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
	    
	    StRichLambdaUstTrack* theUstTrack = static_cast<StRichLambdaUstTrack*>((*theGlobals)[gindexforprimary[iTrack]-1]);
	    Float_t theSDCA = theUstTrack->GetGSDCA();
	    
	    
	    blah.SetGSDCA(theSDCA);
	    Float_t theSDCA2d = theUstTrack->GetGSDCA2d();
	    blah.SetGSDCA2d(theSDCA2d);
	    
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
	if (fabs(Bmag)>0.1) {
	    gaP =  theGeometry->helix().momentumAt(anodePath,Bmag*kilogauss);
	}
	else {
	    gaP = theMomentum;
	}
	
	StThreeVector<double> gaPstl(gaP.x(),gaP.y(),gaP.z());
	StThreeVector<double> laP;
	    
	ptrans->localMomentum(gaPstl,laP);
	blah.SetGlobalPadPx(gaP.x());
	blah.SetGlobalPadPy(gaP.y());
	blah.SetGlobalPadPz(gaP.z());

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
	if (fabs(Bmag)>0.1) {
	    rP = theGeometry->helix().momentumAt(radPath,Bmag*kilogauss);
	}
	else {
	    rP = theMomentum;
	}
	
	StThreeVector<double> rPstl(rP.x(),rP.y(),rP.z());
	StThreeVector<double> lrP;
	    
	ptrans->localMomentum(rPstl,lrP);
	blah.SetGlobalRadPx(rP.x());
	blah.SetGlobalRadPy(rP.y());
	blah.SetGlobalRadPz(rP.z());
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
	Int_t crossedRich = 0;
	
	if ( 
	    (
		(fabs(anodeImpactLocal.position().x()) < 
		 mGeometryDb->radiatorDimension().x()-2.) &&
		(fabs(anodeImpactLocal.position().y()) < 
		 mGeometryDb->radiatorDimension().y()-2.) &&
		(fabs(anodeImpactLocal.position().x()) > 
		 mGeometryDb->quadrantGapInX()/2.+2.0) &&
		(fabs(anodeImpactLocal.position().y()) > 
		 mGeometryDb->quadrantGapInY()/2.+2.0) )
	    &&
	    (
		(fabs(rlr.position().x()) < 
		 mGeometryDb->radiatorDimension().x()-2.) &&
		(fabs(rlr.position().y()) < 
		 mGeometryDb->radiatorDimension().y()-2.) &&
		(fabs(rlr.position().x()) > 
		 mGeometryDb->quadrantGapInX()/2.+2.0) &&
		(fabs(rlr.position().y()) > 
		 mGeometryDb->quadrantGapInY()/2.+2.0) ) )
	{
	    crossedRich = 1;
	}
	blah.SetCrossedRich(crossedRich);
	

// Don't deal with v0's.
// Don't deal with traits.
// But create the residual.
	//
	// proximity matching between TPC track's predicted MIP and 
	// RICH pad plane MIP 
	//
    
	double smallestResidual=10.e10;
	double testThisResidual=0;
	
	
	vector<candidate> candidateHits;
    

	StThreeVectorD mProjectedMIP(anodeImpactLocal.position().x(),
				     anodeImpactLocal.position().y(),
				     anodeImpactLocal.position().z())
	    ;
	int mAssociatedMIP = -999;
	
	for (size_t hitIndex=0; hitIndex < theHits.size(); ++hitIndex) { 
	    testThisResidual = ((theHits[hitIndex])->local() - mProjectedMIP).perp();      

	    if(testThisResidual>5.*centimeter) continue;
	    
	    if(testThisResidual < 2.*centimeter)
		candidateHits.push_back( candidate(hitIndex, testThisResidual) );
	    
	    if (testThisResidual<smallestResidual) {
		smallestResidual = testThisResidual;   
		mAssociatedMIP   = hitIndex;
	    }
	}
    

	//
	// if there is more than 1 candidate
	// take the one with the highest charge
	//
	
	if(candidateHits.size()>1) {
	    mAssociatedMIP = -999;
	    
//	    cout << "associateMIP()\n";
	    //    cout << "\tMore than 1 hit *CAN BE* associated ";
	    //cout << "(" << candidateHits.size() << ")\n";
	    //cout << "\tTake highest amplitude with smallest residual\n";
	    
	    //
	    // sort from smallest residual to highest
	    // see operator< defined at the top of this file
	    //
	    sort(candidateHits.begin(), candidateHits.end());

	    double highestAmplitude = 0;
	    for(size_t ii=0; ii<candidateHits.size(); ii++) {
		if( (theHits[candidateHits[ii].first])->isSet(eMip) ) {
		    mAssociatedMIP = candidateHits[ii].first;
		    highestAmplitude = (theHits[candidateHits[ii].first])->charge();
		    break;
		}
	    }
	    
	    if(mAssociatedMIP<0) {
		//cout << "StRichTrack::assignMIP()\n";
		//cout << "\tTake smallest Residual" << endl;
		mAssociatedMIP = candidateHits[0].first;
		highestAmplitude = (theHits[candidateHits[0].first])->charge();
	    }
	    
	}
	
	blah.SetMipIndex(mAssociatedMIP);
       
	if (mAssociatedMIP>=0) {
	    StThreeVectorD theResid = ((theHits[mAssociatedMIP])->local() - mProjectedMIP);
	

	    blah.SetResidX(theResid.x());
	    blah.SetResidY(theResid.y());
//	    blah.SetResidZ(theResid.z());
	    blah.SetResid(theResid.perp());
	}
	
// Deal with traits
	const StSPtrVecTrackPidTraits&
            theRichPidTraits = theTrack->pidTraits();
       StRichSpectra *theSpectra = 0;
       
        for (StSPtrVecTrackPidTraitsConstIterator piter = theRichPidTraits.begin();
             piter != theRichPidTraits.end(); ++piter) {
            if (dynamic_cast<StRichPidTraits*>(*piter)) {
		StRichPidTraits *tempTrait = dynamic_cast<StRichPidTraits*>(*piter);
		if (tempTrait->productionVersion() == 20013) {
		    StRichSpectra *tempSpectra = tempTrait->getRichSpectra();
		    if (tempSpectra && tempSpectra->getVersion() == 20013) {
			theSpectra = tempSpectra;
		    }
		    
		}
	    }
        }

	if (theSpectra) {
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

	mRichUstStruct->AddRichPrimary(blah);
	
    }
// pack up the photons
    mRichUstStruct->GetFPhotons()->Expand(theUstPhotons.size());
    mRichUstStruct->SetNPhotons(0);
    for (vector<StRichLambdaUstPhoton>::iterator p = theUstPhotons.begin();
	 p != theUstPhotons.end(); ++p) {
	mRichUstStruct->AddPhoton((*p));
    }
    theUstPhotons.clear();
    
    // pack up the rich hits 
    mRichUstStruct->GetFHits()->Expand(theHits.size());
    mRichUstStruct->SetNHits(0);
		
    for (StSPtrVecRichHitConstIterator iter = theHits.begin();
	 iter != theHits.end(); ++iter) {
	StRichLambdaUstHit blah;
	blah.SetCharge((*iter)->charge());
	blah.SetMaxAdc((*iter)->maxAmplitude());
	blah.SetNPads((*iter)->numberOfPads());
	blah.SetFlag((*iter)->reservedLong());
	
	UInt_t theClusterNumber = (*iter)->clusterNumber();
	blah.SetClusterNumber(theClusterNumber);
	blah.SetClusterFirstPad(theClusters[theClusterNumber]->firstPad());
	blah.SetNClusterPads(theClusters[theClusterNumber]->numberOfPads());
	blah.SetNClusterLocalMax(theClusters[theClusterNumber]->numberOfLocalMax());
	blah.SetClusterMinAmp(theClusters[theClusterNumber]->minimumAmplitudeOfLocalMax());
	

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




StRichLambdaSimpleUst::StRichLambdaSimpleUst(const Char_t *name) : StMaker(name) { 

    mEventCounter = 0;
    sprintf(mFileName,"lambdaUst.root");
    
}

StRichLambdaSimpleUst::~StRichLambdaSimpleUst() {}

void StRichLambdaSimpleUst::SetFileName(const Char_t *in) 
{
    sprintf(mFileName,in);
}

Int_t StRichLambdaSimpleUst::Init() {
    cout << "In simpleUst" << endl;
    
    mGeometryDb = StRichGeometryDb::getDb();

    mOutput1 = new TFile(mFileName,"RECREATE");
    mOutput1->SetCompressionLevel(9);
//    mOutput1->SetFormat(1);
    mRichUstStruct = new StRichLambdaUstStruct();
    
    mSimpleUstTree = new TTree("RichTree","Big Tree");
// Only set branch style if root version is greater than 3.01.05
#if ROOT_VERSION_CODE >= ROOT_VERSION(3,01,05)
    mSimpleUstTree->SetBranchStyle(0);
#endif 
    

    
    Int_t bufsize = 20000;
    Int_t split = 1;
    cout << "Making branches" << endl;
    
    mSimpleUstTree->Branch("StRichLambdaUstStruct","StRichLambdaUstStruct",
			   &mRichUstStruct,
			   bufsize,split);
    cout << "Branches Made" << endl;
    
    return 0; 
}

void StRichLambdaSimpleUst::Clear(Option_t *opt) 
{ 
    StMaker::Clear();
}

Int_t StRichLambdaSimpleUst::Finish()            
{ 
    mOutput1->Write();
    mOutput1->Close();
    return kStOK; 
}

//const char* StRichLambdaSimpleUst::GetCVS() {static const char cvs[]="Tag $Name:  $ $Id: StRichLambdaSimpleUst.cxx,v 1.1 2002/11/19 18:32:02 dunlop Exp $ built "__DATE__" "__TIME__ ; return cvs;}




map<unsigned int,StV0MuDst*> StRichLambdaSimpleUst::getLambdas(StEvent* mEvent) 
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
	     (v0m->decayLengthV0()>5.0) &&
	     (v0m->topologyMapPos().numberOfHits(kTpcId)>15) &&
	     (v0m->topologyMapNeg().numberOfHits(kTpcId)>15) &&
	     (v0m->dcaV0Daughters()<0.75) 
	    )
	{
	    if (
		(v0m->alphaV0()>0) &&
		(v0m->massLambda() > 1.07) &&
		(v0m->massLambda() < 1.18) &&
		(v0m->dcaNegToPrimVertex()>2.85) 
		)
	    {
		theLambdas.insert(make_pair(v0m->keyPos(),v0m));
		keys.push_back(v0m->keyPos());
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
map<unsigned int,StV0MuDst*> StRichLambdaSimpleUst::getAntiLambdas(StEvent* mEvent) 
{
    cout << " Getting AntiLambdas";
    
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
	     (v0m->decayLengthV0()>5.0) &&
	     (v0m->topologyMapPos().numberOfHits(kTpcId)>15) &&
	     (v0m->topologyMapNeg().numberOfHits(kTpcId)>15) &&
	     (v0m->dcaV0Daughters()<0.75) 
	    )
	{
	    if (
		(v0m->alphaV0()<0) &&
		(v0m->massAntiLambda() > 1.07) && 
		(v0m->massAntiLambda() < 1.18) &&
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
	    double tempMass = (*viter)->massAntiLambda() - mLambda;
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
map<unsigned int,StV0MuDst*> StRichLambdaSimpleUst::getK0Shorts(StEvent* mEvent) 
{
    cout << " Getting K0Shorts";
    
    //StSPtrVecTrackNode& nodes = mEvent->trackNodes();8
    StStrangeMuDstMaker* strangeDst =
	(StStrangeMuDstMaker*) GetMaker("strangeMuDst");
    
      
    map<unsigned int,StV0MuDst*> ret;
    if (!strangeDst) return ret;
    cout << "Got the strangeMuDst" << endl;
    
    multimap<unsigned int,StV0MuDst*> theLambdas;
    vector<unsigned int> keys;
    double mK0Short = 0.497672;
    
    for( Int_t j=0; j<strangeDst->GetNV0(); j++ ) {
	StV0MuDst *v0m = strangeDst->GetV0(j);
	if (
	    v0m->dcaV0ToPrimVertex()<0.6 &&
	    v0m->topologyMapPos().numberOfHits(kTpcId) > 15 &&
	    v0m->topologyMapNeg().numberOfHits(kTpcId) > 15 &&
	    v0m->decayLengthV0() > 4. &&
	    v0m->dcaNegToPrimVertex()>1.3 &&
	    v0m->dcaPosToPrimVertex()>1.3 &&
	    v0m->dcaV0Daughters()<1. &&
	    v0m->massK0Short() > 0.43 &&
	    v0m->massK0Short() < 0.56
	    ) {
	    theLambdas.insert(make_pair(v0m->keyNeg(),v0m));
	    keys.push_back(v0m->keyNeg());
	    theLambdas.insert(make_pair(v0m->keyPos(),v0m));
	    keys.push_back(v0m->keyPos());
		
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
	if (them.size()>1) cout << "Found two K0Shorts for one track.  Keeping one closest to K0Short mass" << endl;
	
	double theMass = 10e10;
	StV0MuDst* theV0 = 0;
	
	for (vector<StV0MuDst*>::iterator viter = them.begin();
	     viter != them.end(); ++viter) 
	{
	    double tempMass = (*viter)->massK0Short() - mK0Short;
	    if (tempMass < theMass) {
		theV0 = *viter;
		theMass = tempMass;
	    }
	}
	cout << "k0Short Mass = " << theMass << endl;
	
	ret.insert(make_pair(*keyIter,theV0));
    }
	
    return ret;
    
}

ClassImp(StRichLambdaSimpleUst)








   
	    
    
    
    
