//  StiMaker.cxx
// M.L. Miller
// 5/00

#include <iostream.h>
#include <math.h>
#include <string>

//Root (Temp)
#include "TCanvas.h"
#include "TPolyMarker3D.h"
#include "TNode.h"
#include "TTUBE.h"

// StRoot
#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "StMessMgr.h"

// SCL
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"

// StEvent
#include "StEventTypes.h"

// Sti
#include "Sti/StiHitContainer.h"
#include "Sti/StiHitFiller.h"
#include "Sti/StiDetectorContainer.h"
#include "Sti/StiTrackContainer.h"
#include "Sti/StiGeometryTransform.h"
#include "Sti/StiEvaluableTrackSeedFinder.h"

//StiGui
#include "StiGui/StiDrawableHits.h"
#include "StiGui/StiRootDrawableHits.h"
#include "StiGui/StiRootDrawableLine.h"
#include "StiGui/StiDisplayManager.h"

// StiMaker
#include "StiMaker.h"

StiMaker* StiMaker::sinstance = 0;
bool StiMaker::mdone = true;
int StiMaker::mcounter = 0;

ClassImp(StiMaker)
  
StiMaker::StiMaker(const Char_t *name) : StMaker(name)
{
    cout <<"StiMaker::StiMaker()"<<endl;
    sinstance = this;
}

StiMaker* StiMaker::instance()
{
    return (sinstance) ? sinstance : new StiMaker();
}

void StiMaker::kill()
{
    if (sinstance) {
	delete sinstance;
	sinstance = 0;
    }
    return;
}

StiMaker::~StiMaker() 
{
    StiHitContainer::kill();
    mhitstore = 0;
    
    delete mhitfactory;
    mhitfactory = 0;
    
    delete mhitfiller;
    mhitfiller = 0;
    
    delete mdisplay;
    mdisplay = 0;

    delete mtrackfactory;
    mtrackfactory = 0;

    delete mtrackseedfinder;
    mtrackseedfinder = 0;
    
    //delete md_trackfactory;
    md_trackfactory = 0;
    
    StiDetectorContainer::kill();
    mdetector = 0;
    
    StiTrackContainer::kill();
    mtrackstore = 0;

    delete mdetectorfactory;
    mdetectorfactory = 0;

    delete mdatanodefactory;
    mdatanodefactory = 0;
    
}

void StiMaker::Clear(const char*)
{
    //Clear HitContainer
    mhitstore->clear();

    //Clear Drawable hits
    mdrawablehits->clear();
    
    //Reset DetectorContainer
    StiDetectorContainer::instance()->reset();
    
    //Reset HitFactory
    mhitfactory->reset();

    //Reset TrackFactory
    mtrackfactory->reset();

    //Reset Drawable TrackFactory
    md_trackfactory->reset();
    
    //Reset DisplayManager
    mdisplay->reset();
    
    StMaker::Clear();
}

Int_t StiMaker::Finish()
{
    return StMaker::Finish();
}

Int_t StiMaker::Init()
{
    //The track store
    mtrackstore = StiTrackContainer::instance();

    //The hit container
    mhitstore = StiHitContainer::instance();

    //The Hit Factory
    mhitfactory = new StiHitFactory("HitFactory");
    mhitfactory->setIncrementalSize(50000); //Allocate in chunks of 50k hits
    mhitfactory->setMaxIncrementCount(10);  //So, we can have 10 allocations at 50k a pop -> 500k hits max.

    //The Evalualbe Track Factory
    mtrackfactory = new StiEvaluableTrackFactory("EvaluableTrackFactory");
    mtrackfactory->setIncrementalSize(1000);
    mtrackfactory->setMaxIncrementCount(10);

    md_trackfactory = new StiRDEvaluableTrackFactory("DrawableEvaluableTrackFactory");
    md_trackfactory->setIncrementalSize(1000);
    md_trackfactory->setMaxIncrementCount(10);

    //EvaluableTrack SeedFinder
    mtrackseedfinder = new StiEvaluableTrackSeedFinder();
    mtrackseedfinder->setFactory(mtrackfactory);
    mtrackseedfinder->setStTrackType(global);

    //The StiDetector factory
    mdetectorfactory = new detector_factory("DrawableDetectorFactory");
    mdetectorfactory->setIncrementalSize(1000);
    mdetectorfactory->setMaxIncrementCount(10);
    mdetectorfactory->reset();

    //The DetectorNodeFactory
    mdatanodefactory = new data_node_factory("DataNodeFactory");
    mdatanodefactory->setIncrementalSize(1000);
    mdatanodefactory->setMaxIncrementCount(10);
    mdatanodefactory->reset();
    
    //The Display
    mdisplay = StiDisplayManager::instance(); //Must come before anything that you want to be drawn
    mdisplay->cd();
    mdisplay->draw();
    mdisplay->update();
    
    //Drawable hits
    mdrawablehits = new StiRootDrawableHits();
    mdrawablehits->clear();
    mdisplay->addDrawable(mdrawablehits);

    //The Detector Tree
    //Must build Materials before detectors
    mdetector = StiDetectorContainer::instance();
    mdetector->buildDetectors(mdetectorbuildpath, mdatanodefactory, mdetectorfactory);
    mdetector->reset();
    //mdetector->print();
      
    mdisplay->draw();
    mdisplay->update();

    //The Hit Filler
    mhitfiller = new StiHitFiller();
    mhitfiller->addDetector(kTpcId);
    //mhitfiller->addDetector(kSvtId);
    cout <<"Hits used from detectors:\t"<<*mhitfiller<<endl;
    
    return StMaker::Init();
}

Int_t StiMaker::Make()
{
    Clear();
    StEvent* rEvent = 0;
    rEvent = (StEvent*) GetInputDS("StEvent");
    if (rEvent) {
	mevent = rEvent;
	
	cout <<"\n---------- StiMaker::Make() ------------\n"<<endl;
	cout <<"Number of Primary Vertices:\t"<<mevent->numberOfPrimaryVertices()<<endl;

	//Fill hits, organize the container
	mhitfiller->setEvent(mevent);
	mhitfiller->fillHits(mhitstore, mhitfactory);
	mhitstore->sortHits();

	//Temp patch to draw hits
	const hitmap& hits = mhitstore->hits();
	for (hitmap::const_iterator it=hits.begin(); it!=hits.end(); it++) {
	    const hitvector& tempvec = (*it).second;
	    for (hitvector::const_iterator vit=tempvec.begin(); vit!=tempvec.end(); vit++) {
		
		mdrawablehits->push_back( (*vit) );
	    }
	}
	mdrawablehits->fillHitsForDrawing();

	//Temp patch to test the SeedFinder, draw some tracks
	//Initialize the SeedFinder, loop on tracks
	mtrackseedfinder->setEvent(mevent);
	while (mtrackseedfinder->hasMore()) {
	    StiEvaluableTrack* thetrack = dynamic_cast<StiEvaluableTrack*>(mtrackseedfinder->next());
	    if (thetrack) {
		StTrack* sttrack = thetrack->stTrack();
		cout <<sttrack->geometry()->momentum()<<endl;

		//Make a drawable track, add to display
		StiRootDrawableStiEvaluableTrack* drawabletrack = md_trackfactory->getObject();
		drawabletrack->setStTrack(sttrack);
		drawabletrack->fillHitsForDrawing();
		mdisplay->addDrawable(drawabletrack);
	    }
	    
	}
	
    }
    mdisplay->draw();
    mdisplay->update();
    return kStOK;
}

void StiMaker::printStatistics() const
{
    cout <<"HitFactory Size:\t"<<mhitfactory->getCurrentSize()<<endl;
    cout <<"HitContainer size:\t"<<mhitstore->size()<<endl;
    cout <<"Number of Primary Vertices:\t"<<mhitstore->numberOfVertices()<<endl;
}

void StiMaker::setMaterialBuildPath(char* val)
{
    mmaterialbuildpath = val;
}

void StiMaker::setDetectorBuildPath(char* val)
{
    mdetectorbuildpath = val;
}

void StiMaker::reset()
{
    mdone=false;
    mcounter=0;
    
}

void StiMaker::doNextAction()
{
    //Add call to next tracker action here
    return;
}

bool StiMaker::hasMore()
{
    return (!mdone);
}

