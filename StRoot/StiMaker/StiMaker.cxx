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

//StiGui
//#include "StiGui/StiRootDrawableDetector.h"
#include "StiGui/StiDrawableHits.h"
#include "StiGui/StiRootDrawableHits.h"
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
    
    StiDetectorContainer::kill();
    mdetector = 0;
    
    StiTrackContainer::kill();
    mtrackstore = 0;
    
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
    
    StMaker::Clear();
}

Int_t StiMaker::Finish()
{
    return StMaker::Finish();
}

Int_t StiMaker::Init()
{
    //Ben, uncomment the next line to produce seg-fualt, and then look at StiGeometryTransform constructor.  MLM
    //StiGeometryTransform* trans = StiGeometryTransform::instance();
    
    mtrackstore = StiTrackContainer::instance();
    mhitstore = StiHitContainer::instance();
    mhitfactory = new StiHitFactory("HitFactory");
    mhitfactory->setIncrementalSize(50000); //Allocate in chunks of 50k hits
    mhitfactory->setMaxIncrementCount(10);  //So, we can have 10 allocations at 50k a pop -> 500k hits max.  Throw's error if over this!

    mdisplay = StiDisplayManager::instance(); //Must come before anything that you want to be drawn
    mdisplay->cd();
    mdisplay->draw();
    mdisplay->update();
    
    //Drawable hits
    mdrawablehits = new StiRootDrawableHits();
    mdrawablehits->clear();
    mdisplay->addDrawable(mdrawablehits);
    
    //Must build Polygons and Materials before detectors
    mdetector = StiDetectorContainer::instance();
    mdetector->buildPolygons(mpolygonbuildpath);
    mdetector->buildMaterials(mmaterialbuildpath);
    mdetector->buildDetectors(mdetectorbuildpath);
    mdetector->reset();
    //mdetector->print();
      
    mdisplay->draw();
    mdisplay->update();

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

void StiMaker::setPolygonBuildPath(char* val)
{
    mpolygonbuildpath = val;
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

