//  StiMaker.cxx
// M.L. Miller
// 5/00

#include <iostream.h>
#include <math.h>
#include <string>

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
#include "Sti/StiGeometryTransform.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiHitFiller.h"
#include "Sti/StiDetectorContainer.h"
#include "Sti/StiDrawableDetector.h"
#include "Sti/StiTrackContainer.h"

// StiMaker
#include "StiDisplayManager.h"
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
    mhitstore->clear();
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
    
    mdisplay = StiDisplayManager::instance(); //Must come before anything that you want to be drawn

    mtrackstore = StiTrackContainer::instance();
    mhitstore = StiHitContainer::instance();
    mhitfactory = new StiHitFactory("HitFactory");

    mdisplay->cd();
    mdisplay->draw();
    mdisplay->update();

    mdetector = StiDetectorContainer::instance();

    //Must build Polygons and Materials before detectors
    mdetector->buildPolygons(mpolygonbuildpath);
    mdetector->buildMaterials(mmaterialbuildpath);
    mdetector->buildDetectors(mdetectorbuildpath);
    mdetector->reset();
    mdetector->print();
    
    mdisplay->draw();
    mdisplay->update();

    
    //mhitfiller = new StiHitFiller();
    //mhitfiller->addDetector(kTpcId);
    //mhitfiller->addDetector(kSvtId);
    //cout <<"Hits used from detectors:\t"<<*mhitfiller<<endl;
    
    return StMaker::Init();
}

Int_t StiMaker::Make()
{
    StEvent* rEvent = 0;
    rEvent = (StEvent*) GetInputDS("StEvent");
    if (rEvent) {
	mevent = rEvent;
	
	cout <<"\n---------- StiMaker::Make() ------------\n"<<endl;
	cout <<"Number of Primary Vertices:\t"<<mevent->numberOfPrimaryVertices()<<endl;
	//mhitfiller->setEvent(mevent);
	//mhitfiller->fillHits(mhitstore, mhitfactory);
    }
    return kStOK;
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
    StiDetectorContainer::instance()->reset();
}

void StiMaker::doNextAction()
{
    /*
      if (mdone) {
      cout <<"StiMaker::doNext()\t Nothing Left to do"<<endl;
      return;
      }
      
      StiDetectorContainer& rdet = *(StiDetectorContainer::instance());
      const StiDrawableDetector* layer = dynamic_cast<const StiDrawableDetector*>(*rdet);
      if (!layer) return;
      StiDisplayManager::instance()->setVisible(layer);
      StiDisplayManager::instance()->draw();
      StiDisplayManager::instance()->update();
      bool cangofurther = rdet.padrowStepMinus();
      if (!cangofurther) {
      rdet.setRefDetector( layer->getSector()+1 );
      }

    */
    return;
}

bool StiMaker::hasMore()
{
    return (!mdone);
}

