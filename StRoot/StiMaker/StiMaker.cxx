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
#include "Sti/StiHitContainer.h"
#include "Sti/StiHitFactory.h"
#include "Sti/StiHitFiller.h"

// StiMaker
//#include "AnaTreeManager/TreeEntryClasses.h"
//#include "AnaTreeManager/AnaTreeManager.h"
#include "StiMaker.h"

ClassImp(StiMaker)
  
StiMaker::StiMaker(const Char_t *name) : StMaker(name)
{
    mhitstore = new StiHitContainer();
    mhitfactory = new StiHitFactory();
    mhitfiller = new StiHitFiller();
    //AnaTreeManager* treestore = AnaTreeManager::instance();
    //char* outfilename = "testTree.root";
    //treestore->setFileName(outfilename);
    //treestore->setAnaTreeType(AnaTreeManager::write);
    //bool ok = treestore->makeTree();
    //cout <<"Result of opening tree in write mode:\t"<<ok<<endl;
}

StiMaker::~StiMaker() 
{
    delete mhitstore;
    mhitstore = 0;
    delete mhitfactory;
    mhitfactory = 0;
    delete mhitfiller;
    mhitfiller = 0;
    
    //AnaTreeManager::kill();
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
    mhitfiller->addDetector(kTpcId);
    mhitfiller->addDetector(kSvtId);
    cout <<"Hits used from detectors:\t"<<*mhitfiller<<endl;

    return StMaker::Init();
}

Int_t StiMaker::Make()
{   
    StEvent* rEvent = 0;
    rEvent = (StEvent*) GetInputDS("StEvent");
    if (rEvent) {
	mevent = rEvent;
	
	//Initialize tree this event
	//AnaTreeManager* treestore = AnaTreeManager::instance();
	//treestore->event()->clear();

	cout <<"\n---------- StiMaker::Make() ------------\n"<<endl;
	cout <<"Number of Primary Vertices:\t"<<mevent->numberOfPrimaryVertices()<<endl;
    }
    
    return kStOK;
}
