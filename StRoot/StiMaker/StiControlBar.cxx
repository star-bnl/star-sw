//StiControlBar.cxx
//M.L. Miller (Yale Software)
//04/01

#include <iostream>

#include "StChain.h"
#include "TControlBar.h"
#include "StiMaker.h"

//SCL
#include "StMemoryInfo.hh"

//Sti
#include "Sti/StiDetector.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiDetectorContainer.h"
#include "Sti/StiHitContainer.h"

//StiGui
#include "StiGui/StiRootDrawableDetector.h"
#include "StiGui/StiDisplayManager.h"

//StiMaker
#include "StiControlBar.h"

int StiControlBar::mnevent = 0;
StChain* StiControlBar::mchain = 0;

ostream& operator<<(ostream&, const StiDetector&);

ClassImp(StiControlBar)

StiControlBar::StiControlBar()
{
    cout <<"StiControlBar::StiControlBar()"<<endl;
    mbar = makeControlBar();
}

StiControlBar::~StiControlBar()
{
    cout <<"StiControlBar::~StiControlBar()"<<endl;
}

void StiControlBar::resetStiGuiForEvent()
{
    //cout <<"StiControlBar::resetStiGuiForEvent()"<<endl;
    StiControlBar::setCurrentDetectorToDefault();
    StiMaker::instance()->Clear();
    StiDisplayManager::instance()->draw();
    StiDisplayManager::instance()->update();
    StiControlBar::showCurrentDetector();
    //cout <<"\t Leaving StiControlBar::resetStiGuiForEvent()"<<endl;
}

void StiControlBar::doNextStiGuiAction()
{
    //cout <<"StiControlBar::doNextStGuiAction()"<<endl;
    StiControlBar::setCurrentDetectorToDefault();
    StiMaker::instance()->doNextAction();
    StiControlBar::showCurrentDetector();
    //cout <<"\t Leaving StiControlBar::doNextStGuiAction()"<<endl;
}

void StiControlBar::stepToNextEvent()
{
    StiControlBar::setCurrentDetectorToDefault();    
    mchain->Clear();
    mchain->Make();
    StiControlBar::showCurrentDetector();
    ++mnevent;
}

void StiControlBar::stepThroughNEvents()
{
    StiControlBar::setCurrentDetectorToDefault();    
    cout <<"\nEnter number of events to process (int) "<<endl;
    int nevents;
    cin >> nevents;
    for (int i=0; i<nevents; ++i) {
	mchain->Clear();
	mchain->Make();
	StiControlBar::showCurrentDetector();
	++mnevent;
    }
}

void StiControlBar::printFactorySize()
{
    StiMaker::instance()->printStatistics();
}

void StiControlBar::finish()
{
    mchain->Finish();
    
    //delete this;
    //this=0;
}

void StiControlBar::setVisible()
{
    StiDisplayManager::instance()->setVisible();
    StiDisplayManager::instance()->draw();
    StiDisplayManager::instance()->update();
}

void StiControlBar::setInvisible()
{
    StiDisplayManager::instance()->setInvisible();
    StiDisplayManager::instance()->draw();
    StiDisplayManager::instance()->update();
}

void StiControlBar::setSvtVisible()
{
    StiDisplayManager::instance()->setSvtVisible();
    StiDisplayManager::instance()->draw();
    StiDisplayManager::instance()->update();
}

void StiControlBar::setSvtInvisible()
{
    StiDisplayManager::instance()->setSvtInvisible();
    StiDisplayManager::instance()->draw();
    StiDisplayManager::instance()->update();
}

void StiControlBar::setTpcVisible()
{
    StiDisplayManager::instance()->setTpcVisible();
    StiDisplayManager::instance()->draw();
    StiDisplayManager::instance()->update();
}

void StiControlBar::setTpcInvisible()
{
    StiDisplayManager::instance()->setTpcInvisible();
    StiDisplayManager::instance()->draw();
    StiDisplayManager::instance()->update();
}

void StiControlBar::printDisplayManager()
{
    StiDisplayManager::instance()->print();
}

void StiControlBar::printDetector()
{
    StiDetectorContainer::instance()->print();
}


void StiControlBar::setLayer()
{
    //cout <<"Function Not Currently Implemented"<<endl;
    StiControlBar::setCurrentDetectorToDefault();
    cout <<"\nEnter position: (double)"<<endl;
    double position;
    cin >>position;
    cout <<"Setting to  position:\t"<<position<<endl;
    StiDetectorContainer& rdet = *(StiDetectorContainer::instance());
    rdet.setToDetector(position);
    StiDetector* layer = *rdet;
    if (!layer) {
	cout <<"Error in setSectorAndPadrow"<<endl;
	return;
    }
    cout <<"Detector Set To: "<<layer->getName()<<endl;
    StiControlBar::showCurrentDetector();
}

void StiControlBar::setLayerAndAngle()
{
    //cout <<"Function Not Currently Implemented"<<endl;
    StiControlBar::setCurrentDetectorToDefault();
    cout <<"\nEnter position: (double)"<<endl;
    double position;
    cin >>position;

    cout <<"\nEnter angle: (double)"<<endl;
    double angle;
    cin >> angle;
    cout <<"Setting to  position:\t"<<position<<"\tangle:\t"<<angle<<endl;

    StiDetectorContainer& rdet = *(StiDetectorContainer::instance());
    rdet.setToDetector(position, angle);
    StiDetector* layer = *rdet;
    if (!layer) {
	cout <<"Error in setSectorAndPadrow"<<endl;
	return;
    }
    cout <<"Detector Set To: "<<layer->getName()<<endl;
    StiControlBar::showCurrentDetector();
}

void StiControlBar::moveOut()
{
    //cout <<"Function Not Currently Implemented"<<endl;
    //cout <<"StiControlBar::moveOut()"<<endl;
    StiControlBar::setCurrentDetectorToDefault();
    StiDetectorContainer& rdet = *(StiDetectorContainer::instance());
    rdet.moveOut();
    StiControlBar::showCurrentDetector();
    //cout <<"\t Leaving StiControlBar::moveOut()"<<endl;
}

void StiControlBar::moveIn()
{
    //cout <<"Function Not Currently Implemented"<<endl;
    //cout <<"StiControlBar::moveIn()"<<endl;
    StiControlBar::setCurrentDetectorToDefault();
    StiDetectorContainer& rdet = *(StiDetectorContainer::instance());
    rdet.moveIn();
    StiControlBar::showCurrentDetector();
    //cout <<"\t Leaving StiControlBar::moveIn()"<<endl;
}

void StiControlBar::nextStartPoint()
{
    StiDetectorContainer& rdet = *(StiDetectorContainer::instance());
    if (rdet.hasMoreStartPoints()) {
	StiControlBar::setCurrentDetectorToDefault();
	rdet.nextStartPoint();
	StiControlBar::showCurrentDetector();
    }
}

//void StiControlBar::previousStartPoint()
//{
//  StiDetectorContainer& rdet = *(StiDetectorContainer::instance());
//  if (rdet.hasMoreStartPoints()) {
//StiControlBar::setCurrentDetectorToDefault();
//rdet.previousStartPoint();
//StiControlBar::showCurrentDetector();
//  }
//}

void StiControlBar::movePlusPhi()
{
    //cout <<"Function Not Currently Implemented"<<endl;
    StiControlBar::setCurrentDetectorToDefault();
    StiDetectorContainer& rdet = *(StiDetectorContainer::instance());
    rdet.movePlusPhi();
    StiControlBar::showCurrentDetector();
}

void StiControlBar::moveMinusPhi()
{
    //cout <<"Function Not Currently Implemented"<<endl;
    StiControlBar::setCurrentDetectorToDefault();
    StiDetectorContainer& rdet = *(StiDetectorContainer::instance());
    rdet.moveMinusPhi();
    StiControlBar::showCurrentDetector();
}

void StiControlBar::printHitContainerForDetector()
{
    StiDetectorContainer& rdet = *(StiDetectorContainer::instance());
    StiDetector* layer = *rdet;
    
    if (!layer) {
	cout <<"Error! StiControlBar::printHitContainerForDetector(): Failed to get detector"<<endl;
	return;
    }
    StiHitContainer::instance()->print( layer->getPlacement()->getCenterRefAngle(), layer->getPlacement()->getCenterRadius() );
}

void StiControlBar::showCurrentDetector()
{
    //cout <<"Function Not Currently Implemented"<<endl;
    StiDetectorContainer& rdet = *(StiDetectorContainer::instance());
    StiRootDrawableDetector* layer = dynamic_cast<StiRootDrawableDetector*>(*rdet);
    
    if (!layer) {
	cout <<"Error! StiControlBar::showCurrentDetector(): Failed to get drawable detector"<<endl;
	return;
    }
    layer->setVisibility(true);
    layer->setColor(2);
    
    cout<<*layer<<endl;
    StiDisplayManager::instance()->draw();
    StiDisplayManager::instance()->update();
    
    return;
}

void StiControlBar::memoryInfo()
{
    StMemoryInfo::instance()->snapshot();
    StMemoryInfo::instance()->print();
}

void StiControlBar::setCurrentDetectorToDefault()
{
    //cout <<"Function Not Currently Implemented"<<endl;
    //cout <<"StiControlBar::setCurrentDetectorToDefault()"<<endl;
    StiDetectorContainer& rdet = *(StiDetectorContainer::instance());
    StiRootDrawableDetector* layer = dynamic_cast<StiRootDrawableDetector*>(*rdet);
    if (!layer) {
	cout <<"Error! StiControlBar::setCurrentDetectorToDefault(): Failed to get drawable detector"<<endl;
	return;
    }
    layer->setColor(1);
    //cout <<"\t Leaving StiControlBar::setCurrentDetectorToDefault()"<<endl;
}

void StiControlBar::printHits()
{
    StiHitContainer::instance()->print();
}

void StiControlBar::printVertices()
{
    StiHitContainer::instance()->printVertices();
}

TControlBar* StiControlBar::makeControlBar()
{
    TControlBar* bar = new TControlBar("vertical","Sti Control Panel");
    
    //Add Buttons:
    bar->AddButton("Memory Snapshot","StiControlBar::memoryInfo()",
		   "Print a Current Membory Snapshot");

    bar->AddButton("Show Maker Summary","StiControlBar::printFactorySize()",
		   "Show summary of StiMaker objects");

    bar->AddButton("Dump Display Manager","StiControlBar::printDisplayManager()",
		   "Show contents of the Display Manager");
    bar->AddButton("Dump Detector","StiControlBar::printDetector()","Show contents of Detector Container");
    bar->AddButton("Dump Hits","StiControlBar::printHits()","Show all contents of Hit Container");
    bar->AddButton("Dump Vertices", "StiControlBar::printVertices()", "Show Primary vertices for this event");

    bar->AddSeparator();
    
    bar->AddButton("All Visible","StiControlBar::setVisible()","Set All Drawables to Visible State");
    bar->AddButton("All Invisible","StiControlBar::setInvisible()","Set All Drawables to Invisible State");
    
    bar->AddButton("Svt Visible","StiControlBar::setSvtVisible()","Set Svt Drawables to Visible State");    
    bar->AddButton("Svt Invisible","StiControlBar::setSvtInvisible()","Set Svt Drawables to Invisible State");
    
    bar->AddButton("Tpc Visible","StiControlBar::setTpcVisible()","Set Tpc Drawables to Visible State");    
    bar->AddButton("Tpc Invisible","StiControlBar::setTpcInvisible()","Set Tpc Drawables to Invisible State");
    

    //Detector navigation
    bar->AddButton("Show Current Detector","StiControlBar::showCurrentDetector()","Highlight Current Detector");
    bar->AddButton("Show Hits for Current Detector","StiControlBar::printHitContainerForDetector()",
		   "Show hits for Current Detector");

    bar->AddButton("Next Start Point", "StiControlBar::nextStartPoint()","Move to Next Starting Point");
    //bar->AddButton("Previous Start Point", "StiControlBar::previousStartPoint()","Move to Previous Starting Point");
    
    bar->AddButton("Move Out","StiControlBar::moveOut()","Step Radially to Next Padrow Out");
    bar->AddButton("Move In","StiControlBar::moveIn()","Step Radially to Next Padrow In");
    
    bar->AddButton("Move Around (Plus)","StiControlBar::movePlusPhi()","Step positive in global phi");
    bar->AddButton("Move Around (Minus)","StiControlBar::moveMinusPhi()","Step negative in global phi");

    bar->AddButton("Set Layer","StiControlBar::setLayer()","Set to given layer");
    bar->AddButton("Set Layer and Angle","StiControlBar::setLayerAndAngle()",
		   "Set to given layer, angle");

    //Chain management
    bar->AddSeparator();
    bar->AddButton("Reset", "StiControlBar::resetStiGuiForEvent()","Reset Sti For Next Event");
    bar->AddButton("Step",  "StiControlBar::doNextStiGuiAction()","Step Through Next Action");
    bar->AddButton("Event Step","StiControlBar::stepToNextEvent()","Step Through to Next Event");
    bar->AddButton("N-Event Step","StiControlBar::stepThroughNEvents()","Step Through N-Events");
    bar->AddButton("Finish","StiControlBar::finish()","Call StChain::Finish()");

    bar->Show();
    return bar;
};
