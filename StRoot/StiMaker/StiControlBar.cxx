//StiControlBar.cxx
//M.L. Miller (Yale Software)
//04/01

#include <iostream>

#include "StChain.h"
#include "TControlBar.h"
#include "StiMaker.h"

#include "Sti/StiDetector.h"
#include "Sti/StiDrawableDetector.h"
#include "Sti/StiDetectorLayerContainer.h"


#include "StiDisplayManager.h"
#include "StiControlBar.h"
//#include "TestActionClass.h"

int StiControlBar::mevent = 0;
StChain* StiControlBar::mchain = 0;

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
    StiMaker::instance()->reset();
}

void StiControlBar::doNextStiGuiAction()
{
    StiMaker::instance()->doNextAction();
}

void StiControlBar::stepToNextEvent()
{
    mchain->Make();
    ++mevent;
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

void StiControlBar::printDisplayManager()
{
    StiDisplayManager::instance()->print();
}

void StiControlBar::printDetector()
{
    StiDetectorLayerContainer::instance()->print();
}

TControlBar* StiControlBar::makeControlBar()
{
    TControlBar* bar = new TControlBar("vertical","Sti Control Panel");
    
    //Add Buttons:
    bar->AddButton("Dump Display Manager","StiControlBar::printDisplayManager()",
		   "Show contents of the Display Manager");
    bar->AddButton("Dump Detector","StiControlBar::printDetector()","Show contents of Detector Container");
    bar->AddSeparator();
    bar->AddButton("All Visible","StiControlBar::setVisible()","Set All Drawables to Visible State");
    bar->AddButton("All Invisible","StiControlBar::setInvisible()","Set All Drawables to Invisible State");    
    bar->AddButton("Reset", "StiControlBar::resetStiGuiForEvent()","Reset Sti For Next Event");
    bar->AddButton("Step",  "StiControlBar::doNextStiGuiAction()","Step Through Next Action");
    bar->AddButton("Event Step","StiControlBar::stepToNextEvent()","Step Through to Next Event");

    //Detector navigation
    bar->AddButton("Set Sector","StiControlBar::setSector()","Set to top of sector");
    bar->AddButton("Set Sector and Padrow","StiControlBar::setSectorAndPadrow()",
		   "Set to sector, padrow");
    bar->AddButton("Show Current Detector","StiControlBar::showCurrentDetector()","Highlight Current Detector");
    bar->AddButton("Padrow Step (Plus)","StiControlBar::padrowStepPlus()","Step Radially to Next Padrow Out");
    bar->AddButton("Padrow Step (Minus)","StiControlBar::padrowStepMinus()","Step Radially to Next Padrow In");
    bar->AddButton("Sector Step (Plus)","StiControlBar::sectorStepPlus()","Step Clockwise to Next Sector");
    bar->AddButton("Sector Step (Minus)","StiControlBar::sectorStepMinus()",
		   "Step Counter-clockwise to next sector");

    //Chain management
    bar->AddSeparator();
    bar->AddButton("Finish","StiControlBar::finish()","Call StChain::Finish()");

    bar->Show();
    return bar;
};

void StiControlBar::setSector()
{
    cout <<"\nEnter Sector: (int)"<<endl;
    int sector;
    cin >>sector;
    cout <<"Setting to  sector:\t"<<sector<<endl;
    StiDetectorLayerContainer& rdet = *(StiDetectorLayerContainer::instance());
    rdet.setRefDetector(sector);
    const StiDetector* layer = *rdet;
    if (!layer) {
	cout <<"Error in setSectorAndPadrow"<<endl;
	return;
    }
    cout <<"Detector Set To: "<<layer->getName()<<endl;
    StiControlBar::showCurrentDetector();
}

void StiControlBar::setSectorAndPadrow()
{
    cout <<"\nEnter Sector: (int)"<<endl;
    int sector;
    cin >>sector;
    cout <<"\nEnter Padrow: (int) "<<endl;
    int padrow;
    cin >>padrow;

    cout <<"Setting to  sector:\t"<<sector<<"\tpadrow:\t"<<padrow<<endl;
    StiDetectorLayerContainer& rdet = *(StiDetectorLayerContainer::instance());
    rdet.setRefDetector(sector, padrow);
    const StiDetector* layer = *rdet;
    if (!layer) {
	cout <<"Error in setSectorAndPadrow"<<endl;
	return;
    }
    cout <<"Detector Set To: "<<layer->getName()<<endl;
    StiControlBar::showCurrentDetector();
}

void StiControlBar::padrowStepPlus()
{
    StiControlBar::setCurrentDetectorToDefault();
    StiDetectorLayerContainer& rdet = *(StiDetectorLayerContainer::instance());
    rdet.padrowStepPlus();
    StiControlBar::showCurrentDetector();
}

void StiControlBar::padrowStepMinus()
{
    StiControlBar::setCurrentDetectorToDefault();
    StiDetectorLayerContainer& rdet = *(StiDetectorLayerContainer::instance());
    rdet.padrowStepMinus();
    StiControlBar::showCurrentDetector();
}

void StiControlBar::sectorStepPlus()
{
    StiControlBar::setCurrentDetectorToDefault();
    StiDetectorLayerContainer& rdet = *(StiDetectorLayerContainer::instance());
    rdet.sectorStepPlus();
    StiControlBar::showCurrentDetector();
}

void StiControlBar::sectorStepMinus()
{
    StiControlBar::setCurrentDetectorToDefault();
    StiDetectorLayerContainer& rdet = *(StiDetectorLayerContainer::instance());
    rdet.sectorStepMinus();
    StiControlBar::showCurrentDetector();
}

void StiControlBar::showCurrentDetector()
{
    StiDetectorLayerContainer& rdet = *(StiDetectorLayerContainer::instance());
    const StiDrawableDetector* constlayer = dynamic_cast<const StiDrawableDetector*>(*rdet);
    //Bad, but it's the only way around it:
    StiDrawableDetector* layer = const_cast<StiDrawableDetector*>(constlayer);
    if (!layer) {
	cout <<"Error! StiControlBar::padrowStepMinus(): Failed to get drawable detector"<<endl;
	return;
    }
    layer->setVisibility(2);
    layer->setColor(2);
    
    cout<<*layer<<endl;
    StiDisplayManager::instance()->draw();
    StiDisplayManager::instance()->update();
    
    return;
}

void StiControlBar::setCurrentDetectorToDefault()
{
    //cout <<"setCurrentDetectorToDefault()"<<endl;
    StiDetectorLayerContainer& rdet = *(StiDetectorLayerContainer::instance());
    const StiDrawableDetector* constlayer = dynamic_cast<const StiDrawableDetector*>(*rdet);
    StiDrawableDetector* layer = const_cast<StiDrawableDetector*>(constlayer);
    if (!layer) {
	cout <<"Error! StiControlBar::setCurrentDetectorToDefault(): Failed to get drawable detector"<<endl;
	return;
    }
    layer->setColor(1);
}
