//StiControlBar.h
//M.L. Miller (Yale Software)
//04/01

//This class is meant to be instantiated from the root-prompt, from where it controls program flow for the chain that executes Sti Tracker

#ifndef StiControlBar_HH
#define StiControlBar_HH

#include "TObject.h"

class TControlBar;
class StChain;

class StiControlBar
{
public:
    StiControlBar();
    virtual ~StiControlBar();

    //To be called with buttons
    static void doNextStiGuiAction(); //do next action within this event
    static void finishEvent(); //fit/find the entire event
    static void stepToNextEvent(); //step to the next event
    static void stepThroughNEvents();
    //step through user-specified number of events
    static void finish(); //call StChain::Finish() and close control
    static void printDisplayManager(); //call StiDisplayManager::print()
    static void printDetector(); //call StiDetecotrLayerContainer::print()
    static void printHits(); //print all hits in event
    static void printHitContainerForDetector(); //print hits for the current detector layer
    static void printVertices();
    
    static void setVisible(); //call StiDisplayManager::setVisible()
    static void setInvisible(); //call StiDisplayManager::setInvisible()
    
    static void setSvtVisible(); //call StiDisplayManager::setSvtVisible()
    static void setSvtInvisible(); //call StiDisplayManager::setSvtInvisible()

    static void setTpcVisible(); //call StiDisplayManager::setTpcVisible()
    static void setTpcInvisible(); //call StiDisplayManager::setTpcInvisible()

    static void setSkeletonView(); //call StiDisplayManager::makeSkeletonView

    //Navigate through detector (should be sub-menued/cascaded)
    static void setCurrentDetectorToDefault();
    static void showCurrentDetector(); //Show the current
    
    static void setLayer(); //call StiDetectorLayerContainer::setSector(int)
    static void setLayerAndAngle(); //call StiDetectorLayerContainer::setSector(int sector, int padrow)
    
    static void moveIn();
    static void moveOut();
    
    static void movePlusPhi();
    static void moveMinusPhi();
    
    static void memoryInfo();
    static void printFactorySize();

    static void toggleFitFind();
    
    //General access
    void setStChain(StChain* val) {mchain=val;}
    
private:
    static int mnevent;
    static TControlBar *makeControlBar();
    static StChain* mchain; //!
    
    TControlBar* mbar; //!
    
    ClassDef(StiControlBar, 1) //StiControlBar
};

#endif
