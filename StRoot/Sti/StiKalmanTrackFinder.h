#ifndef StiKalmanTrackFinder_H
#define StiKalmanTrackFinder_H 1

#include <iostream>
using std::cout;
using std::endl;

#include "StiTrackFinder.h"
#include "StiKalmanTrackNode.h"
#include "StiKalmanTrackFinderParameters.h"
#include "Messenger.h"
#include "SubjectObserver.h"

class StiDetector;
class StiDectorContainer;
class StiTrack;
class StiKalmanTrack;

enum StiFindStep {StepByLayer=1,StepByDetector=2 };

class StiKalmanTrackFinder : public StiTrackFinder, public Observer
{
public:
    StiKalmanTrackFinder(StiToolkit * userToolkit);
    ~StiKalmanTrackFinder();
    //action methods_______________________________________________

    //Inherited from Observer
    virtual void update(Subject* changedSubject);
    virtual void forgetSubject(Subject* theObsoleteSubject);
    
    //inherited
    virtual void findTracks();
		virtual void fitTracks(); 
		virtual void extendTracksToVertex(StiHit * vertex);
		virtual void findNextTrack();
		virtual void fitNextTrack();
		virtual void findNextTrackSegment();
    
		virtual void reset();
    virtual bool isValid(bool debug=false) const; //Check if everything is kosher
    
    virtual bool hasMore();
    
		void setParameters(StiKalmanTrackFinderParameters *par);
		StiKalmanTrackFinderParameters * getParameters();

    //Local
    virtual void findTrack(StiTrack * t); //throw ( Exception);
    void removeNodeFromTrack(StiKalmanTrackNode * node, StiKalmanTrack* track);
    void pruneNodes(StiKalmanTrackNode * node);
    void reserveHits(StiKalmanTrackNode * node);
    void extendToMainVertex(StiKalmanTrackNode * node, StiHit * vertex);
    
    void doInitTrackSearch();
    void doScanLayer();
    void doInitLayer();
    void doNextDetector();
    void doFinishLayer();
    void doFinishTrackSearch();
    void doNextTrackStep();
    void setStepMode(StiFindStep m)
			{
				mode = m;
			}
    StiFindStep getStepMode()
			{
				return mode;
			}
    
protected:

    void getNewState();
    
    //int    singleNodeFrom;
    //bool   singleNodeDescent;
    //double massHypothesis;
    //double maxChi2ForSelection;
    void printState();
    
private:
    
    StiFindStep mode;
    int       state;
    int       visitedDet ;
    int       position;
    int       lastMove;
    
    double    chi2;
    double    bestChi2;
    StiKalmanTrack         * track;
    StiKalmanTrackNode * sNode;
    StiKalmanTrackNode * tNode;
    StiKalmanTrackNode * bestNode;
    StiKalmanTrackNode * leadNode;
    StiHit * bestHit;
    StiHit * hit;
    const StiDetector * sDet;
    const StiDetector * tDet;
    const StiDetector * leadDet;
    bool trackDone;
    bool scanningDone;
    bool hasHit;
    bool hasDet;
    
    void initSearch(StiKalmanTrackNode * node);
    
    Messenger & trackMes;
		StiKalmanTrackFinderParameters * pars;
		Subject * mSubject;

};

//inlines


inline void StiKalmanTrackFinder::update(Subject* changedSubject)
{
    // cout <<"StiKalmanTrackFinder::update(Subject*)"<<endl;
    if (changedSubject!=mSubject) {
	cout <<"StiKalmanTrackFinder::update(Subject*). ERROR:\t"
	     <<"changedSubject!=mSubject"<<endl;
    }
    else {
	// cout <<"getting new values"<<endl;
	getNewState();
	// cout <<"\tdone getting new values"<<endl;
    }
}

inline void StiKalmanTrackFinder::forgetSubject(Subject* obsolete)
{
    // cout <<"StiKalmanTrackFinder::forgetSubject(Subject*)"<<endl;
    if (obsolete==mSubject) {
	mSubject=0;
    }
    else {
	cout <<"StiKalmanTrackFinder::forgetSubject(Subject*). ERROR:\t"
	     <<"changedSubject!=mSubject"<<endl;
    }
}

inline void StiKalmanTrackFinder::setParameters(StiKalmanTrackFinderParameters *par)
{
	pars = par;
	StiKalmanTrackNode::pars = par;
}


#endif



