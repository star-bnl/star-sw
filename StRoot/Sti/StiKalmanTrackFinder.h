#ifndef StiKalmanTrackFinder_H
#define StiKalmanTrackFinder_H 1

#include "StiTrackFinder.h"
#include "Messenger.h"

class StiDetector;
class StiDectorContainer;
class StiTrack;
class StiKalmanTrack;

enum StiFindStep {StepByLayer=1,StepByDetector=2 };

class StiKalmanTrackFinder : public StiTrackFinder
{
public:
    StiKalmanTrackFinder();
    ~StiKalmanTrackFinder();
    //action methods_______________________________________________
    //inherited
    virtual void reset();
    virtual void findTracks();
    virtual bool isValid(bool debug=false) const; //Check if everything is kosher
    
    virtual void doTrackFit();
    virtual void doTrackFind();
    virtual bool hasMore();
    
    virtual void setElossCalculated(bool option);
    virtual void setMCSCalculated(bool option);
    void   setMassHypothesis(double m);
    double getMassHypothesis();
    
    //Local
    virtual void findTrack(StiTrack * t); //throw ( Exception);
    virtual StiKalmanTrackNode * followTrackAt(StiKalmanTrackNode * node); //throw (Exception);
    void removeNodeFromTrack(StiKalmanTrackNode * node, StiKalmanTrack* track);
    void pruneNodes(StiKalmanTrackNode * node);
    void reserveHits(StiKalmanTrackNode * node);
    bool extendToMainVertex(StiKalmanTrackNode * node);
    
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
    
    //double getYWindow(StiKalmanTrackNode * n, StiHit * h) const;
    //double getZWindow(StiKalmanTrackNode * n, StiHit * h) const;
    
protected:
    
    int    singleNodeFrom;
    bool   singleNodeDescent;
    double massHypothesis;
    double maxChi2ForSelection;
    
    
    void printState();
    
private:
    
    StiFindStep mode;
    int       state;
    //int       hitCount;
    //int       nullCount; 
    //int       contiguousHitCount;	
    //int       contiguousNullCount;	
    int       visitedDet ;
    int       position;
    int       lastMove;
    
    //double    sAlpha, tAlpha;
    //double    yWindow,zWindow;
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
    void search();
    
    Messenger & trackMes;
};

#endif


