#ifndef StiKalmanTrackFinder_H
#define StiKalmanTrackFinder_H 1

#include "StiTrackFinder.h"

class StiDetector;
class StiDectorContainer;
class StiTrack;
class StiKalmanTrack;

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
    virtual void doNextAction();

    void initSeedFinderForStart();

private:
    //Local
    int  findTrack(StiTrack * t);
    
    bool followTrackAtNode(StiKalmanTrack * t, StiKalmanTrackNode * node);
    bool propagateTrackAtNodeTo(StiKalmanTrack * t, 
				StiKalmanTrackNode * node, 
				StiDetector  * sDet,
				StiDetector  * tDet);
    bool exploreTrackAtNode(StiKalmanTrack * t, StiKalmanTrackNode * parentNode, StiKalmanTrackNode * workNode);
    bool followBestTrackAtNode(StiKalmanTrack * t, StiKalmanTrackNode * node, StiKalmanTrackNode * wNode);
    
    double getPredictedChi2(const StiKalmanTrackNode * node, const StiHit *hit) const ;
    
    StiKalmanTrackNode * updateTrackAtNode(StiKalmanTrackNode * node, StiKalmanTrackNode * wNode, StiHit * hit, double chisq);
    int rotate(StiKalmanTrackNode * node, double alpha);
    void removeNodeFromTrack(StiKalmanTrackNode * node, StiKalmanTrack* track);
    void pruneNodes(StiKalmanTrackNode * node);
    StiKalmanTrackNode * findBestBranch(StiKalmanTrackNode * node);
    bool extendToMainVertex(StiKalmanTrackNode * node);
    void   setMassHypothesis(double m) { massHypothesis=m;};
    double getMassHypothesis()         { return massHypothesis;};
    
    double getYWindow(StiKalmanTrackNode * n, StiHit * h) const;
    double getZWindow(StiKalmanTrackNode * n, StiHit * h) const;
    
private:
    
    int    singleNodeFrom;
    bool   singleNodeDescent;
    double maxChi2ForSelection;
    double massHypothesis;
};


#endif


