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
    
    bool followTrackAtNode(StiTrack * t, StiTrackNode * node);
    bool propagateTrackAtNodeTo(StiTrack * t, 
				StiTrackNode * node, 
				StiDetector  * sDet,
				StiDetector  * tDet);
    bool exploreTrackAtNode(StiTrack * t, StiTrackNode * parentNode, StiTrackNode * workNode);
    bool followBestTrackAtNode(StiTrack * t, StiTrackNode * node, StiTrackNode * wNode);
    
    double getPredictedChi2(const StiTrackNode * node, const StiHit *hit) const ;
    
    StiTrackNode * updateTrackAtNode(StiTrackNode * node, StiTrackNode * wNode, StiHit * hit, double chisq);
    int rotate(StiTrackNode * node, double alpha);
    void removeNodeFromTrack(StiTrackNode * node, StiTrack* track);
    void pruneNodes(StiTrackNode * node);
    StiTrackNode * findBestBranch(StiTrackNode * node);
    bool extendToMainVertex(StiTrackNode * node);
    void   setMassHypothesis(double m) { massHypothesis=m;};
    double getMassHypothesis()         { return massHypothesis;};
    
    double getYWindow(StiTrackNode * n, StiHit * h) const;
    double getZWindow(StiTrackNode * n, StiHit * h) const;
    
private:
    
    int    singleNodeFrom;
    bool   singleNodeDescent;
    double maxChi2ForSelection;
    double massHypothesis;
};


#endif


