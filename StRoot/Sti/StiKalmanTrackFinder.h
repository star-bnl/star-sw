#ifndef StiKalmanTrackFinder_H
#define StiKalmanTrackFinder_H 1

#include "StiTrackFinder.h"
#include "Exception.h"

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

private:
    //Local
    int  findTrack(StiTrack * t) throw ( Exception);
    
    void fitInward(StiKalmanTrackNode * node) throw (Exception) ;
    void fitOutward(StiKalmanTrackNode * node) throw (Exception) ;

    void followTrackAtNode(StiKalmanTrackNode * node) throw (Exception) ;
    int  propagate(StiKalmanTrackNode * node, 
				     StiDetector  * sDet,
				     StiDetector  * tDet) throw ( Exception) ; 
    void exploreNode(StiKalmanTrackNode * sNode,
				       StiKalmanTrackNode * tNode) throw (Exception) ; 

    void updateNode(StiKalmanTrackNode * node, double chisq) throw ( Exception) ; 
    void followBestNode(StiKalmanTrackNode * sNode, 
			StiKalmanTrackNode * tNode) throw ( Exception);
    
    double evaluateChi2(const StiKalmanTrackNode * node, 
			const StiHit *hit) const  throw ( Exception);
    
    void rotate(StiKalmanTrackNode * node, double alpha)  throw ( Exception);
    void removeNodeFromTrack(StiKalmanTrackNode * node, StiKalmanTrack* track);
    void pruneNodes(StiKalmanTrackNode * node);
    StiKalmanTrackNode * findBestBranch(StiKalmanTrackNode * node) throw ( Exception);
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


