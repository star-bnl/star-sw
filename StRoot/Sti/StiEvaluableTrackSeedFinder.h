//StiEvaluableTrackSeedFinder.h
//M.L. Miller (Yale Software)
//04/01

/*! \class StiEvaluableTrackSeedFinder.
  StiEvaluableTrackSeedFinder is used to provide candidate seeds for the track finder
  when running on simulated data.  These seeds are of type StiEvaluableTrack, and
  encapsulate information regarding the monte-carlo track and the StGlobalTrack found by
  existing STAR tracking.  The association is performed via StAssociationMaker.  The
  best match is chosen from all possible matches, and this match must pass a cut on the
  number of matched points.  This cut is explained in makeTrack().
  <p>
  StiEvaluableTrackSeedFinder is a StiSeedFinder, so it supports the enforced user interface,
  namely hasMore() and next().  Additionaly, one can control the nature of the generated
  seeds.  StiEvaluableTrackSeedFinder is, like all StiSeedFinder objects, dynamically buildable.
  Currently this is done by parsing a text file.  However, it is understood that the
  information necessary for a dynamic build must eventually come from a data base.  For more
  information, see the documentation for setBuildPath() and build() methods.
  <p>
  StiEvaluableTrackSeedFinder requires a valid pointer to an instance of type
  StAssociationMaker in the constructor call.  Contsturctors of other types are
  explicitly prohibited.

  \author M.L. Miller (Yale Software)
*/


#ifndef StiEvaluableTrackSeedFinder_HH
#define StiEvaluableTrackSeedFinder_HH

#include <vector>
using std::vector;
#include <map>
using std::map;
#include <string>
using std::string;

#include "StiSeedFinder.h"
#include "StiObjectFactoryInterface.h"

class StiKalmanTrack;
class StMcEvent;
class StMcTrack;
class StAssociationMaker;
class StTrackPairInfo;
class StTpcHitFilter;

class StiEvaluableTrackSeedFinder : public StiSeedFinder
{
public:
    
    ///This is the only constructor available.
    /*! We require a valid pointer to an StAssociationMaker object.  All other constructor
      types are excplicity prohibited.  It is assumed, however, that the StAssociationMaker
      object is owned by some other scope.
     */
    StiEvaluableTrackSeedFinder(StAssociationMaker*);

    ///Default destructor.
    virtual ~StiEvaluableTrackSeedFinder();
    
    //Sets

    ///Set a pointer to StMcEvent
    /*! This should be called once per event.  The call to setEVent internally initializes
      the seed finder for the event.  Without this call, the behavior of hasMore() and next()
      is undefined.
    */
    void setEvent(StMcEvent* mcevt=0);

    ///Set a pointer to the track factory.
    void setFactory(StiObjectFactoryInterface<StiKalmanTrack>* val);

    ///Set the path of the file from which to build the seed-finder.
    void setBuildPath(const string&);
    
    //User query interface to StiKalmanTracks

    ///Are there more tracks to be had?
    /*! A call to hasMore() simply checks if there are more seeds to be generated.
      It does not implement any increment or decrement calls, and thus may be called without
      ever changing the internal state of the seed finder.
     */
    virtual bool hasMore();

    ///Access to the next track.
    /*! A call to next() constructs a seed from the current m.c. track and increments
      a pointer to the next available m.c. track.  The generation of the seed itself is
      performed by the private makeTrack() method.
    */
    virtual StiKalmanTrack* next();

    ///Dynamically build the internal state of the seed-finder.
    /*! A call to build builds the internal state from the text file specified by
      buildPath.  If buildPath is not initialized, warning messages are streamed and
      behavior of the seed-finder object is undefined.
     */
    
    virtual void build();
    
    ///This performs no operation.
    /*! This call is inherited from StiSeedFinder but does not make much sense in the context
      of evaluable seeds.  That is, the internal state cannot be reset without a call to
      setEvent().
    */
    virtual void reset();

protected:
    ///Construct an evaluable track from a m.c. track
    /*! This function is the heart of the seed-finder.  It finds the best associated match
      from the StAssociationMaker instance and initializes the StiKalmanTrack state with
      the parameters from the m.c. track and the hits from the StGlobalTrack.
    */
    StiEvaluableTrack* makeTrack(StMcTrack*);
    
private:
    StiEvaluableTrackSeedFinder(); //Not implemented, gotta have association maker
    
    StAssociationMaker* mAssociationMaker;
    StMcEvent* mMcEvent;
    StiObjectFactoryInterface<StiKalmanTrack>* mFactory;
    StTpcHitFilter* mTpcHitFilter; //deep memeber, requires non-defualt assignment and copy
    string mBuildPath;
    bool mBuilt;

    vector<StMcTrack*>::iterator mCurrentMc;
    vector<StMcTrack*>::iterator mBeginMc;
    vector<StMcTrack*>::iterator mEndMc;
};

//Stl utility functors

class BestCommonHits
{
public:
    typedef pair<StMcTrack*, StTrackPairInfo*> McToStPair_t;
    
    BestCommonHits();

    void operator()(const McToStPair_t& rhs);
    
    StTrackPairInfo* pair() const {return mPair;}
    
private:
    unsigned int mMostCommon;
    StTrackPairInfo* mPair;
};
    
inline void StiEvaluableTrackSeedFinder::setFactory(StiObjectFactoryInterface<StiKalmanTrack>* val)
{
    mFactory=val;
}

inline void StiEvaluableTrackSeedFinder::setBuildPath(const string& val)
{
    mBuildPath = val;
}
#endif


