/*StGlobalTrackRefitter.h
M.L. Miller (Yale Software)
04/01
modified by bum
*/
 
#ifndef StGlobalTrackRefitter_HH
#define StGlobalTrackRefitter_HH

#include "Stiostream.h"
#include "StPhysicalHelixD.hh"
#include "TObject.h"
#include <vector>

class StFastCircleFitter;
class StTptCircleFitter;
class StFastLineFitter;
class StPhysicalHelixD;
class StTrack;
class StMcTrack;
class StHit;
class StTpcHit;
class StMcTpcHit;
class StMcHit;
//class StThreeVectorF;


#ifndef __CINT__
#include <utility>

typedef vector<StTpcHit*> VECHIT;
typedef vector<StMcTpcHit*> VECMCHIT;
//contains position and error 
typedef pair<const StThreeVectorF*,StThreeVectorF*> HITPAIR;
typedef vector<HITPAIR> VECPAIR;

bool sortInc(StTpcHit*,StTpcHit*);
bool sortMcInc(StMcTpcHit*,StMcTpcHit*);
//bool sortPairInc(HITPAIR*,HITPAIR*);
bool sortPairInc(HITPAIR,HITPAIR);

#endif

class StGlobalTrackRefitter : public TObject
{
public:
    StGlobalTrackRefitter();
    virtual ~StGlobalTrackRefitter();

    //Sets
    void setHitFlagFilter(bool val) {mDoFlagFilter=val;}
    void setHitFitFlagFilter(bool val) {mDoFitFilter=val;}
    void setMinPadrow(unsigned int val) {mMinPadrow=val;}
    void setMaxPadrow(unsigned int val) {mMaxPadrow=val;}
    void setDebug(int val=1) { mDebug=val;}
    void setTptCircleFitter(bool val=true) { mUseTptCircleFitter=val;}
    void setBField(Float_t val); // in tesla

    // setting ridiculous errors on the padrows not within
    // min and max padrow instead of not using the hits
    // in the fit.  the padrow and z errors are hard coded.
    void setError(bool val=true, float padErr=10., float zErr=10.) { 
      mDoError = val; mPadrowError=padErr; mZError=zErr; 
    }
    void setSameError(bool val=true) { mDoSameError=val;}


    //gets
    bool  hitFlagFilter() const { return mDoFlagFilter; }
    bool  hitFitFlagFilter() const { return mDoFitFilter; }
    int   minPadrow() const { return mMinPadrow;}
    int   maxPadrow() const { return mMaxPadrow;}
    bool  useTptCircleFitter() const { return mUseTptCircleFitter; }
    int   refitPoints() const { return mRefitPoints;}
    float padrowError() const { return mPadrowError; }
    float zError() const { return mZError; }
    bool  doError() const { return mDoError; }
    bool  doSameError() const { return mDoSameError; }

    void addPoint(StThreeVectorF&,StThreeVectorF&);
    
    //Action
    const StPhysicalHelixD* refitNoFill(StTrack *track);
    
    const StPhysicalHelixD* refit(StTrack* track);
    const StPhysicalHelixD* refit(StMcTrack* track);
    const StPhysicalHelixD* refitLine(StTrack* track);
    const StPhysicalHelixD* refitLine(StMcTrack* track);
    
private:
    const  StPhysicalHelixD* doHelixRefit(double h);
    const  StPhysicalHelixD* doLineRefit(); 
    void   clear();
    bool   fillRcHits(StTrack*);
    bool   fillMcHits(StMcTrack*);
    // 2d pathlength for circles
    double pathlength(double radius, double xcenter, double ycenter,
		      const StThreeVectorF& hit1, 
		      const StThreeVectorF& hit2);

    StThreeVectorF lineAt2D(double phase, double xorigin, double yorigin,
			     const StThreeVectorF& point);
    // 2d pathlength for lines
    double  pathlength(double phase,double xorigin, double yorigin,
		      const StThreeVectorF& point);

    bool filter(const StHit*) const;     
    bool flagFilter(const StHit*) const; 
    bool fitFilter(const StHit*) const;
    bool padrowFilter(const StHit*) const;
    bool filter(const StMcHit*) const;
    bool padrowFilter(const StMcHit*) const;
    StThreeVectorF* hitError(const StHit*);
    StThreeVectorF* hitError(const StMcHit*);

    unsigned int   mMinPadrow; //!
    unsigned int   mMaxPadrow;  //!
    float mPadrowError; //!
    float mZError; //!
    bool  mDoError; //!
    bool  mDoSameError; //!
    bool  mDoFlagFilter;  //!
    bool  mDoFitFilter;   //!
    short mRefitPoints; //!
    bool  mUseTptCircleFitter; //!

    StFastCircleFitter* mCircleFitter; //!
    StTptCircleFitter* mTptCircleFitter;//!
    StFastLineFitter* mLineFitter;     //!
    StPhysicalHelixD* mRefitHelix;     //!
    int mDebug;//!
    Float_t mBField; //! only used (maybe) if setting h

#ifndef __CINT__
    vector<HITPAIR> mVecHitPair; //! contains the sorted hits 
#endif
    StThreeVectorF* mErrorArray; //! 

    ClassDef(StGlobalTrackRefitter,0)

};

//non-members
inline ostream& operator<< (ostream& os, const StGlobalTrackRefitter& f)
{
  os <<"\tHitFlagFilter: "<<f.hitFlagFilter()<< endl
     <<"\tHitFitFlagFilter: "<<f.hitFitFlagFilter()<< endl
     <<"\tMinPadrow: "<<f.minPadrow()<< endl
     <<"\tMaxPadrow: "<<f.maxPadrow()<<endl 
     <<"\tUse tpt refitter?: "
     << ((f.useTptCircleFitter()) ? "yes" : "no") 
     << endl;

  if(f.doError()){
    os << "\tSetting ridiculous errors is ON." << endl;
    os << "\tHits not within the accepted padrows will be assigned"<< endl
       << "\t\tpadrow error: " << f.padrowError()
       << ", z error: " << f.zError() 
       << endl;
  }
  else{
    os << "\tSetting ridiculous errors is OFF." << endl;
  }
  
  os << "\tErrors used on refit hits: " 
     << (f.doSameError() ? "Dummy error" : "Errors from StEvent")
     << endl;


  return os;
}

#endif

