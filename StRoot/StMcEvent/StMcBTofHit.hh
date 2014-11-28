//                                                                           
#ifndef StMcBTofHit_hh
#define StMcBTofHit_hh

#include "StObject.h"
#include "Stiostream.h"
#include "StThreeVectorF.hh"

//#include "StMemoryPool.hh"

class StMcTrack;

class StMcBTofHit : public StObject {
public:
    StMcBTofHit();
    StMcBTofHit(int, int, int, float, float, float, float, float); //!tray, module, cell, dE, pathLength, tof_exact, tof_measured, charge
    StMcBTofHit(int, int, int, float, float, float, float, float, StThreeVectorF&, StMcTrack*);// above + position + partentTrack
    virtual ~StMcBTofHit();
    
    int operator==(const StMcBTofHit&) const;
    int operator!=(const StMcBTofHit&) const;
    void operator+=(const StMcBTofHit&);

    bool sameCell(const StMcBTofHit&) const;

  // "Get" Methods
    virtual int                            tray() const;
    virtual int                          module() const;
    virtual int                            cell() const;
    virtual float                            dE() const;
    virtual float                    pathLength() const;
    virtual float                          time() const; //! time of flight geant
    virtual float                           tof() const; //! tof simulated
    virtual float                        charge() const; //! collected charge
    virtual StThreeVectorF             position() const;
    virtual StMcTrack*              parentTrack() const;	
    virtual int                   parentTrackId() const;	

  // "Set" Methods

    virtual void setTray(int);
    virtual void setModule(int);
    virtual void setCell(int);
    virtual void setdE(float);
    virtual void setPathLength(float);
    virtual void setTime(float);
    virtual void setTof(float);
    virtual void setCharge(float);
    virtual void setPosition(StThreeVectorF&);
    virtual void setParentTrack(StMcTrack*);
    virtual void setParentTrackId(int);
    
  //  void* operator new(size_t)     { return mPool.alloc(); }
  //  void  operator delete(void* p) { mPool.free(p); }
    
protected:
    int                  mTray;
    int                  mModule;
    int                  mCell;
    float                mdE;
    float                mPathLength;
    float                mTime;
    float                mTof;
    float                mCharge;
    StThreeVectorF       mPosition;
    StMcTrack*           mParentTrack;
    int			 mParentTrackId;

  //private:
  //  static StMemoryPool mPool; 
    ClassDef(StMcBTofHit,1)
};

ostream&  operator<<(ostream& os, const StMcBTofHit&);

inline int  StMcBTofHit::tray() const {return mTray; }
inline int  StMcBTofHit::module() const {return mModule; }
inline int  StMcBTofHit::cell() const {return mCell; }
inline float StMcBTofHit::dE()  const {return mdE; }
inline float StMcBTofHit::pathLength()  const {return mPathLength; }
inline float StMcBTofHit::time()  const {return mTime; }
inline float StMcBTofHit::tof() const {return mTof; }
inline float StMcBTofHit::charge() const {return mCharge; }
inline StThreeVectorF StMcBTofHit::position() const {return mPosition; }
inline StMcTrack* StMcBTofHit::parentTrack() const {return mParentTrack; }
inline int StMcBTofHit::parentTrackId() const {return mParentTrackId; }

#endif

