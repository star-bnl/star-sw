//                                                                           
#ifndef StMcMtdHit_hh
#define StMcMtdHit_hh

#include "StObject.h"
#include "Stiostream.h"
#include "StThreeVectorF.hh"
#include "StMcHit.hh"

//#include "StMemoryPool.hh"

class StMcTrack;
class g2t_mtd_hit_st;


class StMcMtdHit : public StMcHit {
public:
  StMcMtdHit();
  StMcMtdHit(int, int, int, float, float, float, float, float); //! backleg, module, cell, dE, pathLength, tof_exact, tof_measured, charge
  StMcMtdHit(int, int, int, float, float, float, float, float, StThreeVectorF&, StMcTrack*);// above + position + partentTrack
  StMcMtdHit(g2t_mtd_hit_st*);

  virtual ~StMcMtdHit();
    
    int operator==(const StMcMtdHit&) const;
    int operator!=(const StMcMtdHit&) const;
   void operator+=(const StMcMtdHit&);

    bool sameCell(const StMcMtdHit&) const;

  // "Get" Methods
    virtual int                         backleg() const;
    virtual int                          module() const;
    virtual int                            cell() const;
    virtual float                    pathLength() const;
    virtual float                          time() const; //! time of flight geant
    virtual float                           tof() const; //! tof simulated
    virtual float                        charge() const; //! collected charge
    virtual int                   parentTrackId() const;	

  // "Set" Methods

    virtual void setBackleg(int);
    virtual void setModule(int);
    virtual void setCell(int);
    virtual void setPathLength(float);
    virtual void setTime(float);
    virtual void setTof(float);
    virtual void setCharge(float);
    virtual void setParentTrackId(int);
    
  //  void* operator new(size_t)     { return mPool.alloc(); }
  //  void  operator delete(void* p) { mPool.free(p); }
    
protected:
    int                  mBackleg;
    int                  mModule;
    int                  mCell;
    float                mPathLength;
    float                mTime;
    float                mTof;
    float                mCharge;
    int			 mParentTrackId;

    ClassDef(StMcMtdHit,1)//
};

ostream&  operator<<(ostream& os, const StMcMtdHit&);

inline   int StMcMtdHit::backleg()	const {return mBackleg; 	}
inline   int StMcMtdHit::module() 	const {return mModule; 		}
inline   int StMcMtdHit::cell() 	const {return mCell; 		}
inline float StMcMtdHit::pathLength()  	const {return mPathLength; 	}
inline float StMcMtdHit::time()  	const {return mTime; 		}
inline float StMcMtdHit::tof() 		const {return mTof; 		}
inline float StMcMtdHit::charge() 	const {return mCharge; 		}
inline   int StMcMtdHit::parentTrackId()const {return mParentTrackId; 	}

#endif

