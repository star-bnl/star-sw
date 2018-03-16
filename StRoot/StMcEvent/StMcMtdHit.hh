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
  StMcMtdHit(g2t_mtd_hit_st*);

  virtual ~StMcMtdHit();
    
    int operator!=(const StMcMtdHit&) const;

    bool sameCell(const StMcMtdHit&) const;

  // "Get" Methods
    virtual int                         backleg() const;
    virtual int                          module() const;
    virtual int                            cell() const;
    virtual float                    pathLength() const;
    virtual int                   parentTrackId() const;	

  // "Set" Methods

  //  void* operator new(size_t)     { return mPool.alloc(); }
  //  void  operator delete(void* p) { mPool.free(p); }
    
protected:
    int                  mBackleg;
    int                  mModule;
    int                  mCell;
    float                mPathLength;
    int			 mParentTrackId;

    ClassDef(StMcMtdHit,1)//
};

ostream&  operator<<(ostream& os, const StMcMtdHit&);

inline   int StMcMtdHit::backleg()	const {return mBackleg; 	}
inline   int StMcMtdHit::module() 	const {return mModule; 		}
inline   int StMcMtdHit::cell() 	const {return mCell; 		}
inline float StMcMtdHit::pathLength()  	const {return mPathLength; 	}
inline   int StMcMtdHit::parentTrackId()const {return mParentTrackId; 	}

#endif

