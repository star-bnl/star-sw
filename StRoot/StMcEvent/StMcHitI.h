#ifndef STAR_STMCHITI
#define STAR_STMCHITI
#include "StThreeVectorF.hh"
// MS event hit intergace

class StMcHitI {
  public:
    enum EMcHitBits {
       kMatched = BIT(23) // if hit has matched with reconstructed one
    };
    StMcHitI() {}
    virtual ~StMcHitI() {}
    virtual StThreeVectorF             position() const;
    virtual float                             x() const  =0;
    virtual float                             y() const  =0;
    virtual float                             z() const  =0;
    virtual StThreeVectorF         localMomentum() const;
    virtual float                            px() const  =0;
    virtual float                            py() const  =0;
    virtual float                            pz() const  =0;
    virtual float                            dE() const  =0;
    virtual float                            dS() const  =0;
    virtual long                            key() const  =0;
    virtual long                       volumeId() const  =0;
    virtual float                           tof() const  =0;

    virtual long               parentTrackIndex() const = 0;
};

#define MCHITCLASS(CLASSNAME, TYPE)                       \
class CLASSNAME : public StMcHitT<TYPE*> {                \
protected:                                                \
     CLASSNAME(){;}                                       \
public:                                                   \
    CLASSNAME(TYPE*data) :StMcHitT<TYPE*>(data) {;}       \
    virtual ~CLASSNAME(){;}

#define ENDMCHITCLASS };

#endif
