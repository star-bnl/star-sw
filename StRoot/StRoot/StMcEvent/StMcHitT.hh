/***************************************************************************
 *
 * $Id: StMcHitT.hh,v 2.3 2010/05/04 23:58:43 fine Exp $
 * $Log: StMcHitT.hh,v $
 * Revision 2.3  2010/05/04 23:58:43  fine
 * Vertex, and emc models
 *
 * Revision 2.2  2010/04/28 20:15:45  fine
 * Implementation if the new OO for Mc hits
 *
 * Revision 2.1  2010/04/28 18:10:11  fine
 * New OO model for Mc event components
 *
 * Revision 2.10  2005/11/22 21:44:52  fisyak
 * Add compress Print for McEvent, add Ssd collections
  *
 *
 **************************************************************************/
#ifndef StMcHitT_hh
#define StMcHitT_hh

#include "StEventObject.h"
#include "StMcHitI.h"

template <class T>
class StMcHitT : public StEventObject<T>, public StMcHitI {
protected:
    StMcHitT(){;}
public:
    StMcHitT(T data) :StEventObject<T>(data) {;}
    // StMcHitT(const StSvtHit&);                  use default
    // const StMcHitT & operator=(const StMcHitT&);   use default
    virtual ~StMcHitT(){;}
    
    int operator==(const StMcHitT&r) const
    { return StEventObject<T>::operator==(r); }
    int operator!=(const StMcHitT&r) const
    { return StEventObject<T>::operator!=(r); }
    // "Get" Methods
    virtual float                             x() const { return StEventObject<T>::fData->x[0];      }
    virtual float                             y() const { return StEventObject<T>::fData->x[1];      }
    virtual float                             z() const { return StEventObject<T>::fData->x[2];      }
    virtual float                            px() const { return StEventObject<T>::fData->p[0];      }
    virtual float                            py() const { return StEventObject<T>::fData->p[1];      }
    virtual float                            pz() const { return StEventObject<T>::fData->p[2];      }
    virtual float                            dE() const { return StEventObject<T>::fData->de;        }
    virtual float                            dS() const { return StEventObject<T>::fData->ds;        }
    virtual long                            key() const { return StEventObject<T>::fData->id;        }
    virtual long                       volumeId() const { return StEventObject<T>::fData->volume_id; }
    virtual long               parentTrackIndex() const { return StEventObject<T>::fData->track_p;   }
    virtual float                           tof() const { return StEventObject<T>::fData->tof;       }
};

struct g2t_emc_hit_st;
template <>
class StMcHitT<g2t_emc_hit_st*> : public StEventObject<g2t_emc_hit_st*>, public StMcHitI {
protected:
    StMcHitT(){;}
public:
    StMcHitT(g2t_emc_hit_st *data) :StEventObject<g2t_emc_hit_st*>(data) {;}
    // StMcHitT(const StSvtHit&);                  use default
    // const StMcHitT & operator=(const StMcHitT&);   use default
    virtual ~StMcHitT(){;}
#if 0
    int operator==(const StMcHitT&r) const
    { return StEventObject<g2t_emc_hit_st*>::operator==(r); }
    int operator!=(const StMcHitT&r) const
    { return StEventObject<g2t_emc_hit_st*>::operator!=(r); }
#endif
    // "Get" Methods
    virtual float                            dE() const { return StEventObject<g2t_emc_hit_st*>::fData->de;        }
    virtual long                            key() const { return StEventObject<g2t_emc_hit_st*>::fData->id;        }
    virtual long                       volumeId() const { return StEventObject<g2t_emc_hit_st*>::fData->volume_id; }
    virtual long               parentTrackIndex() const { return StEventObject<g2t_emc_hit_st*>::fData->track_p;   }
};

#endif

