/***************************************************************************
 *
 * $Id: StMcVertexC.h,v 2.1 2010/05/04 23:58:43 fine Exp $
 * $Log: StMcVertexC.h,v $
 * Revision 2.1  2010/05/04 23:58:43  fine
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
#ifndef STMCVERTEXT_HH
#define STMCVERTEXT_HH

#include "StEventObject.h"
#include "StMcHitI.h"
#include "g2t_vertex.h"

#include "TString.h"

class StMcVertexC : public StEventObject<g2t_vertex_st*> , public StMcHitI {
protected:
    StMcVertexC(){;}
public:
    StMcVertexC(g2t_vertex_st*data) : StEventObject<g2t_vertex_st*>(data) {;}
    // StMcVertexC(const StSvtHit&);                  use default
    // const StMcVertexC & operator=(const StMcVertexC&);   use default
    virtual ~StMcVertexC(){;}
    int operator==(const StMcVertexC&r) const;
    int operator!=(const StMcVertexC&r) const
    { return !operator==(r); }
    // "Get" Methods
    virtual float                             x() const { return StEventObject<g2t_vertex_st*>::fData->ge_x[0];   }
    virtual float                             y() const { return StEventObject<g2t_vertex_st*>::fData->ge_x[1];   }
    virtual float                             z() const { return StEventObject<g2t_vertex_st*>::fData->ge_x[2];   }
    
    virtual long                   geantProcess() const { return StEventObject<g2t_vertex_st*>::fData->ge_proc;   }
    virtual long               generatorProcess() const { return StEventObject<g2t_vertex_st*>::fData->eg_proc;   }
    virtual long                    geantMedium() const { return StEventObject<g2t_vertex_st*>::fData->ge_medium; }

    virtual long                            key() const { return StEventObject<g2t_vertex_st*>::fData->id;        }
    virtual long                       volumeId() const { long tmp;
                                                          memcpy(&tmp, &StEventObject<g2t_vertex_st*>::fData->ge_volume,4);
                                                          return tmp; 
                                                        }
    virtual float                           tof() const { return StEventObject<g2t_vertex_st*>::fData->eg_tof;    }
    TString                         geantVolume() const { char tmp[5]={0};
                                                          strncpy(tmp, StEventObject<g2t_vertex_st*>::fData->ge_volume,4);
                                                          return TString(tmp); 
                                                        }
#if 0
    StPtrVecMcTrack&            daughters()               { return return StEventObject<g2t_vertex_st*>::fData->*&mDaughters; } 
    const StPtrVecMcTrack&      daughters() const         { return return StEventObject<g2t_vertex_st*>::fData->*&mDaughters; } 
    unsigned int                numberOfDaughters()       { return return StEventObject<g2t_vertex_st*>::fData->mDaughters.size(); }
    unsigned int                numberOfDaughters() const { return return StEventObject<g2t_vertex_st*>::fData->mDaughters.size(); }
    StMcTrack*                  daughter(unsigned int i)  { return (i < mDaughters.size() ? mDaughters[i] : 0); }
    const StMcTrack*            daughter(unsigned int i) const { return (i < mDaughters.size() ? mDaughters[i] : 0); }
    const StMcTrack*            parent() const            { return return StEventObject<g2t_vertex_st*>::fData->mParent; }   
#endif
};

#endif

