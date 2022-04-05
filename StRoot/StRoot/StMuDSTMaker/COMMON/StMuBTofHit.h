/***************************************************************************
 *
 * $Id: StMuBTofHit.h,v 1.1 2009/02/20 17:05:59 tone421 Exp $
 *
 * Author: Xin Dong, Feb. 2009
 *
 ***************************************************************************
 *
 * Description: Tof Hit structure in MuDst
 *
 ***************************************************************************
 *
 * $Log: StMuBTofHit.h,v $
 * Revision 1.1  2009/02/20 17:05:59  tone421
 * *** empty log message ***
 *
 *
 ***************************************************************************/
         
#ifndef StMuBTofHit_hh
#define StMuBTofHit_hh

#include <sstream>
#include "StObject.h"
#include "StThreeVectorF.hh"

class StBTofHit;
#include "StMuDst.h"
#include "StMuTrack.h"

class StMuBTofHit : public StObject {
public:
    StMuBTofHit();
    StMuBTofHit(const StBTofHit* );
    ~StMuBTofHit();

    int tray() const;
    int module() const;
    int cell() const;
    double leadingEdgeTime() const;
    double trailingEdgeTime() const;
    double tot() const;

    int associatedTrackId() const;
    int index2Primary() const;
    int index2Global() const;
    
    StMuTrack *primaryTrack() const;
    StMuTrack *globalTrack() const;
    
    int idTruth() const;
    int qaTruth() const;

    void setTrayIndex(unsigned char);
    void setModuleIndex(unsigned char);
    void setCellIndex(unsigned char);
    void setLeadingEdgeTime(double);
    void setTrailingEdgeTime(double);

    void setAssociatedTrackId(short);
    void setIndex2Primary(int);
    void setIndex2Global(int);

    void setIdTruth(Int_t idtru, Int_t qatru=0);

 protected:
    UChar_t mTray;
    UChar_t mModule;
    UChar_t mCell;
    Double_t mLeadingEdgeTime;
    Double_t mTrailingEdgeTime;
    Short_t mAssociatedTrackId;
    Int_t mIndex2Primary;
    Int_t mIndex2Global;
    UShort_t mIdTruth;
    UShort_t mQuality;

    friend class StMuDst;
    
    ClassDef(StMuBTofHit,1)
};

inline int StMuBTofHit::tray() const { return mTray; }
inline int StMuBTofHit::module() const { return mModule; }
inline int StMuBTofHit::cell() const { return mCell; }
inline double StMuBTofHit::leadingEdgeTime() const { return mLeadingEdgeTime; }
inline double StMuBTofHit::trailingEdgeTime() const { return mTrailingEdgeTime; }
inline double StMuBTofHit::tot() const { return mTrailingEdgeTime-mLeadingEdgeTime; }
inline int StMuBTofHit::associatedTrackId() const { return mAssociatedTrackId; }
inline int StMuBTofHit::index2Primary() const { return mIndex2Primary; }
inline int StMuBTofHit::index2Global() const { return mIndex2Global; }
inline int StMuBTofHit::idTruth() const { return mIdTruth; }
inline int StMuBTofHit::qaTruth() const { return mQuality; }

inline StMuTrack* StMuBTofHit::primaryTrack() const { return (mIndex2Primary>=0) ? (StMuTrack*)StMuDst::array(muPrimary)->UncheckedAt(mIndex2Primary) : 0; }
inline StMuTrack* StMuBTofHit::globalTrack() const { return (mIndex2Global>=0) ? (StMuTrack*)StMuDst::array(muGlobal)->UncheckedAt(mIndex2Global) : 0; }

inline void StMuBTofHit::setTrayIndex(unsigned char tray) { mTray=tray; }
inline void StMuBTofHit::setModuleIndex(unsigned char module) { mModule=module; }
inline void StMuBTofHit::setCellIndex(unsigned char cell) { mCell=cell; }
inline void StMuBTofHit::setLeadingEdgeTime(double time) { mLeadingEdgeTime=time; }
inline void StMuBTofHit::setTrailingEdgeTime(double time) { mTrailingEdgeTime=time; }
inline void StMuBTofHit::setAssociatedTrackId(short id) { mAssociatedTrackId=id; }
inline void StMuBTofHit::setIndex2Primary(int index) { mIndex2Primary=index; }
inline void StMuBTofHit::setIndex2Global(int index) { mIndex2Global=index; }

#endif
