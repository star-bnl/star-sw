/*!
 * \class StTofPidTraits 
 * \author Thomas Ullrich, Dec 2000
 */
/***************************************************************************
 *
 * $Id: StTofPidTraits.h,v 2.7 2004/07/15 16:36:25 ullrich Exp $
 *
 * Author: Thomas Ullrich, Dec 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofPidTraits.h,v $
 * Revision 2.7  2004/07/15 16:36:25  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.6  2004/07/12 21:04:16  jeromel
 * Commented out clone()
 *
 * Revision 2.5  2004/07/08 16:56:35  ullrich
 * New class member introduced.
 *
 * Revision 2.4  2002/02/22 22:56:52  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.3  2001/03/24 03:34:59  perev
 * clone() -> clone() const
 *
 * Revision 2.2  2000/12/09 02:13:23  perev
 * default StObject::clone() const used
 *
 * Revision 2.1  2000/12/08 03:52:42  ullrich
 * Initial Revision
 *
 ***************************************************************************/
#ifndef StTofPidTraits_hh
#define StTofPidTraits_hh

#include "StTrackPidTraits.h"

class StTofPidTraits : public StTrackPidTraits {
public:
    StTofPidTraits();
    StTofPidTraits(int, int, int, float, float, float);
    ~StTofPidTraits();
    
    //StTofPidTraits(const StTofPidTraits&) {/* nopt */}
    //StTofPidTraits& operator=(const StTofPidTraits&) {/* nopt */}

    int     tray() const;
    int     module() const;
    int     cell() const;
    float   tof() const;
    float   pathLength() const;
    float   beta() const;

    float   sigmaElectron() const;
    float   sigmaPion() const;
    float   sigmaKaon() const;
    float   sigmaProton() const;
        
    void    setSigmaElectron(float);
    void    setSigmaPion(float);
    void    setSigmaKaon(float);
    void    setSigmaProton(float);

    void    Print(Option_t *opt = "") const;

private:
    Int_t     mTray;
    Int_t     mModule;
    Int_t     mCell;
    Float_t   mTof;
    Float_t   mPathLength;
    Float_t   mBeta;

    Float_t   mSigmaElectron;
    Float_t   mSigmaPion;
    Float_t   mSigmaKaon;
    Float_t   mSigmaProton;

    ClassDef(StTofPidTraits,2)
};

inline int StTofPidTraits::tray() const { return mTray; }
inline int StTofPidTraits::module() const { return mModule; }
inline int StTofPidTraits::cell() const { return mCell; }
inline float StTofPidTraits::tof() const { return mTof; }
inline float StTofPidTraits::pathLength() const { return mPathLength; }
inline float StTofPidTraits::beta() const { return mBeta; }
inline float StTofPidTraits::sigmaElectron() const { return mSigmaElectron; }
inline float StTofPidTraits::sigmaPion() const { return mSigmaPion; }
inline float StTofPidTraits::sigmaKaon() const { return mSigmaKaon; }
inline float StTofPidTraits::sigmaProton() const { return mSigmaProton; }
inline void StTofPidTraits::setSigmaElectron(float sigma) { mSigmaElectron=sigma; }
inline void StTofPidTraits::setSigmaPion(float sigma) { mSigmaPion=sigma; }
inline void StTofPidTraits::setSigmaKaon(float sigma) { mSigmaKaon=sigma; }
inline void StTofPidTraits::setSigmaProton(float sigma) { mSigmaProton=sigma; }

#endif
