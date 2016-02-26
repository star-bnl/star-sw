/*!
 * \class StTofHit 
 * \author Wei-Ming Zhang, Dec 2000
 */
/***************************************************************************
 *
 * $Id: StTofHit.h,v 2.11 2016/02/25 17:10:20 ullrich Exp $
 *
 * Author: Wei-Ming Zhang, Dec 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofHit.h,v $
 * Revision 2.11  2016/02/25 17:10:20  ullrich
 * Implemented detector() which is now a pure abstract method in StHit.
 *
 * Revision 2.10  2004/07/15 16:36:25  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.9  2004/02/05 17:59:44  ullrich
 * Changed $LINK to StLink mechanism and add new member.
 *
 * Revision 2.8  2003/09/02 17:58:05  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.7  2003/07/09 20:14:20  ullrich
 * New methods added.
 *
 * Revision 2.6  2003/05/21 18:22:46  ullrich
 * Major Revision of ToF classes (F. Geurts)
 *
 * Revision 2.5  2002/02/22 22:56:51  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.4  2001/04/16 20:49:00  ullrich
 * Fixed typo in setSlatIndex().
 *
 * Revision 2.3  2001/04/05 04:00:43  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.2  2001/03/24 03:34:59  perev
 * clone() -> clone() const
 *
 * Revision 2.1  2000/12/21 23:52:25  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StTofHit_hh
#define StTofHit_hh

#include <Stiostream.h>
#include "StHit.h"

class StParticleDefinition;
class StTrack;

class StTofHit : public StHit {
public:
    StTofHit();
    ~StTofHit();

    int   trayIndex() const;
    int   moduleIndex() const;
    int   cellIndex() const;
    int   daqIndex() const;
    int   cellCollIndex() const;
    float timeOfFlight() const;
    float pathLength() const;
    float beta() const;

    StTrack*       associatedTrack();
    const StTrack* associatedTrack() const;

    float tofExpectedAsElectron() const;
    float tofExpectedAsPion() const;
    float tofExpectedAsKaon() const;
    float tofExpectedAsProton() const;

    float sigmaElectron() const;
    float sigmaPion() const;
    float sigmaKaon() const;
    float sigmaProton() const;

    StParticleDefinition*       particleHypothesis();
    const StParticleDefinition* particleHypothesis() const;

    void setTrayIndex(int);
    void setModuleIndex(int);
    void setCellIndex(int);
    void setCellCollIndex(int);
    void setDaqIndex(int);
    void setTimeOfFlight(float);
    void setPathLength(float);
    void setBeta(float);
    void setAssociatedTrack(StTrack*);
    void setTofExpectedAsElectron(float);
    void setTofExpectedAsPion(float);
    void setTofExpectedAsKaon(float);
    void setTofExpectedAsProton(float);
    void setSigmaElectron(float);
    void setSigmaPion(float);
    void setSigmaKaon(float);
    void setSigmaProton(float);
    void setParticleHypothesis(StParticleDefinition*);
    
    StDetectorId detector() const;

 protected:
    Int_t   mTrayIndex;
    Int_t   mModuleIndex;
    Int_t   mCellIndex;
    Int_t   mDaqIndex;
    Int_t   mCellCollIndex;
    Float_t mTimeOfFlight;
    Float_t mPathLength;
    Float_t mBeta;
    //    StTrack *mAssociatedTrack;   //$LINK
#ifdef __CINT__
    StObjLink        mAssociatedTrack;		
#else
    StLink<StTrack>  mAssociatedTrack;		
#endif //__CINT__
    Float_t mTOFExpectedAsElectron;
    Float_t mTOFExpectedAsPion;
    Float_t mTOFExpectedAsKaon;
    Float_t mTOFExpectedAsProton;
    Float_t mSigmaElectron;
    Float_t mSigmaPion;
    Float_t mSigmaKaon;
    Float_t mSigmaProton;
    StParticleDefinition *mParticleHypothesis;

    ClassDef(StTofHit,3)
};

inline StDetectorId StTofHit::detector() const {return static_cast<StDetectorId>(StHit::bits(0, 4));}

#endif
