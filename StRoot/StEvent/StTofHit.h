/*!
 * \class StTofHit 
 * \author Wei-Ming Zhang, Dec 2000
 */
/***************************************************************************
 *
 * $Id: StTofHit.h,v 2.6 2003/05/21 18:22:46 ullrich Exp $
 *
 * Author: Wei-Ming Zhang, Dec 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofHit.h,v $
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

#include <iostream.h>
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
    void setDaqIndex(int);
    void setTimeOfFlight(float);
    void setPathLength(float);
    void setBeta(float);
    void setAssociatedTrack(StTrack*);
    void settofExpectedAsElectron(float);
    void settofExpectedAsPion(float);
    void settofExpectedAsKaon(float);
    void settofExpectedAsProton(float);
    void setsigmaElectron(float);
    void setsigmaPion(float);
    void setsigmaKaon(float);
    void setsigmaProton(float);
    void setparticleHypothesis(StParticleDefinition*);

 protected:
    Int_t   mTrayIndex;
    Int_t   mModuleIndex;
    Int_t   mCellIndex;
    Int_t   mDaqIndex;
    Float_t mTimeOfFlight;
    Float_t mPathLength;
    Float_t mBeta;
    StTrack *mAssociatedTrack;   //$LINK
    Float_t mTOFExpectedAsElectron;
    Float_t mTOFExpectedAsPion;
    Float_t mTOFExpectedAsKaon;
    Float_t mTOFExpectedAsProton;
    Float_t mSigmaElectron;
    Float_t mSigmaPion;
    Float_t mSigmaKaon;
    Float_t mSigmaProton;
    StParticleDefinition *mParticleHypothesis;

    ClassDef(StTofHit,2)
};

#endif
