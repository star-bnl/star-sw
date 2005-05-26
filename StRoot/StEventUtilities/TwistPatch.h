/*!
* \class TwistPatch
 * \author Gene Van Buren, BNL, 24-May-2005
 *
 *               Code to patch incorrect data from using the wrong
 *               Twist distortion correction.
 *               Default is to patch P05ia production.
 *
 */

#ifndef TwistPatch_h
#define TwistPatch_h

#include "TArrayD.h"
#include "StThreeVectorF.hh"

class TwistPatch {
 public:
    TwistPatch();
    virtual ~TwistPatch();

    /// Enter magnetic field manually (if you leave it blank in patch functions)
    virtual void SetMagF(double mag);

    /// @name Functions to patch data
    //@{
    virtual void PrimaryVertex(StThreeVectorF& vtx, double mag=-999.);
    /// Patch primary vertex positions (use either function)
    virtual void PrimaryVertex(double& vx, double& vy, double& vz, double mag=-999.);
    /// Patch primary tracks
    virtual void PrimaryTrack(double& pt, double& phi, double& eta,
		      int ch, double vz, double mag=-999.);
    //@}

    /// Turn on corrections for P05ia (on by default)
    void CorrectP05ia();

 protected:
    /// @name Functions to set parameters
    //@{
    void SetVtxPars();
    void SetEtaPars();
    void SetPtPars(int ew, int ch);
    void SetPhiPars(int ew, int ch);
    //@}

    
    int magf;
  
    Double_t* vtxpars; //!
    Double_t* etapars; //!
    Double_t* ptpars;  //!
    Double_t* phipars; //!
  
    TArrayD vtxparsFull;
    TArrayD ptparsFull;
    TArrayD phiparsFull;
    TArrayD etaparsFull;

    ClassDef(TwistPatch,0)
};

/***********************************************************************
 * $Id: TwistPatch.h,v 1.2 2005/05/26 17:25:55 genevb Exp $
 * $Log: TwistPatch.h,v $
 * Revision 1.2  2005/05/26 17:25:55  genevb
 * Fixed typo with ptpars,phipars
 *
 * Revision 1.1  2005/05/24 19:05:02  genevb
 * Introduce TwistPatch
 *
 *
 ***********************************************************************/

#endif
