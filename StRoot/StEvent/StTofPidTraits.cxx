/***************************************************************************
 *
 * $Id: StTofPidTraits.cxx,v 2.3 2004/07/08 16:56:35 ullrich Exp $
 *
 * Author: Thomas Ullrich, Dec 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofPidTraits.cxx,v $
 * Revision 2.3  2004/07/08 16:56:35  ullrich
 * New class member introduced.
 *
 * Revision 2.2  2000/12/08 20:21:08  genevb
 * Changed kTofPatchId -> kTofId
 *
 * Revision 2.1  2000/12/08 03:52:42  ullrich
 * Initial Revision
 *
 ***************************************************************************/
#include "StTofPidTraits.h"
#include "StMessMgr.h"

static const char rcsid[] = "$Id: StTofPidTraits.cxx,v 2.3 2004/07/08 16:56:35 ullrich Exp $";

ClassImp(StTofPidTraits)

StTofPidTraits::StTofPidTraits()
  : StTrackPidTraits(kTofId), mTray(0), mModule(0), mCell(0), mTof(0.), mPathLength(0.), mBeta(0.) {
      mSigmaElectron = 999.; 
      mSigmaPion = 999.; 
      mSigmaKaon = 999.; 
      mSigmaProton = 999.;  
}

StTofPidTraits::StTofPidTraits(int itray, int imodule, int icell, float tof, float path, float beta)
  : StTrackPidTraits(kTofId), mTray(itray), mModule(imodule), mCell(icell),
    mTof(tof), mPathLength(path), mBeta(beta) {
      mSigmaElectron = 999.; 
      mSigmaPion = 999.; 
      mSigmaKaon = 999.; 
      mSigmaProton = 999.;  
}

StTofPidTraits::~StTofPidTraits() { /* noop */ }

void StTofPidTraits::Print(Option_t *opt) const {
    gMessMgr->Info() << "StTofPidTraits :: \t tray: " << tray()
		     << "\t module: " << module()
		     << "\t cell: " << cell() 
		     << "\t tof: " << tof()         
		     << "\t pathLength: " << pathLength()           
		     << "\t beta: " << beta()  << endm;
}
