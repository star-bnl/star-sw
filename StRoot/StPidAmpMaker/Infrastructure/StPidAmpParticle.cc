/***************************************************************************
 *
 * $Id: StPidAmpParticle.cc,v 1.1.1.1 2000/03/09 17:48:34 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             particle type definition
 ***************************************************************************
 *
 * $Log: StPidAmpParticle.cc,v $
 * Revision 1.1.1.1  2000/03/09 17:48:34  aihong
 * Installation of package
 *
 **************************************************************************/


#include "StPidAmpMaker/Infrastructure/StPidAmpParticle.hh"

//-----------------------------
StPidAmpParticle::StPidAmpParticle(
                     const string   name,
                     int             id, //geant ID
		     double          charge, //in units of e
		     double          mass,
                     double          peakPos,//peak of pop. on rig axis
                     double          maxllWidth,//maxllBltz fitting width.
                     double          maxllRatio,//peakHeight / # total tracks.
		     double          rigStart,
                     double          rigEnd,
		     StPidAmpWindow  wd){


       mName=name;
       mID=id;
       mCharge=charge;
       mMass=mass;
       mMaxllPeakPos=peakPos;
       mMaxllWidth=maxllWidth;
       mMaxllRatio=maxllRatio;
       mStart=rigStart;
       mEnd=rigEnd;

       mWindow=wd;
}
//-----------------------------
StPidAmpParticle::StPidAmpParticle(){
  /* no op */

}
      
//-----------------------------
StPidAmpParticle::~StPidAmpParticle(){
  /* no op */
 
}


//-----------------------------
void StPidAmpParticle::setWindow(StPidAmpWindow& w){

      mWindow=w;
}


//-------------------------------------------------name        id z  mass      peakPos mWidth mRatio          start   end  window
const StPidAmpParticle StPidAmpParticle::mPositron("positron", 2, 1, 0.511e-3, 0.03,   30,     0.00312906*0.6, 1.0e-3, 5.0, StPidAmpWindow(1.0e-3,0.06,0.30,0.35));
//this package fill e+/- nets without dca cut.ref to StPidAmpNet::fillSlices()
//for other nets, use dca cut to filter out e+/-.
//BUT, in order to have a nice fitting of dedx-betaGamma tail,
//we use a dca cut to filter out particles other than e+/- when
//filling StPidAmpNet::mBGElectronNet. (ref. to StPidAmpNet::push2BGNet())
//width was 190

//-------------------------------------------------name  id z  mass      peakPos mWidth mRatio          start   end  window
const StPidAmpParticle StPidAmpParticle::mElectron("e-", 3,-1, 0.511e-3, 0.03,   30,     0.00312906*0.6, -1.0e-3, -5.0, StPidAmpWindow(-1.0e-3,-0.06,-0.30,-0.35));
//width was 190
//-------------------------------------------------name     id z  mass      peakPos mWidth mRatio             start   end  window
const StPidAmpParticle StPidAmpParticle::mPiPlus("pi+",     8, 1, 0.13957,  0.34,   3.85,   0.7492819e-03*1.55, 0.07,    5.0, StPidAmpWindow(1.0e-3,0.6));//0.32


//-------------------------------------------------name    id z  mass      peakPos mWidth mRatio              start   end  window
const StPidAmpParticle StPidAmpParticle::mPiMinus("pi-" ,  9,-1, 0.13957,  0.34,   3.85,   0.7492819e-03*1.55, -0.07,   -5.0, StPidAmpWindow(-1.0e-3,-0.6));//0.32 //start was 0.1


//-------------------------------------------------name     id z  mass      peakPos mWidth mRatio           start   end  window
const StPidAmpParticle StPidAmpParticle::mKaonPlus("kaon+", 11,1, 0.49368,  0.75,   3.4,   6.105260e-05*0.7*0.65,0.15,  5.0, StPidAmpWindow(0.15,0.460)); //0.47

//-------------------------------------------------name     id z  mass      peakPos mWidth mRatio               start   end  window
const StPidAmpParticle StPidAmpParticle::mKaonMinus("kaon-",12,-1,0.49368, 0.75,    3.4,   6.105260e-05*0.7*0.65,-0.15,  -5.0, StPidAmpWindow(-0.15,-0.460));//0.47

//-------------------------------------------------name        id z  mass      peakPos mWidth   mRatio                start   end  window
const StPidAmpParticle StPidAmpParticle::mProton("proton",     14,1, 0.93827,  1.15,   4.0,   3.552151e-05*0.7,     0.3,   5.0, StPidAmpWindow(0.3,0.9));//0.92

//-------------------------------------------------name                id z  mass      peakPos mWidth    mRatio              start   end  window
const StPidAmpParticle StPidAmpParticle::mAntiProton("anti_proton",     15,-1,0.93827,  1.15,   4.0,  3.552151e-05*0.7,    -0.3,  -5.0, StPidAmpWindow(-0.3,-0.9));//0.92 //width was 2.5

//-------------------------------------------------name        id z  mass      peakPos mWidth mRatio     start   end  window
const StPidAmpParticle StPidAmpParticle::mDeuteron("deuteron", 45,1, 1.8756,   2.0,   3.0,   2.775118e-06,1.0,   5.0, StPidAmpWindow(1.0,1.5));
//   the maxll* parameters for deuteron here is a pure "guess", 
//   maxll* parameters for other types are based on experience.with simu. data.

//-------------------------------------------------name                  id z  mass      peakPos mWidth mRatio     start   end  window
const StPidAmpParticle StPidAmpParticle::mBGParticle("betaGamma",     -999, 1.0,1.0,     0.0,    0.0,   0.0,        1.0e-3,760, StPidAmpWindow(0.0,4.5,17.0,33.0,35.0,146.0,578.0,680.0));
//this is a fake particle for StPidAmpNet::mBGNet construction. 
//for dedx~betaGamma fitting.

//-------------------------------------------------       name                       id z  mass      peakPos mWidth mRatio     start   end  window
const StPidAmpParticle StPidAmpParticle::mBGElectronParticle("mBGe-Particle",     -3, 1.0,1.0,     0.0,    0.0,   0.0,        20.0,    760, StPidAmpWindow(35.0,146.0,578.0,680.0));

//-------------------------------------------------       name                       id z  mass      peakPos mWidth mRatio     start   end  window
const StPidAmpParticle StPidAmpParticle::mBGPositronParticle("mBGe+Particle",     -2, 1.0,1.0,     0.0,    0.0,   0.0,        20.0,    760, StPidAmpWindow(35.0,146.0,578.0,680.0));//correspond to electron 0.21,0.376


//-------------------------------------------------          name                       id z  mass      peakPos mWidth mRatio     start   end  window
const StPidAmpParticle StPidAmpParticle::mBGPionPlusParticle("mBGPionPlusParticle",     -8, 1.0,1.0,     0.0,    0.0,   0.0,        1.0e-3,35.0, StPidAmpWindow(0.0,4.5,17.0,33.0));

//-------------------------------------------------          name                          id z  mass      peakPos mWidth mRatio     start   end  window
const StPidAmpParticle StPidAmpParticle::mBGPionMinusParticle("mBGPionMinusParticle",     -9, 1.0,1.0,     0.0,    0.0,   0.0,        1.0e-3,35.0, StPidAmpWindow(0.0,4.5,17.0,33.0));


//------------------------------------------------           -name                       id z  mass      peakPos mWidth mRatio     start   end  window
const StPidAmpParticle StPidAmpParticle::mBGKaonPlusParticle("mBGKaonPlusParticle",     -11, 1.0,1.0,     0.0,    0.0,   0.0,        1.0e-3,   35.0, StPidAmpWindow(0.3,0.93));


//------------------------------------------------           -name                         id z  mass      peakPos mWidth mRatio     start   end  window
const StPidAmpParticle StPidAmpParticle::mBGKaonMinusParticle("mBGKaonMinusParticle",     -12, 1.0,1.0,     0.0,    0.0,   0.0,       1.0e-3,  35.0, StPidAmpWindow(0.3,0.93));



//-------------------------------------------------      name                       id z  mass      peakPos mWidth mRatio     start   end  window
const StPidAmpParticle StPidAmpParticle::mBGProtonParticle("mBGPrtonParticle",     -14, 1.0,1.0,     0.0,    0.0,   0.0,        1.0e-3, 35.0, StPidAmpWindow(0.32,0.96));


//-------------------------------------------------              name                       id z  mass      peakPos mWidth mRatio     start   end  window
const StPidAmpParticle StPidAmpParticle::mBGAntiProtonParticle("mBGAntiPrtonParticle",     -15, 1.0,1.0,     0.0,    0.0,   0.0,       1.0e-3, 35.0, StPidAmpWindow(0.32,0.96));



