/***************************************************************************
 *
 * $Id: StHbtParticle.cc,v 1.14 2000/10/05 23:09:05 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   Particle objects are part of the PicoEvent, which is what is
 *   stored in the EventMixingBuffers
 *   A Track object gets converted to a Particle object if it
 *   passes the ParticleCut of an Analysis
 *
 ***************************************************************************
 *
 * $Log: StHbtParticle.cc,v $
 * Revision 1.14  2000/10/05 23:09:05  lisa
 * Added kT-dependent radii to mixed-event simulator AND implemented AverageSeparation Cut and CorrFctn
 *
 * Revision 1.13  2000/08/31 22:31:31  laue
 * StHbtAnalysis: output changed (a little bit less)
 * StHbtEvent: new version, members for reference mult added
 * StHbtIOBinary: new IO for new StHbtEvent version
 * StHbtTypes: TTree typedef to StHbtTTree added
 * StHbtVertexAnalysis: overflow and underflow added
 *
 * Revision 1.12  2000/07/23 13:52:56  laue
 * NominalExitPoint set to (-9999.,-9999.-9999.) if helix.at()
 * returns nan (not a number).
 *
 * Revision 1.11  2000/07/19 17:18:48  laue
 * Calculation of Entrance and Exit point added in StHbtParticle constructor
 *
 * Revision 1.10  2000/07/17 20:03:17  lisa
 * Implemented tools for addressing and assessing trackmerging
 *
 * Revision 1.9  2000/07/16 21:38:23  laue
 * StHbtCoulomb.cxx StHbtSectoredAnalysis.cxx : updated for standalone version
 * StHbtV0.cc StHbtV0.hh : some cast to prevent compiling warnings
 * StHbtParticle.cc StHbtParticle.hh : pointers mTrack,mV0 initialized to 0
 * StHbtIOBinary.cc : some printouts in #ifdef STHBTDEBUG
 * StHbtEvent.cc : B-Field set to 0.25Tesla, we have to think about a better
 *                 solution
 *
 * Revision 1.8  2000/05/03 17:44:43  laue
 * StHbtEvent, StHbtTrack & StHbtV0 declared friend to StHbtIOBinary
 * StHbtParticle updated for V0 pos,neg track Id
 *
 * Revision 1.7  2000/04/03 16:21:51  laue
 * some include files changed
 * Multi track cut added
 *
 * Revision 1.6  1999/12/11 15:58:29  lisa
 * Add vertex decay position datum and accessor to StHbtParticle to allow pairwise cuts on seperation of V0s
 *
 * Revision 1.5  1999/09/17 22:38:02  lisa
 * first full integration of V0s into StHbt framework
 *
 * Revision 1.4  1999/09/01 19:04:53  lisa
 * update Particle class AND add parity cf and Randys Coulomb correction
 *
 * Revision 1.3  1999/07/06 22:33:23  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.2  1999/06/29 17:50:27  fisyak
 * formal changes to account new StEvent, does not complie yet
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtParticle.hh"
#include "math_constants.h"
#ifdef __CC5__
  #include <math.h>
#else
  #include <cmath>
#endif
//_____________________
StHbtParticle::StHbtParticle() : mTrack(0), mV0(0) {
  /* no-op for default */
}
//_____________________
StHbtParticle::~StHbtParticle(){
  if (mTrack) delete mTrack;
  if (mV0) delete mV0;
}
//_____________________
StHbtParticle::StHbtParticle(const StHbtTrack* const hbtTrack,const double& mass) : mTrack(0), mV0(0) {
  // I know there is a better way to do this...
  mTrack = new StHbtTrack(*hbtTrack);
  StHbtThreeVector temp = hbtTrack->P();
  mFourMomentum.setVect(temp);
  double ener = sqrt(temp.mag2()+mass*mass);
  mFourMomentum.setE(ener);
  mMap[0] = hbtTrack->TopologyMap(0);
  mMap[1] = hbtTrack->TopologyMap(1);
  mNhits = hbtTrack->NHits();
  mHelix = hbtTrack->Helix();
  CalculateNominalTpcExitAndEntrancePoints();
}
//_____________________
StHbtParticle::StHbtParticle(const StHbtV0* const hbtV0,const double& mass) : mTrack(0), mV0(0) {
  mV0 = new StHbtV0(*hbtV0);
  mMap[0]= 0;
  mMap[1]= 0;
  // I know there is a better way to do this...
  StHbtThreeVector temp = hbtV0->momV0();
  mFourMomentum.setVect(temp);
  double ener = sqrt(temp.mag2()+mass*mass);
  mFourMomentum.setE(ener);
  //  cout << mPosTrackId << " " << mNegTrackId << " " << hbtV0->idPos() << " " << hbtV0->idNeg() << endl;
  //  mHelix = hbtTrack->Helix(); ?? what to do with mHelix for a Particle coming from a V0?
}
//_____________________
const StHbtThreeVector& StHbtParticle::NominalTpcExitPoint() const{
  // in future, may want to calculate this "on demand" only, sot this routine may get more sophisticated
  // for now, we calculate Exit and Entrance points upon instantiation
  return mNominalTpcExitPoint;
}
//_____________________
const StHbtThreeVector& StHbtParticle::NominalTpcEntrancePoint() const{
  // in future, may want to calculate this "on demand" only, sot this routine may get more sophisticated
  // for now, we calculate Exit and Entrance points upon instantiation
  return mNominalTpcEntrancePoint;
}
//_____________________
void StHbtParticle::CalculateNominalTpcExitAndEntrancePoints(){
  // this calculates the "nominal" exit point of a track, either through the endcap or through the Outer Field Cage
  // "nominal" means the track is assumed to start at (0,0,0)
  // it also calculates the "nominal" entrance point of the track, which is the point at which it crosses the
  // inner field cage
  static StHbtThreeVector ZeroVec(0.,0.,0.);
  double dip, curv, phase;
  int h;
  curv = mHelix.curvature();
  dip  = mHelix.dipAngle();
  phase= mHelix.phase();
  h    = mHelix.h();
  StHelixD hel(curv,dip,phase,ZeroVec,h);

  pairD candidates;
  double sideLength;  // this is how much length to go to leave through sides of TPC
  double endLength;  // this is how much length to go to leave through endcap of TPC
  // figure out how far to go to leave through side...
  candidates = hel.pathLength(200.0);  // bugfix MAL jul00 - 200cm NOT 2cm
  sideLength = (candidates.first > 0) ? candidates.first : candidates.second;

  static StHbtThreeVector WestEnd(0.,0.,200.);  // bugfix MAL jul00 - 200cm NOT 2cm
  static StHbtThreeVector EastEnd(0.,0.,-200.); // bugfix MAL jul00 - 200cm NOT 2cm
  static StHbtThreeVector EndCapNormal(0.,0.,1.0);

  endLength = hel.pathLength(WestEnd,EndCapNormal);
  if (endLength < 0.0) endLength = hel.pathLength(EastEnd,EndCapNormal);

  if (endLength < 0.0) cout << "StHbtParticle::CalculateNominalTpcExitAndEntrancePoints(): "
                            << "Hey-- I cannot find an exit point out endcaps" << endl;

  // OK, firstExitLength will be the shortest way out of the detector...
  double firstExitLength = (endLength < sideLength) ? endLength : sideLength;

  // now then, let's return the POSITION at which particle leaves TPC...
  mNominalTpcExitPoint = hel.at(firstExitLength);


  // Finally, calculate the position at which the track crosses the inner field cage
  candidates = hel.pathLength(50.0);  // bugfix MAL jul00 - 200cm NOT 2cm

  sideLength = (candidates.first > 0) ? candidates.first : candidates.second;
//   if (sideLength < 0.0)
//     {
//       cout 
//      << "no crossing with IFC" 
//      << " curve=" << curv 
//      << " candidates=" << candidates.first << " " << candidates.second 
//       << "origin=" << mHelix.origin() << " "<< dip << " " << phase << " " << h << endl;
//     }
//   else
//     {
//       cout 
//      << "does cross       IFC" 
//      << " curve=" << curv 
//      << " candidates=" << candidates.first << " " << candidates.second 
//      << "origin=" << mHelix.origin() << " "<< dip << " " << phase << " " << h << endl;
//     }


//   if (sideLength < 0.0)
//     {
//       if (phase > C_PI)
//      {
//        cout << "righto" << endl;
//      }
//       else
//      {
//        cout << "WRONGO!! 1 " << phase << endl;
//      }
//     }
//   else
//     {
//       if (phase > C_PI)
//      {
//        cout << "WRONGO!! 2 " << phase << endl;
//      }
//       else
//      {
//        cout << "righto " << endl;
//      }
//    }

  mNominalTpcEntrancePoint = hel.at(sideLength);


  // This is the secure way !  
//   if (isnan(mNominalTpcEntrancePoint.x()) || 
//       isnan(mNominalTpcEntrancePoint.x()) || 
//       isnan(mNominalTpcEntrancePoint.x()) ) mNominalTpcEntrancePoint = StHbtThreeVector(-9999.,-9999.,-9999); 
//   if (isnan(mNominalTpcExitPoint.x()) || 
//       isnan(mNominalTpcExitPoint.x()) || 
//       isnan(mNominalTpcExitPoint.x()) ) mNominalTpcExitPoint = StHbtThreeVector(-9999.,-9999.,-9999); 

  // This is faster  
  if (isnan(mNominalTpcExitPoint.x())) mNominalTpcExitPoint = StHbtThreeVector(-9999.,-9999.,-9999); 


  // 03Oct00 - mal.  OK, let's try something a little more along the lines of NA49 and E895 strategy.
  //    calculate the "nominal" position at N radii (say N=11) within the TPC, and for a pair cut
  //    use the average separation of these N
  for (int irad=0; irad<11; irad++){
    float radius = 50.0 + irad*15.0;
    candidates = hel.pathLength(radius);
    sideLength = (candidates.first > 0) ? candidates.first : candidates.second;
    mNominalPosSample[irad] = hel.at(sideLength);
  }


}
//_____________________
