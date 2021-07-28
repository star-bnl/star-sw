/***************************************************************************
 *
 * $Id: StHbtParticle.cc,v 1.23 2015/11/02 20:12:58 perev Exp $
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
 * Revision 1.23  2015/11/02 20:12:58  perev
 * isnan for new compiler
 *
 * Revision 1.22  2003/09/02 17:58:32  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.21  2003/05/07 15:30:43  magestro
 * Fixed bug related to finding merged hits (commit for Fabrice)
 *
 * Revision 1.20  2003/01/14 09:41:26  renault
 * changes on average separation calculation, hit shared finder and memory optimisation
 * for Z,U and Sectors variables.
 *
 * Revision 1.19  2002/12/12 17:01:49  kisiel
 * Hidden Information handling and purity calculation
 *
 * Revision 1.18  2002/11/19 23:36:00  renault
 * Enable calculation of exit/entrance separation for V0 daughters
 *
 * Revision 1.17  2001/12/14 23:11:30  fretiere
 * Add class HitMergingCut. Add class fabricesPairCut = HitMerginCut + pair purity cuts. Add TpcLocalTransform function which convert to local tpc coord (not pretty). Modify StHbtTrack, StHbtParticle, StHbtHiddenInfo, StHbtPair to handle the hit information and cope with my code
 *
 * Revision 1.16  2001/05/25 23:23:59  lisa
 * Added in StHbtKink stuff
 *
 * Revision 1.15  2001/04/03 21:04:36  kisiel
 *
 *
 *   Changes needed to make the Theoretical code
 *   work. The main code is the ThCorrFctn directory.
 *   The most visible change is the addition of the
 *   HiddenInfo to StHbtPair.
 *
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

double StHbtParticle::mPrimPimPar0= 9.05632e-01;
double StHbtParticle::mPrimPimPar1= -2.26737e-01;
double StHbtParticle::mPrimPimPar2= -1.03922e-01;
double StHbtParticle::mPrimPipPar0= 9.09616e-01;
double StHbtParticle::mPrimPipPar1= -9.00511e-02;
double StHbtParticle::mPrimPipPar2= -6.02940e-02;
double StHbtParticle::mPrimPmPar0= 0.;
double StHbtParticle::mPrimPmPar1= 0.;
double StHbtParticle::mPrimPmPar2= 0.;
double StHbtParticle::mPrimPpPar0= 0.;
double StHbtParticle::mPrimPpPar1= 0.;
double StHbtParticle::mPrimPpPar2= 0.;

int TpcLocalTransform(StThreeVectorD& xgl, 
		      int& iSector, 
		      int& iPadrow, 
		      float& xlocal,
		      double& ttPhi);


//_____________________
StHbtParticle::StHbtParticle() : mTrack(0), mV0(0), mKink(0), mHiddenInfo(0) {
  /* no-op for default */
}
//_____________________
StHbtParticle::~StHbtParticle(){
  //  cout << "Issuing delete for StHbtParticle." << endl;

  if (mTrack) delete mTrack;
  if (mV0) {
    delete[] mTpcV0NegPosSample;
    delete[] mV0NegZ;
    delete[] mV0NegU;
    delete[] mV0NegSect;
    delete mV0;
  }
  if (mKink) delete mKink;
  //  cout << "Trying to delete HiddenInfo: " << mHiddenInfo << endl;
  if (mHiddenInfo) 
    {
      //      cout << "Deleting HiddenInfo." << endl;
      delete mHiddenInfo;
    }
}
//_____________________
StHbtParticle::StHbtParticle(const StHbtTrack* const hbtTrack,const double& mass) : mTrack(0), mV0(0), mKink(0), mHiddenInfo(0) {
  
  
  // I know there is a better way to do this...
  mTrack = new StHbtTrack(*hbtTrack);
  StHbtThreeVector temp = hbtTrack->P();
  mFourMomentum.setVect(temp);
  double ener = ::sqrt(temp.mag2()+mass*mass);
  mFourMomentum.setE(ener);
  mMap[0] = hbtTrack->TopologyMap(0);
  mMap[1] = hbtTrack->TopologyMap(1);
  mNhits = hbtTrack->NHits();
  mHelix = hbtTrack->Helix();
  //CalculateNominalTpcExitAndEntrancePoints();

 
  mPrimaryVertex.setX(0.);
  mPrimaryVertex.setY(0.);
  mPrimaryVertex.setZ(0.);
  mSecondaryVertex.setX(0.);
  mSecondaryVertex.setY(0.);
  mSecondaryVertex.setZ(0.);

  CalculateTpcExitAndEntrancePoints(&mHelix,&mPrimaryVertex,
				    &mSecondaryVertex,
				    &mNominalTpcEntrancePoint,
				    &mNominalTpcExitPoint,
				    &mNominalPosSample[0],
				    &mZ[0],
				    &mU[0],
				    &mSect[0]);

  CalculatePurity();
  // ***
  mHiddenInfo= 0;
  if(hbtTrack->ValidHiddenInfo()){
    mHiddenInfo= hbtTrack->getHiddenInfo()->getParticleHiddenInfo();
  }
  // ***

}
//_____________________
StHbtParticle::StHbtParticle(const StHbtV0* const hbtV0,const double& mass) : mTrack(0), mV0(0), mKink(0), mHiddenInfo(0) {
  mV0 = new StHbtV0(*hbtV0);
  mMap[0]= 0;
  mMap[1]= 0;
  // I know there is a better way to do this...
  StHbtThreeVector temp = hbtV0->momV0();
  mFourMomentum.setVect(temp);
  double ener = ::sqrt(temp.mag2()+mass*mass);
  mFourMomentum.setE(ener);
  // Calculating TpcEntrancePoint for Positive V0 daugther
  mPrimaryVertex = hbtV0->primaryVertex();
  mSecondaryVertex = hbtV0->decayVertexV0();
  mHelixV0Pos = hbtV0->HelixPos();

  mTpcV0NegPosSample = new StHbtThreeVector[45];//for V0Neg
  mV0NegZ = new float[45];//for V0Neg
  mV0NegU = new float[45];//for V0Neg
  mV0NegSect = new int[45];//for V0Neg

  CalculateTpcExitAndEntrancePoints(&mHelixV0Pos,&mPrimaryVertex,
				    &mSecondaryVertex,
				    &mTpcV0PosEntrancePoint,
				    &mTpcV0PosExitPoint,
				    &mNominalPosSample[0],
				    &mZ[0],
				    &mU[0],&mSect[0]);

  mHelixV0Neg = hbtV0->HelixNeg();

  CalculateTpcExitAndEntrancePoints(&mHelixV0Neg,
				    &mPrimaryVertex,
				    &mSecondaryVertex,
				    &mTpcV0NegEntrancePoint,
				    &mTpcV0NegExitPoint,
				    &mTpcV0NegPosSample[0],
				    &mV0NegZ[0],
				    &mV0NegU[0],&mV0NegSect[0]);

  // ***
  mHiddenInfo= 0;
  if(hbtV0->ValidHiddenInfo()){
    mHiddenInfo= hbtV0->getHiddenInfo()->clone();
  }
  // ***
}
//_____________________
StHbtParticle::StHbtParticle(const StHbtKink* const hbtKink,const double& mass) : mTrack(0), mV0(0), mHiddenInfo(0) {
  mKink = new StHbtKink(*hbtKink);
  mMap[0]= 0;
  mMap[1]= 0;
  // I know there is a better way to do this...
  StHbtThreeVector temp = hbtKink->Parent().P();
  mFourMomentum.setVect(temp);
  double ener = ::sqrt(temp.mag2()+mass*mass);
  mFourMomentum.setE(ener);
}

//_____________________
StHbtParticle::StHbtParticle(const StHbtXi* const hbtXi, const double& mass)  {
  mXi = new StHbtXi(*hbtXi);
  mMap[0]= 0;
  mMap[1]= 0;
  StHbtThreeVector temp;// = hbtXi->mMomXi;
  mFourMomentum.setVect(temp);
  double ener = ::sqrt(temp.mag2()+mass*mass);
  mFourMomentum.setE(ener);
  mHiddenInfo = 0;
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
//***************WARNING*************************
// Gael includes this in a function with parameters to use it for 
// tracks and V0 daughters GR 5 dec 02
//***************WARNING*************************
// void StHbtParticle::CalculateNominalTpcExitAndEntrancePoints(){
//   // this calculates the "nominal" exit point of a track, either through the endcap or through the Outer Field Cage
//   // "nominal" means the track is assumed to start at (0,0,0)
//   // it also calculates the "nominal" entrance point of the track, which is the point at which it crosses the
//   // inner field cage
//   static StHbtThreeVector ZeroVec(0.,0.,0.);
//   double dip, curv, phase;
//   int h;
//   curv = mHelix.curvature();
//   dip  = mHelix.dipAngle();
//   phase= mHelix.phase();
//   h    = mHelix.h();
//   StHelixD hel(curv,dip,phase,ZeroVec,h);

//   pairD candidates;
//   double sideLength;  // this is how much length to go to leave through sides of TPC
//   double endLength;  // this is how much length to go to leave through endcap of TPC
//   // figure out how far to go to leave through side...
//   candidates = hel.pathLength(200.0);  // bugfix MAL jul00 - 200cm NOT 2cm
//   sideLength = (candidates.first > 0) ? candidates.first : candidates.second;

//   static StHbtThreeVector WestEnd(0.,0.,200.);  // bugfix MAL jul00 - 200cm NOT 2cm
//   static StHbtThreeVector EastEnd(0.,0.,-200.); // bugfix MAL jul00 - 200cm NOT 2cm
//   static StHbtThreeVector EndCapNormal(0.,0.,1.0);

//   endLength = hel.pathLength(WestEnd,EndCapNormal);
//   if (endLength < 0.0) endLength = hel.pathLength(EastEnd,EndCapNormal);

//   if (endLength < 0.0) cout << "StHbtParticle::CalculateNominalTpcExitAndEntrancePoints(): "
//                             << "Hey-- I cannot find an exit point out endcaps" << endl;

//   // OK, firstExitLength will be the shortest way out of the detector...
//   double firstExitLength = (endLength < sideLength) ? endLength : sideLength;

//   // now then, let's return the POSITION at which particle leaves TPC...
//   mNominalTpcExitPoint = hel.at(firstExitLength);


//   // Finally, calculate the position at which the track crosses the inner field cage
//   candidates = hel.pathLength(50.0);  // bugfix MAL jul00 - 200cm NOT 2cm

//   sideLength = (candidates.first > 0) ? candidates.first : candidates.second;
// //   if (sideLength < 0.0)
// //     {
// //       cout 
// //      << "no crossing with IFC" 
// //      << " curve=" << curv 
// //      << " candidates=" << candidates.first << " " << candidates.second 
// //       << "origin=" << mHelix.origin() << " "<< dip << " " << phase << " " << h << endl;
// //     }
// //   else
// //     {
// //       cout 
// //      << "does cross       IFC" 
// //      << " curve=" << curv 
// //      << " candidates=" << candidates.first << " " << candidates.second 
// //      << "origin=" << mHelix.origin() << " "<< dip << " " << phase << " " << h << endl;
// //     }


// //   if (sideLength < 0.0)
// //     {
// //       if (phase > C_PI)
// //      {
// //        cout << "righto" << endl;
// //      }
// //       else
// //      {
// //        cout << "WRONGO!! 1 " << phase << endl;
// //      }
// //     }
// //   else
// //     {
// //       if (phase > C_PI)
// //      {
// //        cout << "WRONGO!! 2 " << phase << endl;
// //      }
// //       else
// //      {
// //        cout << "righto " << endl;
// //      }
// //    }

//   mNominalTpcEntrancePoint = hel.at(sideLength);


//   // This is the secure way !  
// //   if (::isnan(mNominalTpcEntrancePoint.x()) || 
// //       ::isnan(mNominalTpcEntrancePoint.x()) || 
// //       ::isnan(mNominalTpcEntrancePoint.x()) ) mNominalTpcEntrancePoint = StHbtThreeVector(-9999.,-9999.,-9999); 
// //   if (::isnan(mNominalTpcExitPoint.x()) || 
// //       ::isnan(mNominalTpcExitPoint.x()) || 
// //       ::isnan(mNominalTpcExitPoint.x()) ) mNominalTpcExitPoint = StHbtThreeVector(-9999.,-9999.,-9999); 

//   // This is faster  
//   if (::isnan(mNominalTpcExitPoint.x())) mNominalTpcExitPoint = StHbtThreeVector(-9999.,-9999.,-9999); 


//   // 03Oct00 - mal.  OK, let's try something a little more along the lines of NA49 and E895 strategy.
//   //    calculate the "nominal" position at N radii (say N=11) within the TPC, and for a pair cut
//   //    use the average separation of these N
//   for (int irad=0; irad<11; irad++){
//     float radius = 50.0 + irad*15.0;
//     candidates = hel.pathLength(radius);
//     sideLength = (candidates.first > 0) ? candidates.first : candidates.second;
//     mNominalPosSample[irad] = hel.at(sideLength);
//   }



//   static double tSectToPhi[24]={2.,1.,0.,11.,10.,9.,8. ,7. ,6.,5.,4.,3.,
// 				4.,5.,6., 7., 8.,9.,10.,11.,0.,1.,2.,3.};
//   static float tRowRadius[45] = {60,64.8,69.6,74.4,79.2,84,88.8,93.6,98.8, 
// 				 104,109.2,114.4,119.6,127.195,129.195,131.195,
// 				 133.195,135.195,137.195,139.195,141.195,
// 				 143.195,145.195,147.195,149.195,151.195,
// 				 153.195,155.195,157.195,159.195,161.195,
// 				 163.195,165.195,167.195,169.195,171.195,
// 				 173.195,175.195,177.195,179.195,181.195,
// 				 183.195,185.195,187.195,189.195};
//   int tRow,tSect,tOutOfBound;
//   double tU,tLength,tPhi;
//   StHbtThreeVector tPoint;
//   StThreeVectorD tn(0,0,0);
//   StThreeVectorD tr(0,0,0);
//   for(int ti=0;ti<45;ti++){
//     // Find which sector it is on
//     candidates =  hel.pathLength(tRowRadius[ti]);
//     tLength = (candidates.first > 0) ? candidates.first : candidates.second;
//     tPoint = hel.at(tLength);
//     TpcLocalTransform(tPoint,mSect[ti],tRow,tU,tPhi);
//     // calculate crossing plane
//     //tPhi = tSectToPhi[mSect[ti]-1]*TMath::Pi()/6.;
//     tn.setX(cos(tPhi));
//     tn.setY(sin(tPhi));       
//     tr.setX(tRowRadius[ti]*cos(tPhi));
//     tr.setY(tRowRadius[ti]*sin(tPhi));
//     // find crossing point
//     tLength = hel.pathLength(tr,tn);
//     tPoint = hel.at(tLength);
//     mZ[ti] = tPoint.z();
//     tOutOfBound = TpcLocalTransform(tPoint,tSect,tRow,mU[ti],tPhi);
//     if(tOutOfBound || (mSect[ti] == tSect && tRow!=(ti+1))){
//       //cout << "Out of bound " << tOutOfBound2 << " " << tOutOfBound << " " 
//       //   << tSect << " " << mSect[ti] << " "
//       //   << ti+1 << " " << tRow << " " << tRowRadius[ti] << " " 
//       //   << tU << " " << mU[ti] << endl;
//       mSect[ti]=-1;
//     }
//     else{
//       if(mSect[ti] != tSect){
// 	// Try again on the other sector
// 	tn.setX(cos(tPhi));
// 	tn.setY(sin(tPhi));       
// 	tr.setX(tRowRadius[ti]*cos(tPhi));
// 	tr.setY(tRowRadius[ti]*sin(tPhi));
// 	// find crossing point
// 	tLength = hel.pathLength(tr,tn);
// 	tPoint = hel.at(tLength);
// 	mZ[ti] = tPoint.z();
// 	mSect[ti] = tSect;
// 	tOutOfBound = TpcLocalTransform(tPoint,tSect,tRow,mU[ti],tPhi);
// 	if(tOutOfBound || tSect!= mSect[ti] || tRow!=(ti+1)){
// 	  mSect[ti]=-1;
// 	  //cout << "Twice bad : OutOfBound =  " << tOutOfBound 
// 	  //   << " SectOk = " << (tSect!= mSect[ti])
// 	  //   << " RowOk = " <<  (tRow!=(ti+1)) << endl;
// 	}
//       }
//     }
//   }
// }
//_____________________
void StHbtParticle::CalculatePurity(){
  double tPt = mFourMomentum.perp();
  // pi -
  mPurity[0] = mPrimPimPar0*(1.-exp((tPt-mPrimPimPar1)/mPrimPimPar2));
  mPurity[0] *= mTrack->PidProbPion();
  // pi+
  mPurity[1] = mPrimPipPar0*(1.-exp((tPt-mPrimPipPar1)/mPrimPipPar2));
  mPurity[1] *= mTrack->PidProbPion();
  // K-
  mPurity[2] = mTrack->PidProbKaon();
  // K+
  mPurity[3] = mTrack->PidProbKaon();
  // pbar
  mPurity[4] = mTrack->PidProbProton();
  // p
  mPurity[5] = mTrack->PidProbProton();
}

double StHbtParticle::GetPionPurity()
{
  if (mTrack->Charge()>0)
    return mPurity[1];
  else
    return mPurity[0];
}
double StHbtParticle::GetKaonPurity()
{
  if (mTrack->Charge()>0)
    return mPurity[3];
  else
    return mPurity[2];
}
double StHbtParticle::GetProtonPurity()
{
  if (mTrack->Charge()>0)
    return mPurity[5];
  else
    return mPurity[4];
}

void StHbtParticle::CalculateTpcExitAndEntrancePoints(StPhysicalHelixD* tHelix,
						       StHbtThreeVector*  PrimVert,
						       StHbtThreeVector*  SecVert,
						       StHbtThreeVector* tmpTpcEntrancePoint,
						       StHbtThreeVector* tmpTpcExitPoint,
						       StHbtThreeVector* tmpPosSample,
						       float* tmpZ,
						       float* tmpU,
						       int* tmpSect){
  // this calculates the exit point of a secondary track, 
  // either through the endcap or through the Outer Field Cage
  // We assume the track to start at tHelix.origin-PrimaryVertex
  // it also calculates the entrance point of the secondary track, 
  // which is the point at which it crosses the
  // inner field cage
  //  static StHbtThreeVector ZeroVec(0.,0.,0.);
  StHbtThreeVector ZeroVec(0.,0.,0.);
//   ZeroVec.setX(tHelix->origin().x()-PrimVert->x());
//   ZeroVec.setY(tHelix->origin().y()-PrimVert->y());
//   ZeroVec.setZ(tHelix->origin().z()-PrimVert->z());
  ZeroVec.setX(SecVert->x()-PrimVert->x());
  ZeroVec.setY(SecVert->y()-PrimVert->y());
  ZeroVec.setZ(SecVert->z()-PrimVert->z());
  double dip, curv, phase;
  int h;
  curv = tHelix->curvature();
  dip  = tHelix->dipAngle();
  phase= tHelix->phase();
  h    = tHelix->h();
  
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

  if (endLength < 0.0) cout << 
			 "StHbtParticle::CalculateTpcExitAndEntrancePoints(): "
                            << "Hey -- I cannot find an exit point out endcaps" << endl;
  // OK, firstExitLength will be the shortest way out of the detector...
  double firstExitLength = (endLength < sideLength) ? endLength : sideLength;
  // now then, let's return the POSITION at which particle leaves TPC...
  *tmpTpcExitPoint = hel.at(firstExitLength);
  // Finally, calculate the position at which the track crosses the inner field cage
  candidates = hel.pathLength(50.0);  // bugfix MAL jul00 - 200cm NOT 2cm

  sideLength = (candidates.first > 0) ? candidates.first : candidates.second;
  //  cout << "sideLength 2 ="<<sideLength << endl;
  *tmpTpcEntrancePoint = hel.at(sideLength);
  // This is the secure way !  
  if (::isnan(tmpTpcEntrancePoint->x()) || 
      ::isnan(tmpTpcEntrancePoint->y()) || 
      ::isnan(tmpTpcEntrancePoint->z()) ){ 
    cout << "tmpTpcEntrancePoint NAN"<< endl; 
    cout << "tmpNominalTpcEntrancePoint = " <<tmpTpcEntrancePoint<< endl;
    tmpTpcEntrancePoint->setX(-9999.);
    tmpTpcEntrancePoint->setY(-9999.);
    tmpTpcEntrancePoint->setZ(-9999.);
  } 
    
  if (::isnan(tmpTpcExitPoint->x()) || 
      ::isnan(tmpTpcExitPoint->y()) || 
      ::isnan(tmpTpcExitPoint->z()) ) {
//     cout << "tmpTpcExitPoint NAN set at (-9999,-9999,-9999)"<< endl; 
//     cout << "tmpTpcExitPoint X= " <<tmpTpcExitPoint->x()<< endl;
//     cout << "tmpTpcExitPoint Y= " <<tmpTpcExitPoint->y()<< endl;
//     cout << "tmpTpcExitPoint Z= " <<tmpTpcExitPoint->z()<< endl;
    tmpTpcExitPoint->setX(-9999.);
    tmpTpcExitPoint->setY(-9999.);
    tmpTpcExitPoint->setZ(-9999.);
  }


//   if (::isnan(tmpTpcExitPoint->x())) *tmpTpcExitPoint = StHbtThreeVector(-9999.,-9999.,-9999); 
//   if (::isnan(tmpTpcEntrancetPoint->x())) *tmpTpcEntrancePoint = StHbtThreeVector(-9999.,-9999.,-9999); 
  //  cout << "tmpTpcEntrancePoint"<<*tmpTpcEntrancePoint << endl;

  // 03Oct00 - mal.  OK, let's try something a little more 
  // along the lines of NA49 and E895 strategy.
  // calculate the "nominal" position at N radii (say N=11) 
  // within the TPC, and for a pair cut
  // use the average separation of these N
  int irad = 0;
  candidates = hel.pathLength(50.0);
  sideLength = (candidates.first > 0) ? candidates.first : candidates.second;
  while (irad<11 && !::isnan(sideLength)){
    float radius = 50.0 + irad*15.0;
    candidates = hel.pathLength(radius);
    sideLength = (candidates.first > 0) ? candidates.first : candidates.second;
    tmpPosSample[irad] = hel.at(sideLength);
    if(::isnan(tmpPosSample[irad].x()) ||
       ::isnan(tmpPosSample[irad].y()) ||
       ::isnan(tmpPosSample[irad].z()) 
       ){
      cout << "tmpPosSample for radius=" << radius << " NAN"<< endl; 
      cout << "tmpPosSample=(" <<tmpPosSample[irad]<<")"<< endl;
      tmpPosSample[irad] =  StHbtThreeVector(-9999.,-9999.,-9999);
    }
    irad++;
    if (irad<11){
      float radius = 50.0 + irad*15.0;
      candidates = hel.pathLength(radius);
      sideLength = (candidates.first > 0) ? candidates.first : candidates.second;
    }
   }
   for (int i = irad; i<11; i++)
     {
       tmpPosSample[i] =  StHbtThreeVector(-9999.,-9999.,-9999);   
     }

  static float tRowRadius[45] = {60,64.8,69.6,74.4,79.2,84,88.8,93.6,98.8, 
				 104,109.2,114.4,119.6,127.195,129.195,131.195,
				 133.195,135.195,137.195,139.195,141.195,
				 143.195,145.195,147.195,149.195,151.195,
				 153.195,155.195,157.195,159.195,161.195,
				 163.195,165.195,167.195,169.195,171.195,
				 173.195,175.195,177.195,179.195,181.195,
				 183.195,185.195,187.195,189.195};
  int tRow,tSect,tOutOfBound;
  double tLength,tPhi;
  float tU;
  StHbtThreeVector tPoint;
  StThreeVectorD tn(0,0,0);
  StThreeVectorD tr(0,0,0);
  int ti =0;
  // test to enter the loop
  candidates =  hel.pathLength(tRowRadius[ti]);
  tLength = (candidates.first > 0) ? candidates.first : candidates.second;
  if (::isnan(tLength)){
    cout <<"tLength Init tmp NAN" << endl;
    cout <<"padrow number= "<<ti << "not reached" << endl;
    cout << "*** DO NOT ENTER THE LOOP***" << endl;
    tmpSect[ti]=-1;//sector
  }
  // end test
  while(ti<45 && !::isnan(tLength)){
    candidates =  hel.pathLength(tRowRadius[ti]);
    tLength = (candidates.first > 0) ? candidates.first : candidates.second;
    if (::isnan(tLength)){
      cout <<"tLength loop 1st NAN" << endl;
      cout <<"padrow number=  " << ti << " not reached" << endl;
      cout << "*** THIS IS AN ERROR SHOULDN'T  LOOP ***" << endl;
      tmpSect[ti]=-1;//sector
    }
    tPoint = hel.at(tLength);
    // Find which sector it is on
    TpcLocalTransform(tPoint,tmpSect[ti],tRow,tU,tPhi);
    if (::isnan(tmpSect[ti])){
      cout <<"***ERROR tmpSect"<< endl; 
    }
    if (::isnan(tRow)){
      cout <<"***ERROR tRow"<< endl;
    }
    if (::isnan(tU)){
      cout <<"***ERROR tU"<< endl;
    }
    if (::isnan(tPhi)){
      cout <<"***ERROR tPhi"<< endl;
    }  
    // calculate crossing plane
    tn.setX(cos(tPhi));
    tn.setY(sin(tPhi));       
    tr.setX(tRowRadius[ti]*cos(tPhi));
    tr.setY(tRowRadius[ti]*sin(tPhi));
    // find crossing point
    tLength = hel.pathLength(tr,tn); 
    if (::isnan(tLength)){
      cout <<"tLength loop 2nd  NAN" << endl;
      cout <<"padrow number=  " << ti << " not reached" << endl;
      tmpSect[ti]=-2;//sector
    }
    tPoint = hel.at(tLength);
    tmpZ[ti] = tPoint.z();
    tOutOfBound = TpcLocalTransform(tPoint,tSect,tRow,tmpU[ti],tPhi);
    if (::isnan(tSect)){
      cout <<"***ERROR tSect 2"<< endl; 
    }
    if (::isnan(tRow)){
      cout <<"***ERROR tRow 2"<< endl;
    }
    if (::isnan(tmpU[ti])){
      cout <<"***ERROR tmpU[ti] 2"<< endl;
    }
    if (::isnan(tPhi)){
      cout <<"***ERROR tPhi 2 "<< endl;
    }  
    if(tOutOfBound || (tmpSect[ti] == tSect && tRow!=(ti+1))){
      tmpSect[ti]=-2;
      //	  cout << "missed once"<< endl;
    }
    else{
      if(tmpSect[ti] != tSect){
	// Try again on the other sector
	tn.setX(cos(tPhi));
	tn.setY(sin(tPhi));       
	tr.setX(tRowRadius[ti]*cos(tPhi));
	tr.setY(tRowRadius[ti]*sin(tPhi));
	// find crossing point
	tLength = hel.pathLength(tr,tn);
	tPoint = hel.at(tLength);
	if (::isnan(tLength)){
	  cout <<"tLength loop 3rd NAN" << endl;
	  cout <<"padrow number=  "<< ti << " not reached" << endl;
	  tmpSect[ti]=-1;//sector
	}
	tmpZ[ti] = tPoint.z();
	tmpSect[ti] = tSect;
	tOutOfBound = TpcLocalTransform(tPoint,tSect,tRow,tmpU[ti],tPhi);
	if (::isnan(tSect)){
	  cout <<"***ERROR tSect 3"<< endl; 
	}
	if (::isnan(tRow)){
	  cout <<"***ERROR tRow 3"<< endl;
	}
	if (::isnan(tmpU[ti])){
	  cout <<"***ERROR tmpU[ti] 3"<< endl;
	}
	if (::isnan(tPhi)){
	  cout <<"***ERROR tPhi 3 "<< endl;
	}  
	if(tOutOfBound || tSect!= tmpSect[ti] || tRow!=(ti+1)){
	  tmpSect[ti]=-1;
	}
      }
    }
    if (::isnan(tmpSect[ti])){
      cout << "*******************ERROR***************************" << endl;
      cout <<"StHbtParticle--Fctn tmpSect=" << tmpSect[ti] << endl;
      cout << "*******************ERROR***************************" << endl;
    }
    if (::isnan(tmpU[ti])){
      cout << "*******************ERROR***************************" << endl;
      cout <<"StHbtParticle--Fctn tmpU=" << tmpU[ti] << endl;
      cout << "*******************ERROR***************************" << endl;
    }
    if (::isnan(tmpZ[ti])){
      cout << "*******************ERROR***************************" << endl;
      cout <<"StHbtParticle--Fctn tmpZ=" << tmpZ[ti] << endl;
      cout << "*******************ERROR***************************" << endl;
    }
    // If padrow ti not reached all other beyond are not reached
    // in this case set sector to -1
    if (tmpSect[ti]==-1){
      for (int tj=ti; tj<45;tj++){
	tmpSect[tj] = -1;
	ti=45;
      }
    }
    ti++;
    if (ti<45){
      candidates =  hel.pathLength(tRowRadius[ti]);
      tLength = (candidates.first > 0) ? candidates.first : candidates.second;}
  }
}
//_____________________
const StHbtThreeVector& StHbtParticle::TpcV0PosExitPoint() const{
  return mTpcV0PosExitPoint;
}
//_____________________
const StHbtThreeVector& StHbtParticle::TpcV0PosEntrancePoint() const{
  return mTpcV0PosEntrancePoint;
}
//______________________
const StHbtThreeVector& StHbtParticle::TpcV0NegExitPoint() const{
  return mTpcV0NegExitPoint;
}
//_____________________
const StHbtThreeVector& StHbtParticle::TpcV0NegEntrancePoint() const{
  return mTpcV0NegEntrancePoint;
}
//______________________
