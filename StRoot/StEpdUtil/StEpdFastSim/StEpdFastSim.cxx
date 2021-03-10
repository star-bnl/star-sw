#include "./StEpdFastSim.h"
#include "TClonesArray.h"
#include "TRandom3.h"
#include "../StEpdGeom.h"
#include "TVector3.h"
#include "TMath.h"
//#include "../../StPicoEvent/StPicoEpdHit.h"  <---- on your laptop, need explicit path.
#include "StRoot/StPicoEvent/StPicoEpdHit.h"
#include <iostream>

StEpdFastSim::StEpdFastSim(double WID){
  mRan = new TRandom3();
  mRan->SetSeed();

  mTheHits = new TClonesArray("StPicoEpdHit",1000);

  mGeom = new StEpdGeom();
  mWID = WID;


  // set look-up table for ring radii....
  double xcorner[5],ycorner[5];
  int ncorners;
  // first, INNERMOST of ring 1
  mGeom->GetCorners(1,1,0,&ncorners,xcorner,ycorner);

  double rmin(200.0);
  mGeom->GetCorners(1,1,0,&ncorners,xcorner,ycorner);
  for (int icorn=0; icorn<ncorners; icorn++){
    double rad = sqrt(pow(xcorner[icorn],2)+pow(ycorner[icorn],2));
    if (rad<rmin) rmin=rad;
  }
  RingRadii[0] = rmin;
  // from now on, I am finding the OUTERMOST radii of the rings
  for (int Ring=1; Ring<=16; Ring++){
    int TT = Ring*2 - 1;
    mGeom->GetCorners(1,TT,0,&ncorners,xcorner,ycorner);
    double rmax(0.0);
    for (int icorn=0; icorn<ncorners; icorn++){
      double rad = sqrt(pow(xcorner[icorn],2)+pow(ycorner[icorn],2));
      if (rad>rmax) rmax=rad;
    }
    RingRadii[Ring]=rmax;
  }

} 


StEpdFastSim::~StEpdFastSim(){
  /* no op */
}


/*  =================================================================================================== */
TClonesArray* StEpdFastSim::GetPicoHits(TClonesArray* momenta, TVector3 PrimVertex){
  double zEPD=375.0;  // cm
  if (fabs(PrimVertex.Z()>zEPD) || fabs(PrimVertex.X()>0.5) || fabs(PrimVertex.Y()>0.5)){
    std::cout << "Invalid vertex --- I'm giving you an empty event - Tchau!\n";
    return 0;
  }

  // important!
  for (int uniqueID=101; uniqueID<1232; uniqueID++){
    mHitsEast[uniqueID] = 0;
    mHitsWest[uniqueID] = 0;
  }

  mTheHits->Clear();

  //  TClonesArray* theHits = new TClonesArray("StPicoEpdHit",1000);

  for (int itrk=0; itrk<momenta->GetEntries(); itrk++){
    // project to EPD...
    TVector3* mom = (TVector3*)momenta->At(itrk);
    int EW = (mom->Z()<0.0)?0:1;
    double zWheel = (mom->Z()<0.0)?-zEPD:zEPD;   // east or west
    double deltaZ = abs(zWheel-PrimVertex.Z());      // how far to project  -- the abs is important!!

    double xHit = PrimVertex.X() + deltaZ*fabs(tan(mom->Theta()))*cos(mom->Phi());//Fixed bug: sin(theta)->fabs(tan(theta)) Xiaoyu Liu 03/09/2021
    double yHit = PrimVertex.Y() + deltaZ*fabs(tan(mom->Theta()))*sin(mom->Phi());//Xiaoyu Liu 03/09/2021 
    TVector3 xyHit(xHit,yHit,zWheel);

    short UniqueID = FindStruckTile(xyHit);   // returns the unique ID of the struck tile (see comments in StEpdHit), or zero if the EPD is not struck at all.

    if (UniqueID==0){
      //      cout << "Huh, this hit missed the EPD altogether!\n";
      continue;
    }

    float dE = mRan->Landau(1.0,mWID);  // energy loss to add to the hit.  This is added to nMIP

    StPicoEpdHit* theHit;
    if (EW<=0) {       // east
      if (mHitsEast[abs(UniqueID)]!=0){      // this tile has already been struck -- ADD to the nMIP value of this existing StPicoEpdHit
	theHit = mHitsEast[abs(UniqueID)];
        float nMIP = theHit->nMIP() + dE;
	theHit->setnMIP(nMIP);
      }
      else{                                  // this tile has not yet been struck-- make a StPicoEpdHit
	theHit = (StPicoEpdHit*)mTheHits->ConstructedAt(mTheHits->GetEntriesFast());
	theHit->setId(UniqueID);
	theHit->setnMIP(dE);
	theHit->setQTdata(pow(2,30));//w/o this step, theHit->nMIP() will return zero. Xiaoyu Liu 03/09/2021
	mHitsEast[abs(UniqueID)] = theHit;
      }
    }
    else{              // west
      if (mHitsWest[abs(UniqueID)]!=0){      // this tile has already been struck -- ADD to the nMIP value of this existing StPicoEpdHit
	theHit = mHitsWest[abs(UniqueID)];
        float nMIP = theHit->nMIP() + dE;
	theHit->setnMIP(nMIP);
      }
      else{                                  // this tile has not yet been struck-- make a StPicoEpdHit
	theHit = (StPicoEpdHit*)mTheHits->ConstructedAt(mTheHits->GetEntriesFast());
	theHit->setId(UniqueID);
	theHit->setnMIP(dE);
	theHit->setQTdata(pow(2,30));//w/o this step, theHit->nMIP() will return zero. Xiaoyu Liu 03/09/2021
	mHitsWest[abs(UniqueID)] = theHit;
      }
    }
  } // end loop over particles

  return mTheHits;
}

/* =====================================================================================================================
   Find the tile that is struck, and return its unique ID
   you know, you might think to set up a quick "boundaries of the tiles" and quickly find which tile was hit...
   but if you want to do it right, and you want to allow for arbitrary primary vertex, then I'm afraid it is
   a bit less trivial.  Before you try to get more "clever" than what I've done below, think about it...
*/
short StEpdFastSim::FindStruckTile(TVector3 xyHit){
  int PPmapWest[12] = {10,11,12,1,2,3,4,5,6,7,8,9};  // the index here is iphi (see below) and the value is PP number
  int PPmapEast[12] = {3,2,1,12,11,10,9,8,7,6,5,4};  // the index here is iphi (see below) and the value is PP number
  double PhiCenterWest[12] = {1.8326,2.35619,2.87979,3.40339,3.92699,4.45059,4.97419,5.49779,6.02139,0.261799,0.785398,1.309}; // index here is (PP-1) and value is phi of center of SS
  double PhiCenterEast[12] = {1.309,0.785398,0.261799,6.02139,5.49779,4.97419,4.45059,3.92699,3.40339,2.87979,2.35619,1.8326}; // index here is (PP-1) and value is phi of center of SS

  int EW = (xyHit.Z()<0)?0:1;

  // find ring based on radius
  double rHit = xyHit.Perp();
  if (rHit<RingRadii[0]) return 0;  //{std::cout << "Hit goes thru hole\n"; return 0;}          // hit is smaller radius than EPD inner radius (not necessarily a bug)
  if (rHit>RingRadii[16])return 0;  //{std::cout << "Hit too large radius\n"; return 0;}       // hit is bigger radius than EPD outer radius  (not necessarily a bug)
  int iRing;
  for (iRing=1; iRing<=16; iRing++){
    if (rHit<RingRadii[iRing]) break;   // got it
  }

  // now find PP based on phi
  double phiHit = xyHit.Phi();
  if (phiHit<0.0) phiHit += 2.0*TMath::Pi();
  int iphi = (int)(phiHit/(2.0*TMath::Pi()/12.0));
  int PP = (EW<=0)?PPmapEast[iphi]:PPmapWest[iphi];

  // now find TT based on ring and on phi
  int TT;
  if (iRing==1){
    TT=1;
  }
  else{
    if (EW<=0){    // on the East wheel, if phiHit is larger than the average phi of the supersector, then TT is the LARGER of the two possibilities
      if (phiHit>PhiCenterEast[PP-1]) {TT=2*(iRing-1) + 1;}
      else                            {TT=2*(iRing-1);}
    }
    else{          // on the West wheel, if phiHit is larger than the average phi of the supersector, then TT is the SMALLER of the two possibilities
      if (phiHit>PhiCenterWest[PP-1]) {TT=2*(iRing-1);}
      else                            {TT=2*(iRing-1) + 1;}
      
    }
  }

  // Finally, return the UniqueID of the tile
  int uniqueID = 100*PP+TT;
  if (EW<=0) uniqueID*=-1;

  //  std::cout << "EW,Ring,PP,TT = " << EW << " " << iRing << " " << PP << " " << TT << " rHIT,phi = " << rHit << " " << phiHit << "\n";

  /* and, you know, if you really wanted to be careful, and take account of the gaps between tiles, you would do a
   *  if (!(mGeom->IsInTile(UniqueID,xyHit.X(),yyHit.Y()))) return 0;
   *  I imagine we'll want to skip this, but it can be done, just like that. 
   */

  return uniqueID;
}



