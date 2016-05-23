/* $Id: StTpcFastSimMaker.cxx,v 1.5.2.1 2016/05/23 18:33:27 jeromel Exp $
    $Log: StTpcFastSimMaker.cxx,v $
    Revision 1.5.2.1  2016/05/23 18:33:27  jeromel
    Updates for SL12d / gcc44 embedding library - StDbLib, QtRoot update, new updated StJetMaker, StJetFinder, StSpinPool ... several cast fix to comply with c++0x and several cons related fixes (wrong parsing logic). Changes are similar to SL13b (not all ode were alike). Branch BSL12d_5_embed.

    Revision 1.5  2012/05/07 14:54:45  fisyak
    Add printout

    Revision 1.4  2011/01/04 21:40:22  fisyak
    Add pile-up

    Revision 1.3  2010/08/16 21:59:46  fisyak
    leave coordinates in TpcLocalCoordinate because StTpcHitMover expects that

    Revision 1.2  2010/05/28 16:28:44  fisyak
    Adjust for new TpcDb interface, remove pseudo pad rows

    Revision 1.1  2009/11/10 21:15:33  fisyak
    pams clean up
 
*/
#include "Stiostream.h"
#include "StTpcFastSimMaker.h"
#include "StHit.h"
#include "StEventTypes.h"
#include "tables/St_HitError_Table.h"
#include "StTpcHitCollection.h"
#include "StEvent.h"
#include "TMath.h"
#include "TRandom.h"
#include "StTpcDb/StTpcDb.h"
#include "tables/St_g2t_vertex_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_tpc_hit_Table.h"
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StMagUtilities.h"
#include "StTpcDb/StTpcDb.h"
#include "TDataSet.h"
#include "TDataSetIter.h"
#include "StDetectorDbMaker/StiTpcInnerHitErrorCalculator.h"
#include "StDetectorDbMaker/StiTpcOuterHitErrorCalculator.h"
ClassImp(StTpcFastSimMaker);
//____________________________________________________________
Int_t StTpcFastSimMaker::Make() {
  mExB = gStTpcDb->ExB();
  if (! gRandom) gRandom = new TRandom();
  // Get the input data structures from StEvent
  StEvent *rEvent =  (StEvent*) GetInputDS("StEvent");
  if (! rEvent) {    LOG_WARN << "No StEvent on input, bye bye" << endm; return kStWarn; }
  StTpcHitCollection *rCol = rEvent->tpcHitCollection();
  if (!rCol) {
    rCol = new StTpcHitCollection; 
    rEvent->setTpcHitCollection(rCol); 
  }
  St_g2t_tpc_hit *g2t_tpc_hit = (St_g2t_tpc_hit *) GetDataSet("geant/g2t_tpc_hit");
  if (! g2t_tpc_hit) {
    LOG_WARN << "No g2t_tpc_hit on input, bye bye" << endm; return kStWarn;
   }
  Int_t Nhits = g2t_tpc_hit->GetNRows();
  if (Nhits <= 0) return kStWarn;
  St_g2t_track *g2t_track = (St_g2t_track *) GetDataSet("geant/g2t_track"); //  if (!g2t_track)    return kStWarn;
  g2t_track_st *tpc_track = 0;
  if (g2t_track) tpc_track = g2t_track->GetTable();
  St_g2t_vertex  *g2t_ver = (St_g2t_vertex *) GetDataSet("geant/g2t_vertex");// if (!g2t_ver)      return kStWarn;
  g2t_vertex_st     *gver = 0;
  if (g2t_ver) gver = g2t_ver->GetTable();
  g2t_tpc_hit_st *tpc_hit         = g2t_tpc_hit->GetTable();
  StTpcCoordinateTransform transform(gStTpcDb);
  for (Int_t i = 0; i < Nhits; i++)    {
    if (tpc_hit[i].volume_id > 100000) continue; // skip pseudo pad rows
    Int_t Id         = tpc_hit[i].track_p;
    Int_t id3 = 0;
    if (tpc_track) 
      id3        = tpc_track[Id-1].start_vertex_p;
    Int_t sector = (tpc_hit[i].volume_id%10000)/100;
    Int_t row    =  tpc_hit[i].volume_id%100;
    StGlobalDirection     dirG(tpc_hit[i].p[0],tpc_hit[i].p[1],tpc_hit[i].p[2]);
    static StTpcLocalSectorDirection dirL;
    transform(dirG, dirL, sector, row);
    StGlobalCoordinate    coorG(tpc_hit[i].x[0],tpc_hit[i].x[1],tpc_hit[i].x[2]);
    static StTpcLocalCoordinate  coorLT;
    transform(coorG,coorLT,sector,row);
    StTpcLocalCoordinate  coorLTD = coorLT;
    // ExB corrections
    Float_t pos[3] = {(Float_t) coorLTD.position().x(),(Float_t) coorLTD.position().y(),(Float_t) coorLTD.position().z()};
    Float_t posMoved[3];
    if ( mExB ) {
      mExB->DoDistortion(pos,posMoved);   // input pos[], returns posMoved[]
      StThreeVector<double> newPos(posMoved[0],posMoved[1],posMoved[2]);
      coorLTD.setPosition(newPos);
    }
    static StTpcLocalSectorAlignedCoordinate  coorLSA;
    transform(coorLTD,coorLSA); //
    static StTpcLocalSectorCoordinate  coorLS;
    transform(coorLSA,coorLS); // alignment
    Double_t xyzL[3] = {coorLS.position().x(),coorLS.position().y(),coorLS.position().z()};
    if (Debug() && TMath::Abs(xyzL[1]-transform.yFromRow(row)) > 0.1000) {
      cout << "Id: " << tpc_hit[i].volume_id  
	   << "\txyzL :" << xyzL[0] << "\t" << xyzL[1] << "\t" << xyzL[2] 
	   << "\tdR :" << xyzL[1]-transform.yFromRow(row) << endl;
    }
    Double_t Z = xyzL[2];
    Double_t eta = TMath::PiOver2() - TMath::Abs(dirL.position().phi());
    Double_t tanl = dirL.position().z()/dirL.position().perp();
    Double_t sigmaY2, sigmaZ2;
    if (row <= 13)  StiTpcInnerHitErrorCalculator::instance()->calculateError(Z,eta,tanl,sigmaY2, sigmaZ2);
    else            StiTpcOuterHitErrorCalculator::instance()->calculateError(Z,eta,tanl,sigmaY2, sigmaZ2);
    Double_t sigmaY = TMath::Sqrt(sigmaY2);
    Double_t sigmaZ = TMath::Sqrt(sigmaZ2);
    StThreeVectorF e(0, sigmaY, sigmaZ);
    xyzL[0] += gRandom->Gaus(0, sigmaY);
    xyzL[2] += gRandom->Gaus(0, sigmaZ);
    StThreeVectorD newPosition(xyzL);
    coorLS.setPosition(newPosition);
    static StTpcPadCoordinate Pad;
    transform(coorLS,Pad,kFALSE,kTRUE); // don't use T0, use Tau
    Double_t tof = 0;
    if (gver) tof = gver[id3-1].ge_tof;
    tof += tpc_hit[i].tof;
    Float_t  timebkt =  Pad.timeBucket() + 1.e6*gStTpcDb->Electronics()->samplingFrequency()*tof;
    if (timebkt < 0 || timebkt > 512) continue;
    StTpcPadCoordinate newPad(Pad.sector(),Pad.row(), Pad.pad(),timebkt );
    static StTpcLocalCoordinate global; // leave coordinates in TpcLocalCoordinate because StTpcHitMover expects that.
    transform(newPad,global,kFALSE); // alignment
    StThreeVectorF p(global.position().x(),global.position().y(),global.position().z());
    UInt_t hw = 1;   // detid_tpc
    hw += sector << 4;     // (row/100 << 4);   // sector
    hw += row    << 9;     // (row%100 << 9);   // row
    StTpcHit *tpcHit = new StTpcHit(p,e, 
				    hw,TMath::Abs(tpc_hit[i].de), i+1,   // hw, q, c
				    tpc_hit[i].track_p, 100,             // idTruth, quality
				    0,                                   // id
				    0,  0, 0,                            // mnpad, mxpad, mntmbk
				    0, Pad.pad(), Pad.timeBucket());     // mxtmbk, cl_x , cl_t
    if (Debug() > 1) tpcHit->Print();
    rCol->addHit(tpcHit);
  }
  return kStOK;
}
