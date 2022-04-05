/* $Id: St_sfs_Maker.cxx,v 1.1 2009/11/10 21:14:18 fisyak Exp $
    $Log: St_sfs_Maker.cxx,v $
    Revision 1.1  2009/11/10 21:14:18  fisyak
    pams clean up
 
*/
#include "Stiostream.h"
#include "St_sfs_Maker.h"
#include "StHit.h"
#include "StEventTypes.h"
#include "tables/St_HitError_Table.h"
#include "StSvtHitCollection.h"
#include "StEvent.h"
#include "TMath.h"
#include "TRandom.h"
#include "StSvtDbMaker/StSvtDbMaker.h"
#include "tables/St_g2t_svt_hit_Table.h"
#include "TGeoMatrix.h"
ClassImp(St_sfs_Maker);
//____________________________________________________________
Int_t St_sfs_Maker::InitRun(Int_t RunNo)
{
  // Define various SVT hit errors from database
  St_HitError *tableSet = (St_HitError *) GetDataBase("Calibrations/tracker/svtHitError");
  HitError_st* hitError = tableSet->GetTable();
  mResXSvt = TMath::Sqrt(hitError->coeff[0]);
  mResZSvt = TMath::Sqrt(hitError->coeff[3]);
  LOG_DEBUG << "Smearing SVT hits by " << mResXSvt << " " << mResZSvt << " cm in the SVT " << endm;
  return kStOk;
}
//____________________________________________________________
Int_t St_sfs_Maker::Make()
{
  if (! gRandom) gRandom = new TRandom();
  // Get the input data structures from StEvent
  StEvent *rEvent =  (StEvent*) GetInputDS("StEvent");
  if (! rEvent) {    LOG_WARN << "No StEvent on input, bye bye" << endm; return kStWarn; }
  StSvtHitCollection *rCol = rEvent->svtHitCollection();
  if (!rCol) {
    rCol = new StSvtHitCollection; 
    rEvent->setSvtHitCollection(rCol); 
  }
  TDataSetIter geant(GetInputDS("geant"));
  St_g2t_svt_hit *g2t_svt_hit = (St_g2t_svt_hit *) geant("g2t_svt_hit");
  if (! g2t_svt_hit) {
    LOG_WARN << "No g2t_svt_hit on input, bye bye" << endm; return kStWarn;
    return kStWarn;
  }
  g2t_svt_hit_st *g2t         = g2t_svt_hit->GetTable();
  Int_t Nhits = g2t_svt_hit->GetNRows();
  if (Nhits <= 0) return kStWarn;
  THashList *rotMHash = gStSvtDbMaker->GetRotations();
  if (! rotMHash) {LOG_WARN << " No list of Rotation from StSvtDbMaker " << endm; return kStWarn;}
  
  for (Int_t i = 0; i < Nhits; i++)    {
    TGeoHMatrix *comb = (TGeoHMatrix *) rotMHash->FindObject(Form("R%04i",g2t[i].volume_id%10000));
    if (! comb) { LOG_WARN << "No rotation matrix for wafer =" << g2t[i].volume_id << endm; continue;}
    Double_t xyzG[3] = {g2t[i].x[0],g2t[i].x[1],g2t[i].x[2]};
    Double_t xyzL[3];
    comb->MasterToLocal(xyzG,xyzL);
    if (Debug() && TMath::Abs(xyzL[2]) > 0.0100) {
      cout << "Id: " << g2t[i].volume_id  
	   << "\txyzL :" << xyzL[0] << "\t" << xyzL[1] << "\t" << xyzL[2] << endl;
      comb->Print();
    }
    xyzL[0] += gRandom->Gaus(0, mResXSvt);
    xyzL[1] += gRandom->Gaus(0, mResZSvt);
    comb->LocalToMaster(xyzL,xyzG);
    // Id = 1000*layer + 100*wafer + ladder;
    Int_t ladderID =  g2t[i].volume_id%100;
    Int_t waferID  = (g2t[i].volume_id/100)%10;
    Int_t layerID  = (g2t[i].volume_id%10000)/1000;
    Int_t barrelID = (layerID-1)/2 + 1;
    Int_t hybridID = 1;
    if (xyzL[0] >= 0) hybridID = 2;
    static const Int_t NumberOfHybrids = 2;
    static const Int_t NumberOfWafers[3]  = {4, 6, 7}; // Number of wafers per ladder
    static const Int_t NumberOfLadders[3] = {8,12,16}; // Number of ladders per barrel
    Int_t index = ((ladderID-1)*NumberOfWafers[barrelID-1] + (waferID-1))*NumberOfHybrids + (hybridID-1);
    switch (barrelID) {
    case 3: index += NumberOfLadders[barrelID-3]*NumberOfWafers[barrelID-3]*NumberOfHybrids;
    case 2: index += NumberOfLadders[barrelID-2]*NumberOfWafers[barrelID-2]*NumberOfHybrids;
    case 1: break;
    default:
      LOG_ERROR << "There is NO barrel number " << barrelID << " !!!" << endm;
      // gMessMgr->Print();
      index = -1;
      break;
    }
    assert(index != -1);
    Int_t hw  = 2;
    hw += (1L<<4)*(index);
    StSvtHit *svtHit = new StSvtHit();
    svtHit->setHardwarePosition(hw);
    svtHit->setLocalPosition(xyzL[0],xyzL[1]);
    svtHit->setCharge(g2t[i].de);
    svtHit->setIdTruth(g2t[i].track_p,100);
    svtHit->setFlag(0);
    svtHit->setFitFlag(0);
    StThreeVectorF pos(xyzG[0],xyzG[1],xyzG[2]);
    svtHit->setPosition(pos);
    rCol->addHit(svtHit);
  }
  return kStOK;
}
