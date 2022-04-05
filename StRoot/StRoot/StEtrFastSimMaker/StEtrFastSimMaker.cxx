/* $Id: StEtrFastSimMaker.cxx,v 1.3 2012/07/21 18:43:53 perev Exp $
    $Log: StEtrFastSimMaker.cxx,v $
    Revision 1.3  2012/07/21 18:43:53  perev
    IdTruth added

    Revision 1.2  2012/04/10 22:38:18  perev
    Errors increased to 300 microns

    Revision 1.1  2012/03/22 01:19:12  perev
    *** empty log message ***

 
*/
#include "Stiostream.h"
#include "StEtrFastSimMaker.h"
#include "StEtrHit.h"
#include "StEventTypes.h"
#include "StEtrHitCollection.h"
#include "StEvent.h"
#include "TMath.h"
#include "TRandom.h"
#include "tables/St_g2t_etr_hit_Table.h"
#include "StEventHitIter.h"
#include "StuDraw3DEvent.h"
static TRandom *myRandom = 0;

ClassImp(StEtrFastSimMaker);
//____________________________________________________________
StEtrFastSimMaker::StEtrFastSimMaker(const char *name):StMaker(name)
{
  myRandom = new TRandom();
  mErr[0] = 3e-2;  mErr[1] = 3e-2; mErr[2] = 0;
}


//____________________________________________________________
int StEtrFastSimMaker::InitRun(int RunNo)
{
  // Define various SVT hit errors from database
  LOG_DEBUG << "Smearing SVT hits by " << mErr[0] << " " << mErr[2] << " cm in the ETR " << endm;
  return kStOk;
}
//____________________________________________________________
int StEtrFastSimMaker::Make()
{
  // Get the input data structures from StEvent
  StEvent *stEvent =  (StEvent*) GetInputDS("StEvent");
  if (! stEvent) {    LOG_WARN << "No StEvent on input, bye bye" << endm; return kStWarn; }
  StEtrHitCollection *etrCol = stEvent->etrHitCollection();
  if (!etrCol) {
    etrCol = new StEtrHitCollection; 
    stEvent->setEtrHitCollection(etrCol); 
  }
  TDataSetIter geant(GetInputDS("geant"));
  St_g2t_etr_hit *g2t_etr_hit = (St_g2t_etr_hit *) geant("g2t_etr_hit");
  if (! g2t_etr_hit) {
    LOG_WARN << "No g2t_etr_hit on input, bye bye" << endm; return kStWarn;
    return kStWarn;
  }
  g2t_etr_hit_st *g2t         = g2t_etr_hit->GetTable();
  int Nhits = g2t_etr_hit->GetNRows();
  if (Nhits <= 0) return kStWarn;
  for (int i = 0; i < Nhits; i++)    {
    double xyzG[3];
    g2t_etr_hit_st *tb =  g2t+i;
    for (int j=0;j<3;j++) {xyzG[j] = tb->x[j];}
//		Smearing only x,y, not Z
    for (int j=0;j<2;j++) {xyzG[j]+= gRandom->Gaus(0, mErr[j]);}

//          volume_id = section + 100*layer + 10000*sector
    int section =(tb->volume_id      )%100;
    int layer =  (tb->volume_id/100  )%100; 	// 3 disk in TRD, iLayer=0, 1, 2
    int sector = (tb->volume_id/10000)%100;		// 12 sector in TRD layer, 0 - 11

    StEtrHit *etrHit = new StEtrHit(StThreeVectorF(xyzG),sector,layer,section,tb->de);
    etrHit->setIdTruth(tb->track_p,100);
    etrCol->addHit(etrHit);
  }
  Info("Make","%d EtrHits was pushed into StEvent",Nhits);

static int etrDraw = 0;
  if (etrDraw) {
static StuDraw3DEvent *stu=0;
  if (!stu) stu = new StuDraw3DEvent;
    stu->Clear();
    StEventHitIter iter(stEvent);
    iter.AddDetector(kEtrId);
    stu->Hits(iter);
    stu->Wait();
  }

  return kStOK;
}
