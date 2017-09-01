//$Id: StSstDbMaker.cxx,v 1.23 2017/09/01 22:22:14 yiguo Exp $
//
//$Log: StSstDbMaker.cxx,v $
//Revision 1.23  2017/09/01 22:22:14  yiguo
//corresponding to DB table change sstOnTpc = oscOnTpc*sstOnOsc by Yi Guo
//
//Revision 1.22  2016/06/20 18:48:31  bouchet
//coverity : STACK_USE ; heap allocation for sstWafersPosition
//
//Revision 1.21  2016/05/31 21:51:49  bouchet
//coverity : UNINIT_CTOR (m_positions)
//
//Revision 1.20  2015/08/03 13:07:20  bouchet
//getSstDimensions() returns the table data, not the structure
//
//Revision 1.19  2015/06/23 17:17:38  bouchet
//move to SST tables ; cpp-checked
//
//Revision 1.6  2015/06/10 13:59:15  bouchet
//cppcheck style-issues for pointer casting ; properly renamed some methods
//
//Revision 1.5  2015/05/20 13:29:25  bouchet
//mapping fixed when decoding and setting the maskChip table
//
//Revision 1.4  2015/05/08 14:14:34  bouchet
//cosmetic
//
//Revision 1.3  2015/04/27 20:07:51  bouchet
//ssdHotChip --> sstMaskChip (final name) ; get and set methods enabled
//
//Revision 1.2  2015/04/21 20:02:24  bouchet
//CVS tags added
//
/***************************************************************************
 * Author: J. Bouchet (KSU)
 * Description: SST DB access Maker
 **************************************************************************/
#include "StSstDbMaker.h"
#include "TDataSetIter.h"
#include "StMessMgr.h"
#include "tables/St_sstWafersPosition_Table.h"
#include "tables/St_sstConfiguration_Table.h"
#include "tables/St_sstDimensions_Table.h"
#include "tables/St_sstMaskChip_Table.h"
#include "tables/St_sstSlsCtrl_Table.h"
#include "tables/St_Survey_Table.h"
#include "TMath.h"
#include "TVector3.h"
#include "StTpcDb/StTpcDb.h"
#include "StSstUtil/StSstConsts.h"

StSstDbMaker *gStSstDbMaker = NULL;
THashList *StSstDbMaker::fRotList = 0;

ClassImp(StSstDbMaker)
//_____________________________________________________________________________
  StSstDbMaker::StSstDbMaker(const char *name):
    StMaker(name), mySst(0), dimensions(0), config(0), ctrl(0), mode(0),
    mReady(kStErr),m_positions(0)
{
  gStSstDbMaker = this;
}
//_____________________________________________________________________________
StSstDbMaker::~StSstDbMaker() {SafeDelete(mySst); gStSstDbMaker = 0;}
//_____________________________________________________________________________
Int_t StSstDbMaker::InitRun(Int_t runNumber)
{
   mode = m_Mode;
   
   St_sstMaskChip *maskChipTable = (St_sstMaskChip*)GetDataBase("Calibrations/sst/sstMaskChip");
   if (maskChipTable) {
     LOG_INFO << "sst mask chips table found ... initialize" << endm;
     setMaskChips(maskChipTable->GetTable());}
   else {LOG_ERROR << " no sst masking chips table " << endm; 
     mReady = kStFatal;
     return kStFatal;}  
   
   St_sstSlsCtrl *m_ctrl = (St_sstSlsCtrl *)GetDataBase("Geometry/sst/sstSlsCtrl");
   if (!m_ctrl) {
   LOG_ERROR << "InitRun: No relevant entry found in 'Geometry/sst/sstSlsCtrl' table" << endm;
   mReady = kStFatal;
   return kStFatal;
   }
   ctrl = m_ctrl->GetTable();

   St_sstDimensions *m_dimensions = (St_sstDimensions *) GetDataBase("Geometry/sst/sstDimensions");

   if (!m_dimensions) {
      LOG_ERROR << "InitRun: No relevant entry found in 'Geometry/sst/sstDimensions' table" << endm;
      mReady = kStFatal;
      return kStFatal;
   }

   dimensions = m_dimensions->GetTable();

   m_positions = calculateWafersPosition();

   if (!m_positions) {
      mReady = kStFatal;
      return kStFatal;
   }

   St_sstConfiguration *configTable = (St_sstConfiguration *) GetDataBase("Geometry/sst/sstConfiguration");

   if (!configTable) {
      LOG_ERROR << "InitRun: No relevant entry found in 'Geometry/sst/sstConfiguration' table" << endm;
      mReady = kStFatal;
      return kStFatal;
   }

   config = configTable->GetTable() ;
   sstDimensions_st *dimensions = m_dimensions->GetTable();
   mySst = new StSstBarrel(dimensions, config);

   if (Debug()) mySst->SetDebug(Debug());

   mySst->initLadders(m_positions);

   // Set the return code for Make() to kStOk since we managed to get to the end of this routine
   mReady = kStOk;

   return kStOK;
}
//_____________________________________________________________________________
Int_t StSstDbMaker::Make()
{
   return mReady;
}
//_____________________________________________________________________________
St_sstWafersPosition *StSstDbMaker::calculateWafersPosition()
{
   SafeDelete(fRotList);
   fRotList = new THashList(320, 0);
   fRotList->SetOwner(kFALSE);

   ///////////////////////////////
   // matrix LS ~ ladderOnSst
   //        OT ~ oscOnTpc
   //        SO ~ sstOnOsc
   //        SG = OT*SO

   TGeoHMatrix LS, OT, SO, SG, LA, WG;
   assert(gStTpcDb);
   const TGeoHMatrix &Tpc2Global = gStTpcDb->Tpc2GlobalMatrix();

   // SSD
   St_Survey *sstOnOsc = (St_Survey *) GetDataBase("Geometry/sst/sstOnOsc");  // sst in Osc

   if (!sstOnOsc) {
      LOG_ERROR << "CalculateWafersPosition: No relevant entry found in 'Geometry/sst/sstOnOsc' table" << endm;
      return 0;
   }

   St_Survey *oscOnTpc = (St_Survey *) GetDataBase("Geometry/sst/oscOnTpc");  // osc in tpc
   if (!oscOnTpc) {
      LOG_ERROR << "CalculateWafersPosition: No relevant entry found in 'Geometry/sst/oscOnTpc' table" << endm;
      return 0;
   }

   St_Survey *sstLadderOnSst = (St_Survey *) GetDataBase("Geometry/sst/sstLadderOnSst");// ladders in the SST sector coordinate systems

   if (!sstLadderOnSst) {
      LOG_ERROR << "CalculateWafersPosition: No relevant entry found in 'Geometry/sst/sstLadderOnSst' table" << endm;
      return 0;
   }

   St_Survey *sstSensorOnLadder = (St_Survey *) GetDataBase("Geometry/sst/sstSensorOnLadder");  // wafers in the SST ladder coordinate systems

   if (!sstSensorOnLadder) {
      LOG_ERROR << "CalculateWafersPosition: No relevant entry found in 'Geometry/sst/sstSensorOnLadder' table" << endm;
      return 0;
   }

   Survey_st *oscOnTpc_st    = oscOnTpc->GetTable(); 
   Survey_st *sstOnOsc_st    = sstOnOsc->GetTable();
   Survey_st *ladderOnSst    = sstLadderOnSst->GetTable();
   Survey_st *sensorOnLadder = sstSensorOnLadder->GetTable();

   Int_t NoOsc     = sstOnOsc->GetNRows();
   Int_t NoLadders = sstLadderOnSst->GetNRows();
   Int_t NoSensors = sstSensorOnLadder->GetNRows();

   LOG_DEBUG << "CalculateWafersPosition:\n"
             << "   Number of Osc:     " << NoOsc << "\n"
             << "   Number of Ladders: " << NoLadders << "\n"
             << "   Number of Sensors: " << NoSensors << endm;

   St_sstWafersPosition *sstwafer = new St_sstWafersPosition("sstWafersPosition", NoSensors);
   AddConst(sstwafer);
   Int_t num = 0;
   sstWafersPosition_st *row = new sstWafersPosition_st();
   memset(row,0, 4*960*sizeof(Double_t));

   OT.SetRotation(&oscOnTpc_st->r00);
   OT.SetTranslation(&oscOnTpc_st->t0);

   for (Int_t i = 0; i < NoSensors; i++, sensorOnLadder++) {
      Int_t Id = sensorOnLadder->Id;

      TGeoHMatrix *comb = (TGeoHMatrix *) fRotList->FindObject(Form("R%04i", Id));

      if (comb) continue;

      comb = new TGeoHMatrix(Form("R%04i", Id));
      Int_t layer  = Id / 1000;

      if (layer > 7) layer = 7;

      Int_t ladder  = Id % 100;
      TGeoHMatrix WLL;
      WLL.SetRotation(&sensorOnLadder->r00);
      WLL.SetTranslation(&sensorOnLadder->t0);

      if (Debug() >= 2) {
         LOG_DEBUG << "CalculateWafersPosition: WL" << endm;
         WLL.Print();
      }

      TGeoHMatrix *WL = (TGeoHMatrix *) fRotList->FindObject(Form("WL%04i", Id));

      if (! WL) {
         WL = new  TGeoHMatrix(Form("WL%04i", Id));
         Double_t *r = WLL.GetRotationMatrix();
         Double_t rot[9] = {r[0], r[1], r[2],
                            r[3], r[4], r[5],
                            r[6], r[7], r[8]
                           };
         WL->SetRotation(rot);
         WL->SetTranslation(WLL.GetTranslation());
         fRotList->Add(WL);
      }

      ladderOnSst = sstLadderOnSst->GetTable();
      Int_t Ladder = 0;
      Int_t OSC    = 0;

      for (Int_t l = 0; l < NoLadders; l++, ladderOnSst++) {
         Ladder = ladderOnSst->Id % 100;

         if (Ladder == ladder) {
            OSC = ladderOnSst->Id / 100;
            LS.SetRotation(&ladderOnSst->r00);
            LS.SetTranslation(&ladderOnSst->t0);

            if (Debug() >= 2) {
               LOG_DEBUG << "CalculateWafersPosition: LS" << endm;
               LS.Print();
            }

            break;
         }
      }

      if (OSC != 1) {
         LOG_WARN << "CalculateWafersPosition: OSC has not been defined. Skipping to next sensor..." << endm;
         continue;
      }

      sstOnOsc_st = sstOnOsc->GetTable();
      Int_t osc = 0;

      for (Int_t s = 0; s < NoOsc; s++, sstOnOsc_st++) {
         if (sstOnOsc_st->Id != OSC) continue;

         osc = OSC;
         SO.SetRotation(&sstOnOsc_st->r00);
         SO.SetTranslation(&sstOnOsc_st->t0);
         break;
      }

      SG = OT * SO;

      if (!osc) {
         LOG_WARN << "CalculateWafersPosition: OSC " << OSC << " has not been found. Skipping to next sensor..." << endm;
         continue;
      }

      if (Debug() >= 2) {
         LOG_DEBUG << "CalculateWafersPosition: Tpc2Global" << endm;
         Tpc2Global.Print();
      }

      WG = Tpc2Global * SG * LS * WLL;

      if (Debug() >= 2) {
         LOG_DEBUG << "CalculateWafersPosition: WG" << endm;
         WG.Print();
      }

      Double_t *r = WG.GetRotationMatrix();
      Int_t index = num*3;
      row->driftDirection[index+0]      = r[0];
      row->driftDirection[index+1]      = r[3];
      row->driftDirection[index+2]      = r[6];
      
      row->normalDirection[index+0]     = r[1];
      row->normalDirection[index+1]     = r[4];
      row->normalDirection[index+2]     = r[7];
      
      row->transverseDirection[index+0] = r[2];
      row->transverseDirection[index+1] = r[5];
      row->transverseDirection[index+2] = r[8];

      Double_t *wgtr = WG.GetTranslation();
      row->centerPosition[index+0]      = wgtr[0];
      row->centerPosition[index+1]      = wgtr[1];
      row->centerPosition[index+2]      = wgtr[2];

      comb->SetRotation(WG.GetRotationMatrix());
      comb->SetTranslation(WG.GetTranslation());

      fRotList->Add(comb);
      num++;

      if (Debug() >= 1) {
         LOG_DEBUG << "CalculateWafersPosition: R" << endm;
         comb->Print();
      }
   }
   sstwafer->AddAt(row);
   delete row;
   return sstwafer;
}
//_____________________________________________________________________________
/**
 * Returns TGeoHMatrix with complete set of transformations from the sensor
 * local coordinate system to the global one. The ladder and the sensor id's are
 * expected to follow the human friendly numbering scheme, i.e.
 *
 * <pre>
 * 1 <= ladder <= kSstNumLadders
 * 1 <= sensor <= kSstNumSensorsPerLadder
 * </pre>
 */
const TGeoHMatrix *StSstDbMaker::getHMatrixSensorOnGlobal(int ladder, int sensor)
{
   if (ladder < 1 || ladder > kSstNumLadders || sensor < 1 || sensor > kSstNumSensorsPerLadder)
      return 0;

   int id = 7000 + 100*sensor + ladder;
   return fRotList ? (TGeoHMatrix *) fRotList->FindObject(Form("R%04i", id)) : 0;
}
//_____________________________________________________________________________
Int_t StSstDbMaker::maskChip(Int_t side, Int_t ladder, Int_t wafer, Int_t chip) const
{
  map<unsigned int,short>::const_iterator got;
  got = mMapMaskChips.find(side*1920 + ladder*96 + wafer*6 + chip);
  if ( got == mMapMaskChips.end() ) {
    return 0;
  }
  else {
    return 1;
  }
}
//_____________________________________________________________________________ 
void StSstDbMaker::setMaskChips(sstMaskChip_st *maskChipTable)
{
  for(Int_t i=0; i<3840; ++i){ 
    if(maskChipTable[0].chip[i]>0){ 
      mMapMaskChips.insert ( std::pair<unsigned long, short>(i,maskChipTable[0].chip[i]) );
      LOG_DEBUG <<" found chip to mask : adress : " << i << endm; 
    } 
    //else break;
  }
}

