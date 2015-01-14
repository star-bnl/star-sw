/***************************************************************************
 * Author: J. Bouchet (KSU)
 * Description: SST DB access Maker
 **************************************************************************/

#include "StSstDbMaker.h"
#include "TDataSetIter.h"
#include "StMessMgr.h"
#include "tables/St_ssdWafersPosition_Table.h"
#include "tables/St_ssdConfiguration_Table.h"
#include "tables/St_ssdDimensions_Table.h"
#include "tables/St_slsCtrl_Table.h"
#include "tables/St_Survey_Table.h"
#include "TMath.h"
#include "TVector3.h"
#include "StTpcDb/StTpcDb.h"
#include "StSsdUtil/StSstConsts.h"

StSstDbMaker *gStSstDbMaker = NULL;
THashList *StSstDbMaker::fRotList = 0;

ClassImp(StSstDbMaker)
//_____________________________________________________________________________
StSstDbMaker::StSstDbMaker(const char *name) :
   StMaker(name), mySsd(0), m_dimensions(0), m_positions(0), m_config(0), m_ctrl(0), mode(0),
   mReady(kStErr)
{
   gStSstDbMaker = this;
}
//_____________________________________________________________________________
StSstDbMaker::~StSstDbMaker() {SafeDelete(mySsd); gStSstDbMaker = 0;}


//_____________________________________________________________________________
Int_t StSstDbMaker::InitRun(Int_t runNumber)
{
   mode = m_Mode;
   m_ctrl = ((St_slsCtrl *) GetInputDB("Geometry/ssd/slsCtrl"))->GetTable();

   if (!m_ctrl) {
      LOG_ERROR << "InitRun: No relevant entry found in 'Geometry/ssd/slsCtrl' table" << endm;
      mReady = kStFatal;
      return kStFatal;
   }

   m_dimensions = (St_ssdDimensions *) GetInputDB("Geometry/ssd/ssdDimensions");

   if (!m_dimensions) {
      LOG_ERROR << "InitRun: No relevant entry found in 'Geometry/ssd/ssdDimensions' table" << endm;
      mReady = kStFatal;
      return kStFatal;
   }

   m_positions = CalculateWafersPosition();

   if (!m_positions) {
      mReady = kStFatal;
      return kStFatal;
   }

   St_ssdConfiguration *configTable = (St_ssdConfiguration *) GetInputDB("Geometry/ssd/ssdConfiguration");

   if (!configTable) {
      LOG_ERROR << "InitRun: No relevant entry found in 'Geometry/ssd/ssdConfiguration' table" << endm;
      mReady = kStFatal;
      return kStFatal;
   }

   //mConfig = new StSsdConfig();
   m_config = (ssdConfiguration_st *) configTable->GetTable() ;
   ssdDimensions_st *dimensions = m_dimensions->GetTable();
   mySsd = new StSsdBarrel(dimensions, m_config);

   if (Debug()) mySsd->SetDebug(Debug());

   mySsd->initLadders(m_positions);

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
St_ssdWafersPosition *StSstDbMaker::CalculateWafersPosition()
{
   SafeDelete(fRotList);
   fRotList = new THashList(320, 0);
   fRotList->SetOwner(kFALSE);

   TGeoHMatrix LS, SG, LA, WG;
   assert(gStTpcDb);
   const TGeoHMatrix &Tpc2Global = gStTpcDb->Tpc2GlobalMatrix();

   // SSD
   St_Survey *SsdOscOnGlobal = (St_Survey *) GetDataBase("Geometry/ssd/SsdOscOnGlobal");  // OSC in IDS

   if (!SsdOscOnGlobal) {
      LOG_ERROR << "CalculateWafersPosition: No relevant entry found in 'Geometry/ssd/SsdOscOnGlobal' table" << endm;
      return 0;
   }

   St_Survey *SsdLaddersOnOsc = (St_Survey *) GetDataBase("Geometry/ssd/SsdLaddersOnOsc");// ladders in the SSD sector coordinate systems

   if (!SsdLaddersOnOsc) {
      LOG_ERROR << "CalculateWafersPosition: No relevant entry found in 'Geometry/ssd/SsdLaddersOnOsc' table" << endm;
      return 0;
   }

   St_Survey *SsdSensorsOnLadders = (St_Survey *) GetDataBase("Geometry/ssd/SsdSensorsOnLadders");  // wafers in the SSD ladder coordinate systems

   if (!SsdSensorsOnLadders) {
      LOG_ERROR << "CalculateWafersPosition: No relevant entry found in 'Geometry/ssd/SsdSensorsOnLadders' table" << endm;
      return 0;
   }

   Survey_st *OscOnGlobal      = SsdOscOnGlobal->GetTable();
   Survey_st *LaddersOnOsc     = SsdLaddersOnOsc->GetTable();
   Survey_st *SensorsOnLadders = SsdSensorsOnLadders->GetTable();

   Int_t NoOsc     = SsdOscOnGlobal->GetNRows();
   Int_t NoLadders = SsdLaddersOnOsc->GetNRows();
   Int_t NoSensors = SsdSensorsOnLadders->GetNRows();

   LOG_DEBUG << "CalculateWafersPosition:\n"
             << "   Number of Osc:     " << NoOsc << "\n"
             << "   Number of Ladders: " << NoLadders << "\n"
             << "   Number of Sensors: " << NoSensors << endm;

   St_ssdWafersPosition *ssdwafer = new St_ssdWafersPosition("ssdWafersPosition", NoSensors);
   AddConst(ssdwafer);
   Int_t num = 0;

   for (Int_t i = 0; i < NoSensors; i++, SensorsOnLadders++) {
      Int_t Id = SensorsOnLadders->Id;
      ssdWafersPosition_st row;
      memset (&row, 0, sizeof(ssdWafersPosition_st));

      TGeoHMatrix *comb = (TGeoHMatrix *) fRotList->FindObject(Form("R%04i", Id));

      if (comb) continue;

      comb = new TGeoHMatrix(Form("R%04i", Id));
      Int_t layer  = Id / 1000;

      if (layer > 7) layer = 7;

      Int_t ladder  = Id % 100;
      TGeoHMatrix WLL;
      WLL.SetRotation(&SensorsOnLadders->r00);
      WLL.SetTranslation(&SensorsOnLadders->t0);

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

      LaddersOnOsc = SsdLaddersOnOsc->GetTable();
      Int_t Ladder = 0;
      Int_t OSC    = 0;

      for (Int_t l = 0; l < NoLadders; l++, LaddersOnOsc++) {
         Ladder = LaddersOnOsc->Id % 100;

         if (Ladder == ladder) {
            OSC = LaddersOnOsc->Id / 100;
            LS.SetRotation(&LaddersOnOsc->r00);
            LS.SetTranslation(&LaddersOnOsc->t0);

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

      OscOnGlobal = SsdOscOnGlobal->GetTable();
      Int_t osc = 0;

      for (Int_t s = 0; s < NoOsc; s++, OscOnGlobal++) {
         if (OscOnGlobal->Id != OSC) continue;

         osc = OSC;
         SG.SetRotation(&OscOnGlobal->r00);
         SG.SetTranslation(&OscOnGlobal->t0);
         break;
      }

      if (! osc) {
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

      row.id = Id;
      row.id_shape  = 2;
      row.ladder = ladder;
      row.layer  = layer;
      num++;
      row.num_chip  = (num - 1) % 16 + 1;
      Double_t *r = WG.GetRotationMatrix();
      row.driftDirection[0] = r[0]; row.normalDirection[0] = r[1]; row.transverseDirection[0] = r[2];
      row.driftDirection[1] = r[3]; row.normalDirection[1] = r[4]; row.transverseDirection[1] = r[5];
      row.driftDirection[2] = r[6]; row.normalDirection[2] = r[7]; row.transverseDirection[2] = r[8];

      Double_t *wgtr = WG.GetTranslation();
      memcpy(row.centerPosition, wgtr, 3 * sizeof(Double_t));
      comb->SetRotation(WG.GetRotationMatrix());

      comb->SetTranslation(WG.GetTranslation());

      fRotList->Add(comb);
      ssdwafer->AddAt(&row);

      if (Debug() >= 1) {
         LOG_DEBUG << "CalculateWafersPosition: R" << endm;
         comb->Print();
      }
   }

   return ssdwafer;
}


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
