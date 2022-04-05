// $Id: StSsdDbMaker.cxx,v 1.17 2014/12/05 21:59:25 smirnovd Exp $
//
// $Log: StSsdDbMaker.cxx,v $
// Revision 1.17  2014/12/05 21:59:25  smirnovd
// Fixed style with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
//
// Revision 1.16  2010/09/01 21:04:06  fisyak
// Disable sim flavor, now sim parameters is coming via DB associated with simulation time stamp
//
// Revision 1.15  2008/08/12 22:45:47  bouchet
// use of SsdLaddersOnSectors,SsdOnGlobal,SsdSectorsOnGlobal,SsdWafersOnLadders tables to calculate ssdWafersPositions;add Get methods to access the tables
//
// Revision 1.14  2008/08/01 22:07:31  bouchet
// fix bug for geometry : simulation takes ideal geometry
//
// Revision 1.13  2007/09/25 13:36:51  bouchet
// add m_Mode to constructor
//
// Revision 1.12  2007/07/12 20:01:15  fisyak
// Don't read whole database but only the table requested
//
// Revision 1.11  2007/03/21 17:17:16  fisyak
// use TGeoHMatrix for coordinate transformation, eliminate ssdWafersPostion
//
// Revision 1.10  2007/02/18 13:32:35  bouchet
// Changes for the new Logger
//
// Revision 1.9  2006/10/16 19:53:24  fisyak
// Adjust for new Ssd chain
//
// Revision 1.8  2006/09/18 16:40:14  fisyak
// Add sim flag for ssdWafersPosition
//
// Revision 1.7  2005/06/03 21:30:41  perev
// Move configuration Init()==>InitRun()
//
// Revision 1.6  2005/05/10 12:48:06  reinnart
// The new StSsdDbMaker without DirectDataBase Access
//

/***************************************************************************
 * Author: christelle roy
 * Description: SSD DB access Maker
 **************************************************************************/

#include "StSsdDbMaker.h"
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
StSsdDbMaker *gStSsdDbMaker = NULL;
THashList *StSsdDbMaker::fRotList = 0;

ClassImp(StSsdDbMaker)
//_____________________________________________________________________________
StSsdDbMaker::StSsdDbMaker(const char *name) :
   StMaker(name), mySsd(0), m_dimensions(0), m_positions(0), m_config(0), m_ctrl(0)
{
   gStSsdDbMaker = this; mode = 0;
}
//_____________________________________________________________________________
StSsdDbMaker::~StSsdDbMaker() {SafeDelete(mySsd); gStSsdDbMaker = 0;}
//_____________________________________________________________________________
Int_t StSsdDbMaker::Init()
{
   LOG_DEBUG << "Init - Start - " << endm;
#if 0

   if ( m_Mode == 1) {
      LOG_INFO << "Init setting WafersPositions to simulation" << endm;
      //SetFlavor("sim","ssdWafersPosition");
      SetFlavor("sim", "SsdOnGlobal");
      SetFlavor("sim", "SsdSectorsOnGlobal");
      SetFlavor("sim", "SsdLaddersOnSectors");
      SetFlavor("sim", "SsdWafersOnLadders");
   }

#endif
   LOG_DEBUG << "StSsdDbMaker::Init() - Done - " << endm;
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StSsdDbMaker::InitRun(Int_t runNumber)
{
   mode = m_Mode;
   m_ctrl          = ((St_slsCtrl *) GetInputDB("Geometry/ssd/slsCtrl"))->GetTable();

   if (!m_ctrl) {
      gMessMgr->Error() << "No  access to control parameters" << endm;
      return kStFatal;
   }

   m_dimensions    =  (St_ssdDimensions *) GetInputDB("Geometry/ssd/ssdDimensions");
   m_positions     =  CalculateWafersPosition();

   if ((!m_dimensions) || (!m_positions)) {
      gMessMgr->Error() << "No  access to geometry parameters" << endm;
      return kStFatal;
   }

   LOG_DEBUG << " geometry loaded " << endm;
   St_ssdConfiguration *configTable = (St_ssdConfiguration *) GetInputDB("Geometry/ssd/ssdConfiguration");

   if (!configTable) {
      gMessMgr->Error() << "InitRun : No access to ssdConfiguration database" << endm;
      return kStFatal;
   }

   //mConfig = new StSsdConfig();
   m_config = (ssdConfiguration_st *) configTable->GetTable() ;
   ssdDimensions_st *dimensions = m_dimensions->GetTable();
   mySsd = new StSsdBarrel(dimensions, m_config);

   if (Debug()) mySsd->SetDebug(Debug());

   mySsd->initLadders(m_positions);
   LOG_DEBUG << " StSsdBarrel built " << endm;
   return kStOK;
}
//_____________________________________________________________________________
Int_t StSsdDbMaker::Make()
{
   LOG_DEBUG << "Make" << endm;

   return kStOK;
}

//_____________________________________________________________________________
void StSsdDbMaker::Clear(const char *)
{
   LOG_DEBUG << "Clear" << endm;
   StMaker::Clear();
}

//_____________________________________________________________________________
Int_t StSsdDbMaker::Finish()
{
   LOG_DEBUG << "Finish" << endm;
   return kStOK;
}
//_____________________________________________________________________________
St_ssdWafersPosition *StSsdDbMaker::CalculateWafersPosition()
{
   SafeDelete(fRotList);
   fRotList = new THashList(100, 0);
   fRotList->SetOwner(kFALSE);
#if 0
   St_ssdWafersPosition *ssdWafersPosition = (St_ssdWafersPosition *) GetDataBase("Geometry/ssd/ssdWafersPosition");

   if (! ssdWafersPosition)  {cout << "ssdWafersPosition has not been found"    << endl; return 0;}

   ssdWafersPosition_st *WafersPosition = ssdWafersPosition->GetTable();
   Int_t NoWafers  = ssdWafersPosition->GetNRows();
#endif
   St_Survey *SsdOnGlobal = (St_Survey *) GetDataBase("Geometry/ssd/SsdOnGlobal");

   if (! SsdOnGlobal)  {cout << "SsdOnGlobal has not been found"  << endl; return 0;}

   TGeoHMatrix GL, LS, SG, LA, WG;
   Survey_st *OnGlobal         = SsdOnGlobal->GetTable();        // SSD and SVT as whole
   GL.SetRotation(&OnGlobal->r00);
   GL.SetTranslation(&OnGlobal->t0);
   assert(gStTpcDb);
   const TGeoHMatrix &Tpc2Global = gStTpcDb->Tpc2GlobalMatrix();
   // SSD
   St_Survey *SsdSectorsOnGlobal = (St_Survey *) GetDataBase("Geometry/ssd/SsdSectorsOnGlobal");

   if (! SsdSectorsOnGlobal)  {cout << "SsdSectorsOnGlobal has not been found"  << endl; return 0;}

   St_Survey *SsdLaddersOnSectors = (St_Survey *) GetDataBase("Geometry/ssd/SsdLaddersOnSectors");// ladders in the SSD sector coordinate systems

   if (! SsdLaddersOnSectors) {cout << "SsdLaddersOnSectors has not been found" << endl; return 0;}

   St_Survey *SsdWafersOnLadders = (St_Survey *) GetDataBase("Geometry/ssd/SsdWafersOnLadders");  // wafers in the SSD ladder coordinate systems

   if (! SsdWafersOnLadders)  {cout << "SsdWafersOnLadders has not been found"  << endl; return 0;}

   Survey_st *SectorsOnGlobal = SsdSectorsOnGlobal->GetTable();  // sectors in the SSD barrel coordinate system
   Survey_st *LaddersOnSectors = SsdLaddersOnSectors->GetTable();// ladders in the SSD sector coordinate systems
   Survey_st *WafersOnLadders = SsdWafersOnLadders->GetTable();  // wafers in the SSD ladder coordinate systems
   Int_t NoSectors = SsdSectorsOnGlobal->GetNRows();
   Int_t NoLadders = SsdLaddersOnSectors->GetNRows();
   Int_t NoWafers  = SsdWafersOnLadders->GetNRows();
   St_ssdWafersPosition *ssdwafer = new St_ssdWafersPosition("ssdWafersPosition", NoWafers);
   AddConst(ssdwafer);
   Int_t num = 0;

   //#if 1
   for (Int_t i = 0; i < NoWafers; i++, WafersOnLadders++) {
      Int_t Id = WafersOnLadders->Id;
      ssdWafersPosition_st row;
      memset (&row, 0, sizeof(ssdWafersPosition_st));
      // #else
      //   for (Int_t w  = 0; w < NoWafers; w++, WafersPosition++) {
      //     ssdWafersPosition_st row = *WafersPosition;
      //     WafersOnLadders = SsdWafersOnLadders->GetTable();
      //     Int_t Id = 0;
      //     for (Int_t i = 0; i < NoWafers; i++,WafersOnLadders++) {
      //       if (WafersOnLadders->Id != row.id) continue;
      //       Id = row.id;
      //       break;
      //     }
      //     if (! Id ) {cout << "Wafer Id\t" << Id << " has not been found" << endl; continue;}
      //#endif
      TGeoHMatrix *comb = (TGeoHMatrix *) fRotList->FindObject(Form("R%04i", Id));

      if (comb) continue;

      comb = new TGeoHMatrix(Form("R%04i", Id));
      Int_t layer  = Id / 1000;

      if (layer > 7) layer = 7;

      Int_t ladder  = Id % 100;
      TGeoHMatrix WLL;
      WLL.SetRotation(&WafersOnLadders->r00);
      WLL.SetTranslation(&WafersOnLadders->t0); //cout << "WL\t"; WL.Print();
      TGeoHMatrix *WL = (TGeoHMatrix *) fRotList->FindObject(Form("WL%04i", Id));

      if (! WL) {
         WL = new  TGeoHMatrix(Form("WL%04i", Id));
         Double_t *r = WLL.GetRotationMatrix();
         Double_t rot[9] = {r[0], r[2], r[1],
                            r[3], r[5], r[4],
                            r[6], r[8], r[7]
                           };
         // {7101,  1.000000,0, 0.000052,0, 1,0,-0.000052,0, 1.000000,-0.000600,0,-32.625900,
         WL->SetRotation(rot);
         WL->SetTranslation(WLL.GetTranslation());
         fRotList->Add(WL);
      }

      LaddersOnSectors = SsdLaddersOnSectors->GetTable();
      Int_t Ladder = 0;
      Int_t Sector = 0;

      for (Int_t l = 0; l < NoLadders; l++, LaddersOnSectors++) {
         //cout << "LaddersOnSectors Id\t" << LaddersOnSectors->Id << endl;
         Ladder = LaddersOnSectors->Id % 100;

         if (Ladder == ladder) {
            Sector = LaddersOnSectors->Id / 100;
            LS.SetRotation(&LaddersOnSectors->r00);
            LS.SetTranslation(&LaddersOnSectors->t0);
            //cout << "LS\t"; LS.Print();
            break;
         }
      }

      if (Sector <= 0 || Sector > 4) {cout << "Sector has not been defined" << endl; continue;}

      SectorsOnGlobal = SsdSectorsOnGlobal->GetTable();
      Int_t sector = 0;

      for (Int_t s = 0; s < NoSectors; s++, SectorsOnGlobal++) {
         //cout << "SectorsOnGlobal Id\t" << SectorsOnGlobal->Id << endl;
         if (SectorsOnGlobal->Id != Sector) continue;

         sector = Sector;
         SG.SetRotation(&SectorsOnGlobal->r00);
         SG.SetTranslation(&SectorsOnGlobal->t0); //cout << "Sector\t" << Sector << "\tSG\t"; SG.Print();
         break;
      }

      if (! sector) {cout << "Sector\t" << Sector << " has not been found" << endl; continue;}

      //    WG = SG * LS * WL * LA; //cout << "WG\t"; WG.Print();
      //    WG = SG * LS * WL * LA = SG * ( LS * WL * LA * WL**-1 ) *WL
      if (Debug()) {
         cout << "Tpc2Global "; Tpc2Global.Print();
         cout << "GL "; GL.Print();
         TGeoHMatrix test =  Tpc2Global * GL; cout << "test "; test.Print();
      }

      WG = Tpc2Global * GL * SG * LS * WLL; //cout << "WG\t"; WG.Print();
      row.id = Id;
      row.id_shape  = 2;
      row.ladder = ladder;
      row.layer  = layer;
      num++;
      row.num_chip  = (num - 1) % 16 + 1;
      //    TGeoHMatrix WGInv = WG.Inverse();
      //    Double_t *wgrot = WGInv.GetRotationMatrix();
      Double_t *r = WG.GetRotationMatrix();
      row.driftDirection[0] = r[0]; row.normalDirection[0] = r[1]; row.transverseDirection[0] = r[2];
      row.driftDirection[1] = r[3]; row.normalDirection[1] = r[4]; row.transverseDirection[1] = r[5];
      row.driftDirection[2] = r[6]; row.normalDirection[2] = r[7]; row.transverseDirection[2] = r[8];
      Double_t norm;
      TVector3 d(row.driftDirection); norm = 1 / d.Mag(); d *= norm;
      TVector3 t(row.transverseDirection); norm = 1 / t.Mag(); t *= norm;
      TVector3 n(row.normalDirection);
      TVector3 c = d.Cross(t);

      if (c.Dot(n) < 0) c *= -1;

      d.GetXYZ(row.driftDirection);
      t.GetXYZ(row.transverseDirection);
      c.GetXYZ(row.normalDirection);

      Double_t *wgtr = WG.GetTranslation();
      memcpy(row.centerPosition, wgtr, 3 * sizeof(Double_t));
      Double_t rot[9] = {
         row.driftDirection[0], row.transverseDirection[0], row.normalDirection[0],
         row.driftDirection[1], row.transverseDirection[1], row.normalDirection[1],
         row.driftDirection[2], row.transverseDirection[2], row.normalDirection[2]
      };
      Double_t tr[3] = {row.centerPosition[0],
                        row.centerPosition[1],
                        row.centerPosition[2]
                       };
      comb->SetRotation(rot);
      comb->SetTranslation(tr);
      fRotList->Add(comb);
      ssdwafer->AddAt(&row);

   }

   return ssdwafer;
}
