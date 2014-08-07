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
StSstDbMaker* gStSstDbMaker=NULL; 
THashList *StSstDbMaker::fRotList = 0;

ClassImp(StSstDbMaker)
//_____________________________________________________________________________
StSstDbMaker::StSstDbMaker(const char *name) : 
  StMaker(name), mySsd(0),m_dimensions(0),m_positions(0),m_config(0),m_ctrl(0){
  gStSstDbMaker = this;mode=0;
}
//_____________________________________________________________________________
StSstDbMaker::~StSstDbMaker() {SafeDelete(mySsd); gStSstDbMaker = 0;}
//_____________________________________________________________________________
Int_t StSstDbMaker::Init()
{
  LOG_DEBUG << "Init - Start - " << endm;
  LOG_DEBUG << "StSstDbMaker::Init() - Done - "<<endm;
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StSstDbMaker::InitRun(Int_t runNumber) {
  mode = m_Mode;
  m_ctrl          = ((St_slsCtrl           *) GetInputDB("Geometry/ssd/slsCtrl"))->GetTable();
  if (!m_ctrl) {
    gMessMgr->Error() << "No  access to control parameters" << endm;
    return kStFatal;
  }   
  m_dimensions    =  (St_ssdDimensions     *) GetInputDB("Geometry/ssd/ssdDimensions"); 
  m_positions     =  CalculateWafersPosition();  
  
  if ((!m_dimensions)||(!m_positions)) {
    gMessMgr->Error() << "No  access to geometry parameters" << endm;
    return kStFatal;
  }
  LOG_DEBUG << " geometry loaded " << endm;
  St_ssdConfiguration* configTable = (St_ssdConfiguration*) GetInputDB("Geometry/ssd/ssdConfiguration");
  if (!configTable) {
    gMessMgr->Error() << "InitRun : No access to ssdConfiguration database" << endm;
    return kStFatal;
  }
  //mConfig = new StSsdConfig();
  m_config = (ssdConfiguration_st*) configTable->GetTable() ; 
  ssdDimensions_st *dimensions = m_dimensions->GetTable();
  mySsd = new StSsdBarrel(dimensions, m_config);
  if (Debug()) mySsd->SetDebug(Debug());
  mySsd->initLadders(m_positions);
  LOG_DEBUG << " StSsdBarrel built " << endm;
  return kStOK;
}
//_____________________________________________________________________________
Int_t StSstDbMaker::Make()
{
  LOG_DEBUG << "Make" << endm; 
  return kStOK;
}
//_____________________________________________________________________________
void StSstDbMaker::Clear(const char*)
{
  LOG_DEBUG << "Clear" << endm;
  StMaker::Clear();
}
//_____________________________________________________________________________
Int_t StSstDbMaker::Finish()
{
  LOG_DEBUG << "Finish" << endm;
  return kStOK;
}
//_____________________________________________________________________________
St_ssdWafersPosition *StSstDbMaker::CalculateWafersPosition(){
  SafeDelete(fRotList);
  fRotList = new THashList(320,0);
  fRotList->SetOwner(kFALSE);
#if 0
  St_ssdWafersPosition *ssdWafersPosition = (St_ssdWafersPosition *) GetDataBase("Geometry/ssd/ssdWafersPosition");
  if (! ssdWafersPosition)  {cout << "ssdWafersPosition has not been found"    << endl; return 0;}
  ssdWafersPosition_st *WafersPosition = ssdWafersPosition->GetTable(); 
  Int_t NoWafers  = ssdWafersPosition->GetNRows();
#endif
  TGeoHMatrix LS,SG,LA,WG;
  assert(gStTpcDb);
  const TGeoHMatrix &Tpc2Global = gStTpcDb->Tpc2GlobalMatrix();
  
  // SSD 
  St_Survey *SsdOscOnGlobal      = (St_Survey *) GetDataBase("Geometry/ssd/SsdOscOnGlobal");  // OSC in IDS
  if (! SsdOscOnGlobal)      {cout << "OscOnGlobal has not been found"  << endl; return 0;}
  St_Survey *SsdLaddersOnOsc     = (St_Survey *) GetDataBase("Geometry/ssd/SsdLaddersOnOsc");// ladders in the SSD sector coordinate systems
  if (! SsdLaddersOnOsc)     {cout << "SsdLaddersOnOsc has not been found" << endl; return 0;}
  St_Survey *SsdSensorsOnLadders = (St_Survey *) GetDataBase("Geometry/ssd/SsdSensorsOnLadders");  // wafers in the SSD ladder coordinate systems
  if (! SsdSensorsOnLadders) {cout << "SsdSensorsOnLadders has not been found"  << endl; return 0;}
  
  Survey_st *OscOnGlobal      = SsdOscOnGlobal->GetTable();      
  Survey_st *LaddersOnOsc     = SsdLaddersOnOsc->GetTable();     
  Survey_st *SensorsOnLadders = SsdSensorsOnLadders->GetTable(); 
  
  Int_t NoOsc     = SsdOscOnGlobal->GetNRows();
  Int_t NoLadders = SsdLaddersOnOsc->GetNRows();
  Int_t NoSensors = SsdSensorsOnLadders->GetNRows();

  LOG_DEBUG <<" # of Osc : " << NoOsc << endm;
  LOG_DEBUG <<" # of Ladders : " << NoLadders << endm;
  LOG_DEBUG <<" # of Sensors : " << NoSensors << endm;

  St_ssdWafersPosition *ssdwafer = new St_ssdWafersPosition("ssdWafersPosition",NoSensors);
  AddConst(ssdwafer);
  Int_t num = 0;
  
  for (Int_t i = 0; i < NoSensors; i++,SensorsOnLadders++) 
    {
      Int_t Id = SensorsOnLadders->Id;
      ssdWafersPosition_st row;
      memset (&row, 0, sizeof(ssdWafersPosition_st));
      
      TGeoHMatrix *comb = (TGeoHMatrix *) fRotList->FindObject(Form("R%04i",Id));
      if (comb) continue;
      comb = new TGeoHMatrix(Form("R%04i",Id)); 
      Int_t layer  = Id/1000;
      if (layer > 7) layer = 7;
      Int_t ladder  = Id%100;
      TGeoHMatrix WLL;
      WLL.SetRotation(&SensorsOnLadders->r00);
      WLL.SetTranslation(&SensorsOnLadders->t0);
      //cout << "WL\t"; WLL.Print();
      TGeoHMatrix *WL = (TGeoHMatrix *) fRotList->FindObject(Form("WL%04i",Id));
      if (! WL) 
	{
	  WL = new  TGeoHMatrix(Form("WL%04i",Id)); 
	  Double_t *r = WLL.GetRotationMatrix();   
	  Double_t rot[9] = {r[0], r[1], r[2],   
			     r[3], r[4], r[5],   
			     r[6], r[7], r[8]};   
	  WL->SetRotation(rot);
	  WL->SetTranslation(WLL.GetTranslation());
	  fRotList->Add(WL);
	}
      LaddersOnOsc = SsdLaddersOnOsc->GetTable();
      Int_t Ladder = 0;
      Int_t OSC    = 0;
      for (Int_t l = 0; l < NoLadders; l++, LaddersOnOsc++) 
	{
	  //cout << "LaddersOnOsc Id\t" << LaddersOnOsc->Id << endl;
	  Ladder = LaddersOnOsc->Id%100;
	  if (Ladder == ladder) 
	    {
	      OSC = LaddersOnOsc->Id/100;
	      LS.SetRotation(&LaddersOnOsc->r00);
	      LS.SetTranslation(&LaddersOnOsc->t0);
	      //cout << "LS\t"; LS.Print();
	      break;
	    }
	}
      
      if (OSC != 1) {cout << "Osc has not been defined" << endl; continue;}
      OscOnGlobal = SsdOscOnGlobal->GetTable();
      Int_t osc = 0;
      for (Int_t s = 0; s <NoOsc; s++, OscOnGlobal++) 
	{
	  //cout << "OscOnGlobal Id\t" << OscOnGlobal->Id << endl;
	  if (OscOnGlobal->Id != OSC) continue;
	  osc = OSC;
	  SG.SetRotation(&OscOnGlobal->r00);
	  SG.SetTranslation(&OscOnGlobal->t0); 
	  //cout << "OSC\t" << OSC << "\tSG\t"; //SG.Print();
	  break;
	}
      if (! osc) {cout << "OSC\t" << OSC << " has not been found" << endl; continue;}
      
      if (Debug()) 
	{
	  cout << "Tpc2Global "; //Tpc2Global.Print();
	}
      WG = Tpc2Global * SG * LS * WLL; //cout << "WG\t"; WG.Print();
      
      row.id = Id;
      row.id_shape  = 2;
      row.ladder = ladder;
      row.layer  = layer;
      num++;
      row.num_chip  = (num-1)%16 + 1;
      Double_t *r = WG.GetRotationMatrix();
      row.driftDirection[0] = r[0]; row.normalDirection[0] = r[1]; row.transverseDirection[0] = r[2];
      row.driftDirection[1] = r[3]; row.normalDirection[1] = r[4]; row.transverseDirection[1] = r[5];
      row.driftDirection[2] = r[6]; row.normalDirection[2] = r[7]; row.transverseDirection[2] = r[8];
      Double_t norm;
      TVector3 d(row.driftDirection); norm = 1/d.Mag(); d *= norm;
      TVector3 t(row.transverseDirection); norm = 1/t.Mag(); t *= norm;
      TVector3 n(row.normalDirection);
      TVector3 c = d.Cross(t);
      if (c.Dot(n) < 0) c *= -1;
      d.GetXYZ(row.driftDirection);
      t.GetXYZ(row.transverseDirection);
      c.GetXYZ(row.normalDirection);
      
      Double_t *wgtr = WG.GetTranslation();
      memcpy(row.centerPosition,wgtr, 3*sizeof(Double_t));
      
      Double_t rot[9] = 
	{
	  row.driftDirection[0], row.normalDirection[0], row.transverseDirection[0],
	  row.driftDirection[1], row.normalDirection[1], row.transverseDirection[1],
	  row.driftDirection[2], row.normalDirection[2], row.transverseDirection[2]
	};
      
      Double_t tr[3] = {
	row.centerPosition[0],
	row.centerPosition[1],
	row.centerPosition[2]};
      
      comb->SetRotation(WG.GetRotationMatrix());
      comb->SetTranslation(WG.GetTranslation());
      
      fRotList->Add(comb);
      ssdwafer->AddAt(&row);
      //comb->Print();
    }
  return ssdwafer;
}
