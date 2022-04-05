//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_sdm_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
#include <stdlib.h>
#include "St_sdm_Maker.h"
#include "St_DataSetIter.h"

#include "tables/St_sdm_condition_par_Table.h"
#include "tables/St_sdm_geom_par_Table.h"
#include "tables/St_sdm_calib_par_Table.h"
#include "tables/St_sdm_calib_db_Table.h"
#include "tables/St_sdm_condition_db_Table.h"

#include "TFile.h"
#include "TString.h"
#include "TRandom.h"

ClassImp(St_sdm_Maker)
  
//_____________________________________________________________________________
St_sdm_Maker::St_sdm_Maker(const char *name):
StMaker(name),
m_cond_par(0),
m_geom_par(0),
m_cal_par(0),
m_DBPath(0),
m_ParPath(0),
m_DBRandom(0)
{
  //m_CondDBFile(0),
  //m_CalibDBFile(0)
//m_noise(0),
//m_condition(0),
}
//_____________________________________________________________________________
St_sdm_Maker::~St_sdm_Maker()
{
}
//_____________________________________________________________________________
Int_t St_sdm_Maker::Init(){
  m_DBRandom = new TRandom();
  if(!m_ParPath) 
    {
      cout<<" **** Path for Parameter Tables not set : Take default Parameter Tables "<<endl;
      InitConditionPar();
      InitCalibPar();
      InitGeomPar();
    }
  else
    {
      cout<<"Parameter Tables  Path : "<<m_ParPath->Data()<<endl;
      cout<<"DB Tables Path : "<<m_DBPath->Data()<<endl;

      if (!(LoadConditionPar()))
	{
	  cout<<" *** Condition Parameter Table not found : Take default parameters ***"<<endl;
	  InitConditionPar();
	}
      
      if (!(LoadCalibPar()))
	{
	  cout<<" *** Calibration Parameter Table not found : Take default parameters ***"<<endl;
	  InitCalibPar();
	}
      
      if (!(LoadGeomPar()))
	{
	  cout<<" *** Geometry Parameter Table not found : Take default parameters ***"<<endl;
	  InitGeomPar();
	}
    }

  sdm_geom_par_st *m_geom_par_t = m_geom_par->GetTable();
  
  mSsdLayer      = m_geom_par_t[0].N_layer;
  mSsdTotLadder  = m_geom_par_t[0].N_ladder;
  mSsdTotWafer   = mSsdTotLadder*m_geom_par_t[0].N_waf_per_ladder;
  mSsdTotPlane   = 2*mSsdTotWafer;
  mSsdTotA128    = m_geom_par_t[0].N_alice_per_side*mSsdTotPlane;
  mSsdTotStrip   = mSsdTotPlane*m_geom_par_t[0].N_strip_per_side;

  if(!m_DBPath) 
    {
      cout<<" **** Path for DataBase Tables not set : Write in Local Directory "<<endl;
      m_DBPath = new TString(".");
    }

  TString *fCalibDBName = new TString("sdm_calib_db.root");
  TFile m_CalibDBFile(((*m_DBPath)+(*fCalibDBName)).Data(),"RECREATE");
  m_CalibDBFile.Close();

  TString *fCondDBName = new TString("sdm_condition_db.root");
  TFile m_CondDBFile(((*m_DBPath)+(*fCondDBName)).Data(),"RECREATE");
  m_CondDBFile.Close();

  delete fCalibDBName;
  delete fCondDBName;
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_sdm_Maker::Make()
{
  BuildCalibDB();
  BuildConditionDB();
  return kStOK;
}
//_____________________________________________________________________________
void St_sdm_Maker::PrintInfo()
{
  printf("**************************************************************\n");
  printf("* $Id: St_sdm_Maker.cxx,v 1.3 2007/04/28 17:56:53 perev Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}
//_____________________________________________________________________________
void  St_sdm_Maker::SetParamPath(Char_t *ParPath)
{
  if (ParPath) m_ParPath = new TString(ParPath);
}
//_____________________________________________________________________________
void  St_sdm_Maker::SetDBPath(Char_t *DBPath)
{
  if (DBPath) m_DBPath = new TString(DBPath);
}
//_____________________________________________________________________________
Bool_t  St_sdm_Maker::LoadConditionPar()
{
  TString *fCondName = new TString("sdm_condition_par.root");
  TFile *fCond = new TFile(((*m_ParPath)+(*fCondName)).Data());

  if (!fCond->IsOpen()) return kFALSE;
  St_sdm_condition_par *file_cond_par = ((St_sdm_condition_par*)fCond->Get("sdm_condition_par"));

  m_cond_par = new St_sdm_condition_par("sdm_condition_par",1);
//   m_cond_par->SetNRow(0);
  sdm_condition_par_st *m_cond_par_t = m_cond_par->GetTable();
  sdm_condition_par_st *file_cond_par_t = file_cond_par->GetTable();

  m_cond_par->SetNRows(file_cond_par->GetNRows());

  m_cond_par_t[0].i_seed              =   file_cond_par_t[0].i_seed;
  m_cond_par_t[0].N_active_ladder[0]  =   file_cond_par_t[0].N_active_ladder[0]  ; 
  m_cond_par_t[0].N_active_ladder[1]  =   file_cond_par_t[0].N_active_ladder[1]  ; 
  m_cond_par_t[0].N_active_ladder[2]  =   file_cond_par_t[0].N_active_ladder[2]  ; 
  m_cond_par_t[0].N_active_ladder[3]  =   file_cond_par_t[0].N_active_ladder[3]  ; 
  m_cond_par_t[0].N_active_ladder[4]  =   file_cond_par_t[0].N_active_ladder[4]  ; 
  m_cond_par_t[0].N_active_ladder[5]  =   file_cond_par_t[0].N_active_ladder[5]  ; 
  m_cond_par_t[0].N_active_ladder[6]  =   file_cond_par_t[0].N_active_ladder[6]  ; 
  m_cond_par_t[0].N_active_ladder[7]  =   file_cond_par_t[0].N_active_ladder[7]  ; 
  m_cond_par_t[0].N_active_ladder[8]  =   file_cond_par_t[0].N_active_ladder[8]  ; 
  m_cond_par_t[0].N_active_ladder[9]  =   file_cond_par_t[0].N_active_ladder[9]  ; 
  m_cond_par_t[0].N_active_ladder[10] =   file_cond_par_t[0].N_active_ladder[10] ; 
  m_cond_par_t[0].N_active_ladder[11] =   file_cond_par_t[0].N_active_ladder[11] ; 
  m_cond_par_t[0].N_active_ladder[12] =   file_cond_par_t[0].N_active_ladder[12] ; 
  m_cond_par_t[0].N_active_ladder[13] =   file_cond_par_t[0].N_active_ladder[13] ; 
  m_cond_par_t[0].N_active_ladder[14] =   file_cond_par_t[0].N_active_ladder[14] ; 
  m_cond_par_t[0].N_active_ladder[15] =   file_cond_par_t[0].N_active_ladder[15] ; 
  m_cond_par_t[0].N_active_ladder[16] =   file_cond_par_t[0].N_active_ladder[16] ; 
  m_cond_par_t[0].N_active_ladder[17] =   file_cond_par_t[0].N_active_ladder[17] ; 
  m_cond_par_t[0].N_active_ladder[18] =   file_cond_par_t[0].N_active_ladder[18] ; 
  m_cond_par_t[0].N_active_ladder[19] =   file_cond_par_t[0].N_active_ladder[19] ; 
  m_cond_par_t[0].p_bad_wafer         =   file_cond_par_t[0].p_bad_wafer         ; 
  m_cond_par_t[0].p_bad_alice         =   file_cond_par_t[0].p_bad_alice         ; 
  m_cond_par_t[0].p_bad_strip         =   file_cond_par_t[0].p_bad_strip         ; 

  fCond->Close();
  delete fCond;
  delete fCondName;
  if (!m_cond_par) return kFALSE;
  return kTRUE;

}
//_____________________________________________________________________________
Bool_t  St_sdm_Maker::LoadGeomPar()
{
  TString *fGeomName = new TString("sdm_geom_par.root");
  TFile *fGeom = new TFile(((*m_ParPath)+(*fGeomName)).Data());

  if (!fGeom->IsOpen()) return kFALSE;

  St_sdm_geom_par *file_geom_par = ((St_sdm_geom_par*)fGeom->Get("sdm_geom_par"));

  m_geom_par = new St_sdm_geom_par("sdm_geom_par",1);
//   m_geom_par->SetNRow(0);
  sdm_geom_par_st *m_geom_par_t = m_geom_par->GetTable();

  sdm_geom_par_st *file_geom_par_t = file_geom_par->GetTable();

  m_geom_par->SetNRows(file_geom_par->GetNRows());

  m_geom_par_t[0].N_layer           =   file_geom_par_t[0].N_layer           ; 
  m_geom_par_t[0].N_ladder          =   file_geom_par_t[0].N_ladder          ; 
  m_geom_par_t[0].N_waf_per_ladder  =   file_geom_par_t[0].N_waf_per_ladder  ; 
  m_geom_par_t[0].N_alice_per_side  =   file_geom_par_t[0].N_alice_per_side  ; 
  m_geom_par_t[0].N_strip_per_side  =   file_geom_par_t[0].N_strip_per_side  ; 
  m_geom_par_t[0].L_strip_pitch     =   file_geom_par_t[0].L_strip_pitch     ; 
  m_geom_par_t[0].L_stereo_angle    =   file_geom_par_t[0].L_stereo_angle    ; 
  m_geom_par_t[0].L_wafer_tot_l     =   file_geom_par_t[0].L_wafer_tot_l     ; 
  m_geom_par_t[0].L_wafer_tot_w     =   file_geom_par_t[0].L_wafer_tot_w     ; 
  m_geom_par_t[0].L_wafer_tot_t     =   file_geom_par_t[0].L_wafer_tot_t     ; 
  m_geom_par_t[0].L_wafer_act_l     =   file_geom_par_t[0].L_wafer_act_l     ; 
  m_geom_par_t[0].L_wafer_act_w     =   file_geom_par_t[0].L_wafer_act_w     ; 

  fGeom->Close();
  delete fGeom;
  delete fGeomName;
  if (!m_geom_par) return kFALSE;
  return kTRUE;

 }
//_____________________________________________________________________________
Bool_t  St_sdm_Maker::LoadCalibPar()
{
  TString *fCalibName = new TString("sdm_calib_par.root");
  TFile *fCalib = new TFile(((*m_ParPath)+(*fCalibName)).Data());

  if (!fCalib->IsOpen()) return kFALSE;

  St_sdm_calib_par *file_cal_par = ((St_sdm_calib_par*)fCalib->Get("sdm_calib_par"));

  m_cal_par = new St_sdm_calib_par("sdm_calib_par",1);
//   m_cal_par->SetNRow(0);
  sdm_calib_par_st *m_cal_par_t = m_cal_par->GetTable();

  sdm_calib_par_st *file_cal_par_t = file_cal_par->GetTable();

  m_cal_par->SetNRows(file_cal_par->GetNRows());

  m_cal_par_t[0].i_seed             =   file_cal_par_t[0].i_seed             ; 
  m_cal_par_t[0].barrel_ped         =   file_cal_par_t[0].barrel_ped         ; 
  m_cal_par_t[0].wafer_sig          =   file_cal_par_t[0].wafer_sig          ; 
  m_cal_par_t[0].alice_sig          =   file_cal_par_t[0].alice_sig          ; 
  m_cal_par_t[0].strip_P_sig        =   file_cal_par_t[0].strip_P_sig        ; 
  m_cal_par_t[0].strip_N_sig        =   file_cal_par_t[0].strip_N_sig        ; 
  m_cal_par_t[0].strip_P_noise      =   file_cal_par_t[0].strip_P_noise      ; 
  m_cal_par_t[0].strip_N_noise      =   file_cal_par_t[0].strip_N_noise      ; 
  m_cal_par_t[0].strip_P_noise_sig  =   file_cal_par_t[0].strip_P_noise_sig  ; 
  m_cal_par_t[0].strip_N_noise_sig  =   file_cal_par_t[0].strip_N_noise_sig  ; 
  m_cal_par_t[0].n_strip_P_factor   =   file_cal_par_t[0].n_strip_P_factor   ; 
  m_cal_par_t[0].n_strip_N_factor   =   file_cal_par_t[0].n_strip_N_factor   ; 
  m_cal_par_t[0].n_noisy_strip      =   file_cal_par_t[0].n_noisy_strip      ; 

  fCalib->Close();
  delete fCalib;
  delete fCalibName;
  if (!m_cal_par) return kFALSE;
  return kTRUE;
}
//_____________________________________________________________________________
void   St_sdm_Maker::InitConditionPar()
{
  m_cond_par = new St_sdm_condition_par("sdm_condition_par",1);
  m_cond_par->SetNRows(1);
  sdm_condition_par_st *m_cond_par_t = m_cond_par->GetTable();
  
  m_cond_par_t[0].i_seed              = 111111;//              random initialisation seed 
  m_cond_par_t[0].N_active_ladder[0]  = 1     ;// array of active(1) and dead(0) ladders
  m_cond_par_t[0].N_active_ladder[1]  = 1     ;
  m_cond_par_t[0].N_active_ladder[2]  = 1     ;
  m_cond_par_t[0].N_active_ladder[3]  = 1     ;
  m_cond_par_t[0].N_active_ladder[4]  = 1     ;
  m_cond_par_t[0].N_active_ladder[5]  = 1     ;
  m_cond_par_t[0].N_active_ladder[6]  = 1     ;
  m_cond_par_t[0].N_active_ladder[7]  = 1     ;
  m_cond_par_t[0].N_active_ladder[8]  = 1     ;
  m_cond_par_t[0].N_active_ladder[9]  = 1     ;
  m_cond_par_t[0].N_active_ladder[10] = 1     ;
  m_cond_par_t[0].N_active_ladder[11] = 1     ;
  m_cond_par_t[0].N_active_ladder[12] = 1     ;
  m_cond_par_t[0].N_active_ladder[13] = 1     ;
  m_cond_par_t[0].N_active_ladder[14] = 1     ;
  m_cond_par_t[0].N_active_ladder[15] = 1     ;
  m_cond_par_t[0].N_active_ladder[16] = 1     ;
  m_cond_par_t[0].N_active_ladder[17] = 1     ;
  m_cond_par_t[0].N_active_ladder[18] = 1     ;
  m_cond_par_t[0].N_active_ladder[19] = 1     ;
  m_cond_par_t[0].p_bad_wafer         = 0.0;//percentage of dead wafers
  m_cond_par_t[0].p_bad_alice         = 0.0;//percentage of dead A128C
  m_cond_par_t[0].p_bad_strip         = 0.00;//percentage of dead strips 
}
//_____________________________________________________________________________
void   St_sdm_Maker::InitGeomPar()
{
  m_geom_par = new St_sdm_geom_par("sdm_geom_par",1);
  m_geom_par->SetNRows(1);
  sdm_geom_par_st *m_geom_par_t = m_geom_par->GetTable();

  m_geom_par_t[0].N_layer           = 7     ;// SSD layer number
  m_geom_par_t[0].N_ladder          = 20    ;// Nbr ladder per layer
  m_geom_par_t[0].N_waf_per_ladder  = 16    ;// Nbr wafer per ladder
  m_geom_par_t[0].N_alice_per_side  = 6     ;// Nbr A128C per wafer side
  m_geom_par_t[0].N_strip_per_side  = 768   ;// Nbr Strip per wafer side 
  m_geom_par_t[0].L_strip_pitch     = 0.0095;// strip pitch
  m_geom_par_t[0].L_stereo_angle    = 0.0175;// half stereo angle
  m_geom_par_t[0].L_wafer_tot_l     = 3.75  ;// half wafer total length
  m_geom_par_t[0].L_wafer_tot_w     = 2.1   ;// half wafer total width
  m_geom_par_t[0].L_wafer_tot_t     = 0.015 ;// half wafer total thickness  
  m_geom_par_t[0].L_wafer_act_l     = 3.65  ;// half wafer active length
  m_geom_par_t[0].L_wafer_act_w     = 2.0   ;// half wafer active width 
}
//_____________________________________________________________________________
void   St_sdm_Maker::InitCalibPar()
{
  m_cal_par  = new St_sdm_calib_par("sdm_calib_par",1);
  m_cal_par->SetNRows(1);
  sdm_calib_par_st *m_cal_par_t = m_cal_par->GetTable();

  m_cal_par_t[0].i_seed             = 111111;// random initialisation seed
  m_cal_par_t[0].barrel_ped         = 100000;// offset
  m_cal_par_t[0].wafer_sig          = 10000 ;// dispersion around the offset
  m_cal_par_t[0].alice_sig          = 5000  ;// dispersion of alice pedestal
  m_cal_par_t[0].strip_P_sig        = 50    ;// dispersion of strip P pedestal
  m_cal_par_t[0].strip_N_sig        = 50    ;// dispersion of strip N pedestal
  m_cal_par_t[0].strip_P_noise      = 1400   ;//(700) mean value of strip P noise
  m_cal_par_t[0].strip_N_noise      = 2200   ;//(1100) mean value of strip N noise
  m_cal_par_t[0].strip_P_noise_sig  = 50    ;//(50) dispersion of strip P noise around mean value
  m_cal_par_t[0].strip_N_noise_sig  = 70    ;//(70) dispersion of strip N noise around mean value
  m_cal_par_t[0].n_strip_P_factor   = 10.   ;// max noise multiplication factor for a noisy P strip
  m_cal_par_t[0].n_strip_N_factor   = 10.   ;// max noise multiplication factor for a noisy N strip
  m_cal_par_t[0].n_noisy_strip      = 0.    ;// percentage of noisy strip
}
//_____________________________________________________________________________
void   St_sdm_Maker::BuildCalibDB()
{
  Int_t   iSide          = 0 ;
  Int_t   lSsdPedestal   = 0 ;
  Int_t   lWaferPedestal = 0 ;
  Int_t   lStripNoise    = 0 ;
  Int_t   lStripPedestal = 0 ;
  Int_t   iBin           = 0 ;

  const Int_t nA128PerSide  = mSsdTotA128/mSsdTotPlane;
  const Int_t nStripPerA128 = mSsdTotStrip/mSsdTotA128;
  const Int_t nStripPerSide = mSsdTotStrip/mSsdTotPlane;
  
  sdm_calib_par_st *m_cal_par_t = m_cal_par->GetTable();

  Int_t *mSignalPar         = new Int_t[9];  // allocation...
  Float_t *mNoisyPar        = new Float_t[3];  // allocation...
  Int_t *mPedestalArray     = new Int_t[mSsdTotStrip];  // allocation...
  Int_t *mNoiseArray        = new Int_t[mSsdTotStrip];  // allocation...

  mSignalPar[0] = m_cal_par_t[0].barrel_ped;
  mSignalPar[1] = m_cal_par_t[0].wafer_sig;
  mSignalPar[2] = m_cal_par_t[0].alice_sig;
  mSignalPar[3] = m_cal_par_t[0].strip_P_sig;
  mSignalPar[4] = m_cal_par_t[0].strip_N_sig;
  mSignalPar[5] = m_cal_par_t[0].strip_P_noise;
  mSignalPar[6] = m_cal_par_t[0].strip_N_noise;
  mSignalPar[7] = m_cal_par_t[0].strip_P_noise_sig;
  mSignalPar[8] = m_cal_par_t[0].strip_N_noise_sig;

  mNoisyPar[0] = m_cal_par_t[0].n_strip_P_factor;
  mNoisyPar[1] = m_cal_par_t[0].n_strip_N_factor;
  mNoisyPar[2] = m_cal_par_t[0].n_noisy_strip;

  Int_t i = 0;
  Int_t j = 0;
  Int_t k = 0;

 
  for(i = 0 ; i < mSsdTotPlane ; i++)
    {
      iSide = (i%2 == 0) ?  0 : 1 ;
      lSsdPedestal = Int_t((mSignalPar[0])+m_DBRandom->Gaus(0.,Double_t(mSignalPar[1])));
      
      for(j = 0 ; j < nA128PerSide ; j++)
	{      
	  lWaferPedestal = lSsdPedestal + Int_t(m_DBRandom->Gaus(0.,mSignalPar[2])); 
	  for(k = 0 ; k < nStripPerA128 ; k++ )
	    {
	      switch(iSide)
		{
		case 0 :
		  lStripNoise = Int_t(mSignalPar[5]+(m_DBRandom->Gaus(0.,mSignalPar[7])));
		  lStripPedestal = lWaferPedestal + Int_t(m_DBRandom->Gaus(0.,mSignalPar[3]));
		  break;
		case 1 :      
		  lStripNoise = Int_t(mSignalPar[6]+(m_DBRandom->Gaus(0.,mSignalPar[8])));
		  lStripPedestal = lWaferPedestal + Int_t(m_DBRandom->Gaus(0.,mSignalPar[4]));
		  break;
		}
	      iBin = i*nStripPerSide + j*nStripPerA128 + k ;
	      mPedestalArray[iBin] = lStripPedestal;
	      mNoiseArray[iBin]    = lStripNoise; 
	    }
	}
    }
  
  Int_t nNoisyStrip = Int_t(mSsdTotStrip*m_cal_par_t[0].n_noisy_strip);
  Float_t factorP     = (m_cal_par_t[0].n_strip_P_factor)-1.;
  Float_t factorN     = (m_cal_par_t[0].n_strip_N_factor)-1.;
  Int_t *noisyTmp = new Int_t[mSsdTotStrip];  // allocation...

  for ( i = 0 ; i < mSsdTotStrip ; i++)
    {
     noisyTmp[i] = i ;
    }
  Int_t localSize = mSsdTotStrip;
  Int_t st = 0;
  Float_t fact = 0.;

  i = 0;
  while (i < nNoisyStrip)
    {
      st = Int_t(m_DBRandom->Rndm()*localSize);
      iSide = (Int_t(st/768))%2;
      switch (iSide)
	{
	case 0:
	  fact = 1.+Float_t(m_DBRandom->Rndm()*factorP);
	  mNoiseArray[noisyTmp[st]] = Int_t(mNoiseArray[noisyTmp[st]]*fact);
	  break;
	case 1:
	  fact = 1+Float_t(m_DBRandom->Rndm()*factorN);
	  mNoiseArray[noisyTmp[st]] = Int_t(mNoiseArray[noisyTmp[st]]*fact);
	  break;   
	}
      noisyTmp[st] = noisyTmp[localSize-1];
      localSize--;
      i++;
    }

  Int_t iStrip   = 0;

  St_sdm_calib_db *m_noise = new St_sdm_calib_db("sdm_calib_db",500000);             // allocation...
  m_noise->SetNRows(0);
  sdm_calib_db_st *m_noise_t = m_noise->GetTable();
  cout<<" noise Used Rows = "<<m_noise->GetNRows()<<endl;
  cout<<" noise Allocated Rows = "<<m_noise->GetTableSize()<<endl;

  for(i = 0 ; i < mSsdTotStrip ; i++ ){
    
    iStrip = ConvertStripId(i+1);
    
    m_noise_t[i].id_strip   = iStrip;
    m_noise_t[i].n_pedestal = mPedestalArray[i]; 
    m_noise_t[i].n_sigma    = mNoiseArray[i];
    m_noise->SetNRows(i+1);
  }

  TString *fCalibDBName = new TString("sdm_calib_db.root");
  TFile *m_CalibDBFile = new TFile(((*m_DBPath)+(*fCalibDBName)).Data(),"RECREATE");
  m_noise->Write();
  m_CalibDBFile->Close();

  delete[] noisyTmp;
  delete[] mSignalPar;
  delete[] mNoisyPar;
  delete[] mPedestalArray;
  delete[] mNoiseArray;
  delete fCalibDBName;
  delete m_noise;
  delete m_CalibDBFile;
}
//_____________________________________________________________________________
void   St_sdm_Maker::BuildConditionDB()
{
  sdm_condition_par_st *m_cond_par_t = m_cond_par->GetTable();

  Int_t *mSsdActiveLadder = new Int_t[mSsdTotLadder];
  Int_t *mSsdActivePlane  = new Int_t[mSsdTotPlane];
  Int_t *mSsdActiveA128   = new Int_t[mSsdTotA128];
  Int_t *mSsdActiveStrip  = new Int_t[mSsdTotStrip];
  
  Int_t i = 0 ;
  Int_t j = 0 ;
  for (i = 0 ; i < mSsdTotLadder ; i++)
    {

      mSsdActiveLadder[i] = Int_t(m_cond_par_t[0].N_active_ladder[i]);

    }
  for (i = 0 ; i < mSsdTotLadder ; i++ )
    {
      Int_t tmp_loc = 0;
      if (mSsdActiveLadder[i])
	{
	  tmp_loc     =  mSsdTotPlane/mSsdTotLadder;
	  for (j  = 0 ; j < tmp_loc ; j++) mSsdActivePlane[j+tmp_loc*i] = 1;
	  tmp_loc     =  mSsdTotA128/mSsdTotLadder;
	  for (j  = 0 ; j < tmp_loc ; j++) mSsdActiveA128[j+tmp_loc*i]  = 1;
	  tmp_loc     = mSsdTotStrip/mSsdTotLadder;
	  for (j  = 0 ; j < tmp_loc ; j++) mSsdActiveStrip[j+tmp_loc*i] = 1;
	}
      else
	{
	  tmp_loc     =  mSsdTotPlane/mSsdTotLadder;
	  for (j  = 0 ; j < tmp_loc ; j++) mSsdActivePlane[j+tmp_loc*i] = 0;
	  tmp_loc     =  mSsdTotA128/mSsdTotLadder;
	  for (j  = 0 ; j < tmp_loc ; j++) mSsdActiveA128[j+tmp_loc*i]  = 0;
	  tmp_loc     = mSsdTotStrip/mSsdTotLadder;
	  for (j  = 0 ; j < tmp_loc ; j++) mSsdActiveStrip[j+tmp_loc*i] = 0;
	}
    }

  Int_t nDeadWafer  = Int_t(mSsdTotWafer*m_cond_par_t[0].p_bad_wafer);
  Int_t nDeadA128   = Int_t(mSsdTotA128*m_cond_par_t[0].p_bad_alice); 
  Int_t nDeadStrip  = Int_t(mSsdTotStrip*m_cond_par_t[0].p_bad_strip);
  Int_t iBin = 0 ;
  Int_t *nTmpDeadStrip = new Int_t[mSsdTotStrip];
  Int_t LocalSize = 0;

  Int_t nStripPerA128 = mSsdTotStrip/mSsdTotA128;
  Int_t nStripPerWafer = mSsdTotStrip/mSsdTotWafer;

  Int_t ii = 0;
  Int_t st = 0;
  Int_t jj = 0;
  Int_t ch = 0;
  Int_t ss = 0;
  for (ii =0 ; ii < mSsdTotStrip ; ii++)
    {
      if (mSsdActiveStrip[ii]) 
	{
	  nTmpDeadStrip[LocalSize] = ii;
	  LocalSize++;
	}
    }
  ii = 0;
  while (ii < nDeadStrip)
    {
      st = Int_t (m_DBRandom->Rndm()*LocalSize);
      mSsdActiveStrip[nTmpDeadStrip[st]] = 0 ;
      nTmpDeadStrip[st] = nTmpDeadStrip[LocalSize - 1];
      nTmpDeadStrip[LocalSize - 1] = 0;
      LocalSize--;
      ii++;
    }
  ii = 0;
  while (ii < nDeadA128)
    {
      ch = Int_t(m_DBRandom->Rndm()*mSsdTotA128);
      if ( mSsdActiveA128[ch])
	{
	  for(jj = 0 ; jj <nStripPerA128 ; jj++)
	    { 
	      iBin = jj + ch*nStripPerA128; 
	      mSsdActiveStrip[iBin] = 0 ;
	    }
	  mSsdActiveA128[ch] = 0 ;
	  ii++;
	}
    }
  
  ii = 0; 
  while (ii < nDeadWafer)
    {
      ss = Int_t(m_DBRandom->Rndm()*mSsdTotWafer);
      if ((mSsdActivePlane[2*ss])||(mSsdActivePlane[2*ss+1]))
	{
	  for(jj = 0 ; jj < nStripPerWafer  ; jj++)
	    {
	      iBin = jj + ss*nStripPerWafer  ; 
	      mSsdActiveStrip[iBin] = 0 ;

	    }
	  mSsdActivePlane[2*ss]   = 0 ; // p-side plane 
	  mSsdActivePlane[2*ss+1] = 0 ; // n-side plane
	}
      ii++;
    }
  
  Int_t iStrip       = 0;
  
  St_sdm_condition_db *m_condition = new St_sdm_condition_db("sdm_condition_db",500000);
  m_condition->SetNRows(0);
  sdm_condition_db_st *m_condition_t = m_condition->GetTable();

  for(jj = 0 ; jj < mSsdTotStrip ; jj++ ){
    
    iStrip = ConvertStripId(jj+1);
    
    m_condition_t[jj].id_strip   = iStrip;
    m_condition_t[jj].is_active  = mSsdActiveStrip[jj]; 
    m_condition->SetNRows(jj+1);
  }
  
  TString *fCondDBName = new TString("sdm_condition_db.root");
  TFile *m_CondDBFile = new TFile (((*m_DBPath)+(*fCondDBName)).Data(),"RECREATE");
  m_condition->Write();
  m_CondDBFile->Close();
 
  delete[] nTmpDeadStrip;
  delete[] mSsdActiveLadder;
  delete[] mSsdActivePlane;
  delete[] mSsdActiveA128;
  delete[] mSsdActiveStrip;
  delete fCondDBName;
  delete m_condition;
  delete m_CondDBFile;
}
//__________________________________________________________
Int_t St_sdm_Maker::WaferNumbToIdWafer(Int_t wafer_numb)
{
  Int_t nWaferPerLadder = mSsdTotWafer/mSsdTotLadder;
  Int_t iLadder = 1+Int_t(wafer_numb/nWaferPerLadder);
  Int_t iWafer = wafer_numb-((iLadder-1)*nWaferPerLadder)+1;
  return mSsdLayer*1000 + iWafer*100 + iLadder;  
}
//__________________________________________________________
Int_t St_sdm_Maker::ConvertStripId(Int_t st)
{
  Int_t iWafer         = 0;
  Int_t iSide          = 0 ;
  Int_t idStrip        = 0;
  Int_t nStripPerWafer = Int_t(mSsdTotStrip/mSsdTotWafer);
  
  iWafer  = Int_t((st-1)/nStripPerWafer);
  idStrip = st - iWafer*nStripPerWafer; 
  iWafer  = WaferNumbToIdWafer(iWafer);
  iSide   = (idStrip <= (nStripPerWafer/2)) ? 0 : 1 ; 

  if(iSide) idStrip -= (nStripPerWafer/2); 
  idStrip = 10000*(10*idStrip+iSide)+iWafer ;
  return idStrip; 
}
