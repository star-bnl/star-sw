// $Id: StFtpcCalibMaker.cxx,v 1.14 2017/03/02 18:28:38 jeromel Exp $
//
// $Log: StFtpcCalibMaker.cxx,v $
// Revision 1.14  2017/03/02 18:28:38  jeromel
// No changes, just empty lines cleanup
//
// Revision 1.13  2009/12/09 14:41:30  jcs
// delta_t0 and delta_gas can now both = 0
// new space point calculation always necessary since reconstruction done with data t0
//
// Revision 1.12  2009/11/18 12:09:50  jcs
// add USE_LOCAL_DRIFTMAP instructions
//
// Revision 1.11  2009/10/14 15:59:55  jcs
// changes to be able to vary the gas temperature in addition to varying t0 and
// gas composition
//
// Revision 1.10  2009/08/04 08:42:09  jcs
// The 'perfect' gain table and adjustAverageWest = adjustAverageEast = 0.0
// are used for laser run calibration
//
// Revision 1.9  2008/05/15 22:39:47  jcs
// re-activate helix fit
//
// Revision 1.8  2008/05/13 19:14:58  jcs
// get  Laser t0 from Calibrations_ftpc/ftpcElectronics offline database table
// clean up comments
//
// Revision 1.7  2008/04/11 17:00:55  nav
// *** empty log message ***
//
// Revision 1.6  2007/04/28 17:56:08  perev
// Redundant StChain.h removed
//
// Revision 1.5  2007/01/22 13:08:15  jcs
// replace cout with LOG
//
// Revision 1.4  2007/01/19 08:53:54  jcs
// replace gMessMgr with LOG
//
// Revision 1.3  2006/04/04 14:34:39  jcs
// replace assert with a warning message and return kStWarn
//
// Revision 1.2  2006/04/04 10:57:03  jcs
// Fix memory leak
//
// Revision 1.1  2006/03/13 19:59:56  jcs
// commit initial version of the FTPC calibration maker
//

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StFtpcCalibMaker class for Makers                                  //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
#include <fstream>
#include <cmath>
#include "PhysicalConstants.h"

#include "StMessMgr.h"
#include "StFtpcCalibMaker.h"
#include "StFtpcLaserCalib.hh"
#include "StFtpcLaserTrafo.hh"

#include "St_DataSetIter.h"
#include "St_DataSet.h"
#include "StMessMgr.h"

#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TObjArray.h"

ClassImp(StFtpcCalibMaker)

//_____________________________________________________________________________


StFtpcCalibMaker::StFtpcCalibMaker(const char *name):
  StMaker(name)
{

}

//_____________________________________________________________________________

StFtpcCalibMaker::~StFtpcCalibMaker(){

}

//_____________________________________________________________________________

void StFtpcCalibMaker::GetRunInfo(TString filename){

  StFtpcLaser *j = new StFtpcLaser();

  j->Init(filename);
  j->GetTreeEntry(0);
  LOG_DEBUG<<"StFtpcCalibMaker::GetRunInfo  j->Run.run = "<<j->Run.run<<" j->Run.date = "<<j->Run.date<<" j->Run.time = "<<j->Run.time<<" j->Run.micropertimebin = "<<j->Run.micropertimebin<<" j->Run.normalizedNowPressure = "<<j->Run.normalizedNowPressure <<" j->Run.standardPressure = "<<j->Run.standardPressure<<" j->Run.baseTemperature = "<<j->Run.baseTemperature<<" j->Run.gasTemperatureWest = "<<j->Run.gasTemperatureWest<<" j->Run.gasTemperatureEast = "<<j->Run.gasTemperatureEast<<endm;
  run  = j->Run.run;
  date = j->Run.date;
  time = j->Run.time;
  micropertime = j->Run.micropertimebin;
  normalizedNowPressure = j->Run.normalizedNowPressure;
  standardPressure = j->Run.standardPressure;
  baseTemperature = j->Run.baseTemperature;
  gasTemperatureWest = j->Run.gasTemperatureWest;
  gasTemperatureEast = j->Run.gasTemperatureEast;
  delete j;  // destructor matches second constructor only
  return;

}

//_____________________________________________________________________________

/**
 *
 *  Offline database initialization
 *
 */

Int_t StFtpcCalibMaker::DbInit(float mbfield)
{
  St_DataSet *ftpc_db = NULL;

  if ( mbfield > 0.8 ) {
     SetFlavor("ffp10kv","ftpcVDrift");
     SetFlavor("ffp10kv","ftpcdVDriftdP");
     SetFlavor("ffp10kv","ftpcDeflection");
     SetFlavor("ffp10kv","ftpcdDeflectiondP");
     LOG_INFO << "StFtpcCalibMaker::DbInit - flavor set to ffp10kv"<<endm;
  }
  else if ( mbfield > 0.2 ) {
     SetFlavor("hfp10kv","ftpcVDrift");
     SetFlavor("hfp10kv","ftpcdVDriftdP");
     SetFlavor("hfp10kv","ftpcDeflection");
     SetFlavor("hfp10kv","ftpcdDeflectiondP");
     LOG_INFO << "StFtpcCalibMaker::DbInit - flavor set to hfp10kv"<<endm;
  }
  else if ( mbfield > -0.2 ) {
     SetFlavor("zf10kv","ftpcVDrift");
     SetFlavor("zf10kv","ftpcdVDriftdP");
     SetFlavor("zf10kv","ftpcDeflection");
     SetFlavor("zf10kv","ftpcdDeflectiondP");
     LOG_INFO << "StFtpcCalibMaker::DbInit - flavor set to zf10kv"<<endm;
  }
  else if ( mbfield > -0.8 ) {
     SetFlavor("hfn10kv","ftpcVDrift");
     SetFlavor("hfn10kv","ftpcdVDriftdP");
     SetFlavor("hfn10kv","ftpcDeflection");
     SetFlavor("hfn10kv","ftpcdDeflectiondP");
     LOG_INFO << "StFtpcCalibMaker::DbInit - flavor set to hfn10kv"<<endm;
  }
  else {
     SetFlavor("ffn10kv","ftpcVDrift");
     SetFlavor("ffn10kv","ftpcdVDriftdP");
     SetFlavor("ffn10kv","ftpcDeflection");
     SetFlavor("ffn10kv","ftpcdDeflectiondP");
     LOG_INFO << "StFtpcCalibMaker::DbInit - flavor set to ffn10kv"<<endm;
  }

  ftpc_db  = GetDataBase("ftpc");
  if (!ftpc_db) {
     LOG_WARN << "StFtpcCalibMaker::DbInit - run parameter database StarDb/ftpc not found"<<endm;
     return kStWarn;
  }
  St_DataSetIter local(ftpc_db);

  m_clusterpars  = (St_ftpcClusterPars *)local("ftpcClusterPars");

  // USE_LOCAL_DRIFTMAP:
  //                    To use the FTPC drift map tables in $PWD/StarDb instead of those
  //                    in the MySQL offline database, uncomment the following 4 lines of code
  //m_vdrift     = (St_ftpcVDrift *)local("ftpcVDrift");
  //m_deflection = (St_ftpcDeflection *)local("ftpcDeflection");
  //m_dvdriftdp     = (St_ftpcdVDriftdP *)local("ftpcdVDriftdP");
  //m_ddeflectiondp = (St_ftpcdDeflectiondP *)local("ftpcdDeflectiondP");

  St_DataSet *ftpc_geometry_db = GetDataBase("Geometry/ftpc");
  St_DataSetIter dblocal_geometry(ftpc_geometry_db);

  m_dimensions = (St_ftpcDimensions *)dblocal_geometry("ftpcDimensions");
  m_padrow_z   = (St_ftpcPadrowZ  *)dblocal_geometry("ftpcPadrowZ");

  St_DataSet *ftpc_calibrations_db = GetDataBase("Calibrations/ftpc");
  St_DataSetIter dblocal_calibrations(ftpc_calibrations_db);

  m_gas= (St_ftpcGas *)dblocal_calibrations("ftpcGas");
  m_efield     = (St_ftpcEField *)dblocal_calibrations("ftpcEField");

  // USE_LOCAL_DRIFTMAP:
  //                    To use the FTPC drift map tables in $PWD/StarDb instead of those
  //                    in the MySQL offline database, comment out the following 4 lines of code
  m_vdrift     = (St_ftpcVDrift *)dblocal_calibrations("ftpcVDrift");
  m_deflection = (St_ftpcDeflection *)dblocal_calibrations("ftpcDeflection");
  m_dvdriftdp     = (St_ftpcdVDriftdP *)dblocal_calibrations("ftpcdVDriftdP");
  m_ddeflectiondp = (St_ftpcdDeflectiondP *)dblocal_calibrations("ftpcdDeflectiondP");

  m_driftfield = (St_ftpcDriftField *)dblocal_calibrations("ftpcDriftField");
  m_electronics = (St_ftpcElectronics *)dblocal_calibrations("ftpcElectronics");

  // create parameter reader
  paramReader = new StFtpcParamReader(m_clusterpars);

  //LOG_DEBUG<<"StFtpcCalibMaker::DbInit -  parameter reader created"<<endm;

  // create db reader
  dbReader = new StFtpcDbReader(m_dimensions,
				m_padrow_z,
				m_efield,
				m_vdrift,
				m_deflection,
				m_dvdriftdp,
				m_ddeflectiondp,
                                m_electronics,
				m_gas,
				m_driftfield);

  //LOG_DEBUG<<"StFtpcCalibMaker::DbInit - Ftpc db reader created"<<endm;

  return kStOK;

}
//_____________________________________________________________________________

/**
 *
 *  Analyze laser runs
 *
 */

void StFtpcCalibMaker::DoLaserCalib(TString filename,int ftpc, int lsec, int straight, int gfit, int minz, int maxz, int minrad, int maxrad, char* t0, char* gas, float gastemp, float mbfield)
{
if (ftpc == 1) LOG_INFO<<"StFtpcCalibMaker::DoLaserCalib - entered for FTPC West"<<endm;
if (ftpc == 2) LOG_INFO<<"StFtpcCalibMaker::DoLaserCalib - entered for FTPC East"<<endm;
  Int_t i=0;
  Int_t ntracksold=0;
  Int_t neventold=2;

  if (ftpc == 1){
      Float_t adjustedAirPressureWest = normalizedNowPressure*((baseTemperature+STP_Temperature)/(gasTemperatureWest+gastemp+STP_Temperature));
      deltap = adjustedAirPressureWest - standardPressure;
      LOG_INFO << "d_TempWest = " << gastemp << " adjustedAirPressureWest = " << adjustedAirPressureWest << " deltap West = " << deltap << endm;
  }
  if (ftpc == 2) {
      Float_t adjustedAirPressureEast = normalizedNowPressure*((baseTemperature+STP_Temperature)/(gasTemperatureEast+gastemp+STP_Temperature));
      deltap = adjustedAirPressureEast - standardPressure;
      LOG_INFO << "d_TempEast = " << gastemp << " adjustedAirPressureEast = " << adjustedAirPressureEast << " deltap East = " << deltap << endm;
  }

  Bool_t laserRun = kTRUE;
  dbReader->setLaserRun(laserRun);

 // get  Laser t0 from Calibrations_ftpc/ftpcElectronics offline database table
  tZero = dbReader->laserTZero();
  LOG_INFO<<"StFtpcCalibMaker::DoLaserCalib() - laserTZero = "<<tZero<<endm;

  LOG_INFO<<"StFtpcCalibMaker::DoLaserCalib() ..."<<endm;

  // for (int step=0;step<10;step++)

  LOG_INFO<<" "<<endm;
  LOG_INFO<<"StFtpcCalibMaker::DoLaserCalib() - Reading Magnetic-Field maps..."<<endm;
  LOG_INFO<<" "<<endm;
//  StMagUtilities *m_magf=new StMagUtilities(kMapped,mbfield,0);
  StarMagField *m_magf=new StarMagField(StarMagField::kMapped, mbfield, kTRUE);
  // analoges Problem bei Magnetfeld 0 shift nicht machen in Track !!!

  // new space point calculation only necessary if atof(t0)!=0 || atof(gas)!=0
  //if (atof(t0)!=0 || atof(gas)!=0) {
  // new space point calculation always necessary since reconstruction done with data t0
      trafo=new StFtpcLaserTrafo(dbReader,paramReader,atof(t0),atof(gas),micropertime,deltap,mbfield,tZero);
      if (trafo->calcpadtrans()) {
        LOG_INFO<<"StFtpcCalibMaker::DoLaserCalib - calcpadtrans done !"<<endm;
      }
      else{
	LOG_FATAL<<"StFtpcCalibMaker::DoLaserCalib - fatal error in calcpadtrans !"<<endm;
        delete trafo;
        return;
      }
  //}

  StFtpcLaserCalib *l=new StFtpcLaserCalib(ftpc,lsec,straight,gfit,minz,maxz,minrad,maxrad,atof(t0),atof(gas),gastemp,trafo,m_magf);

  l->Init(filename);
  l->MakeOutput(filename,t0,gas,gastemp);

  Int_t maxentries=l->btcluster->GetEntries();
  LOG_INFO<<" "<<endm;
  LOG_INFO<<"StFtpcCalibMaker::DoLaserCalib() - processing Cluster-on-Track-Tree with "<<maxentries<<" clusters... please be patient"<<endm;
  LOG_INFO<<" "<<endm;

  for (int k=0;k<=maxentries;k++)
    {
      if (k%(maxentries/10)==0 && k>0) {
        //LOG_DEBUG<<"StFtpcCalibMaker::DoLaserCalib() - "<<k<<" cluster on tracks processed"<<endm;
      }
      l->GetTreeEntry(k);

      //calculate hardware sector
      int hardsec = 6*(int)((l->tcluster.row-1)/2) + l->tcluster.sec;

      //LOG_DEBUG<<"StFtpcCalibMaker::DoLaserCalib() -  hardsec "<< hardsec<<endm;

      // activate following code line to debug with 2 events
      //if (l->tevent.nevent>2) break;

      //LOG_DEBUG<<"StFtpcCalibMaker::DoLaserCalib() - l->tevent.run = "<<l->tevent.run<<" l->tevent.nevent = "<<l->tevent.nevent<<endm;
      //LOG_DEBUG<<"StFtpcCalibMaker::DoLaserCalib() - l->Run.run = "<<l->Run.run<<" l->Run.micropertimebin = "<<l->Run.micropertimebin<<" l->Run.deltapW = "<<l->Run.deltapW<<" l->Run.deltapE = "<<l->Run.deltapE<<endm;

      if (l->tevent.nevent==neventold)
	{
	  if (l->tcluster.ntracks==ntracksold)
	    {
	      l->fillarray(l->thit.x,l->thit.y,l->thit.z,l->thit.ex,l->thit.ey,i,hardsec,l->tcluster.padpos,l->tcluster.padpossigma,l->tcluster.sec,l->tcluster.row,l->tcluster.timepos,l->tcluster.padlength,l->tcluster.timelength,l->tcluster.peakheight,l->tcluster.charge);
	      i++;
	    }
	  else
	    {
	      if (l->laser_straight(l->radius,i)==l->STRAIGHT || l->STRAIGHT==3)
		if (l->laser_fit(i)==0) {}

	      i=0;
	      l->fillarray(l->thit.x,l->thit.y,l->thit.z,l->thit.ex,l->thit.ey,i,hardsec,l->tcluster.padpos,l->tcluster.padpossigma,l->tcluster.sec,l->tcluster.row,l->tcluster.timepos,l->tcluster.padlength,l->tcluster.timelength,l->tcluster.peakheight,l->tcluster.charge);
	      i++;
	    }
	}
      else
	{
	  if (l->laser_straight(l->radius,i)==l->STRAIGHT || l->STRAIGHT==3)
	    if (l->laser_fit(i)==0) {}

	  i=0;
	  l->fillarray(l->thit.x,l->thit.y,l->thit.z,l->thit.ex,l->thit.ey,i,hardsec,l->tcluster.padpos,l->tcluster.padpossigma,l->tcluster.sec,l->tcluster.row,l->tcluster.timepos,l->tcluster.padlength,l->tcluster.timelength,l->tcluster.peakheight,l->tcluster.charge);
	  i++;
	}

      neventold=l->tevent.nevent;
      ntracksold=l->tcluster.ntracks;
    }

  if (l->laser_straight(l->radius,i)==l->STRAIGHT || l->STRAIGHT==3)
    if (l->laser_fit(i)==0) {}

  l->MakePs();
  l->PositionLog();

  l->analyse_defl();

  delete l;

  if (t0!=0)
    delete trafo;

  delete m_magf;

  LOG_INFO<<" "<<endm;
  LOG_INFO<<"Laser calibration done :-) !"<<endm;
  LOG_INFO<<" "<<endm;
}

//_____________________________________________________________________________

/**
 *
 *  Use the time step from data runs to check t0
 *
 */

void StFtpcCalibMaker::DoT0Calib(TString filename, char* t0, char* gas, float mbfield)
{


  Bool_t laserRun = kFALSE;
  dbReader->setLaserRun(laserRun);

  tZero = dbReader->tZero();
  LOG_INFO<<"StFtpcCalibMaker::DoT0Calib entered with filename "<<filename<<" t0 "<<t0<<" gas "<<gas<<" mbfield "<<mbfield<<" and tZero = "<<tZero<<endm;

  // new space point calculation only necessary if atof(t0)!=0 || atof(gas)!=0
  // new space point calculation always necessary since reconstruction done with data t0
  //if (atof(t0)!=0 || atof(gas)!=0) {
      // FTPC West

      deltap = deltapW;
      LOG_INFO<<"StFtpcCalibMaker::DoT0Calib deltap = deltapW = "<<deltap<<endm;


      trafo = new StFtpcLaserTrafo(dbReader,paramReader,atof(t0),atof(gas),micropertime,deltap,mbfield,tZero);

      if (trafo->calcpadtrans()) {
         LOG_INFO<<"StFtpcCalibMaker::DoT0Calib - calcpadtrans (west) done !"<<endm;
      }
      else {
         LOG_FATAL<<"StFtpcCalibMaker::DoT0Calib - fatal error in  calcpadtrans west !"<<endm;
         delete trafo;
         return;
      }

      // FTPC East

      deltap = deltapE;
      LOG_INFO<<"StFtpcCalibMaker::DoT0Calib deltap = deltapE = "<<deltap<<endm;

      trafo2 = new StFtpcLaserTrafo(dbReader,paramReader,atof(t0),atof(gas),micropertime,deltap,mbfield,tZero);

       if (trafo2->calcpadtrans()) {
         LOG_INFO<<"StFtpcCalibMaker::DoT0Calib - calcpadtrans (east) done !"<<endm;
       }
      else {
         LOG_FATAL<<"StFtpcCalibMaker::DoT0Calib - fatal error in  calcpadtrans east !"<<endm;
         delete trafo2;
         return;
      }

  //}


  LOG_INFO<<"StFtpcCalibMaker::DoT0Calib() ..."<<endm;

  HistInit(4,filename,t0,gas);

  StFtpcLaser *l=new StFtpcLaser();

  l->Init(filename);

  Int_t maxentries=(int) l->bcluster->GetEntries();

  LOG_INFO<<" "<<endm;
  LOG_INFO<<"StFtpcCalibMaker::DoT0Calib() - processing Cluster-Tree with "<<maxentries<<" clusters... please be patient"<<endm;
  LOG_INFO<<" "<<endm;

  Float_t x,y,rad;//,phi;

  for (int k=0;k<=maxentries;k++) {

      if (k%(maxentries/10)==0 && k>0) {
         //LOG_DEBUG<<k<<" cluster on tracks processed"<<endm;
      }

      l->GetClusterTreeEntry(k);

  // new space point calculation always necessary since reconstruction done with data t0
      //if (atof(t0)!=0 || atof(gas)!=0) {
          if (l->cluster.sec<31)
            trafo->padtrans(l->cluster.row,l->cluster.sec,l->cluster.timepos,l->cluster.padpos,&x,&y);
          else
            trafo2->padtrans(l->cluster.row,l->cluster.sec,l->cluster.timepos,l->cluster.padpos,&x,&y);
      //}
      //else {
      //     x=l->hit.x;
      //     y=l->hit.y;
      //}

      rad=sqrt(x*x+y*y);

      if (l->cluster.sec<31) {
          hradwall->Fill(rad);
          hradw->Fill(rad);
          htimew->Fill(l->cluster.timepos);
      }
      else {
          hradeall->Fill(rad);
          hrade->Fill(rad);
          htimee->Fill(l->cluster.timepos);
      }

  }

  delete l;

  if (t0!=0)
    {
      delete trafo;delete trafo2;
    }

  MakeT0Ps(4,filename,t0,gas);

  // save histogram file
  anaf->Write();
  anaf->Close();

  LOG_INFO<<" "<<endm;
  LOG_INFO<<"T0 calibration done :-) !"<<endm;
  LOG_INFO<<" "<<endm;
}

//_____________________________________________________________________________

/**
 *
 *   Initialize histograms
 *
 */

void StFtpcCalibMaker::HistInit(int nradbins,TString fname, char* t0, char* gas)
{

  TString outname=fname;
  outname +="_";
  outname +=t0;
  outname +="_";
  outname +=gas;
  outname +="_t0.root";

  // DEBUG :
  //LOG_DEBUG<<" "<<endm;LOG_DEBUG<<"HistInit() ..."<<endm;LOG_DEBUG<<" "<<endm;

  LOG_INFO<<"StFtpcCalibMaker::HistInit - Store histograms in ROOT-file : "<<outname<<endm;

  anaf=new TFile(outname,"RECREATE");

  hradeall=new TH1F("rad_east_all","radius FTPC East",nradbins*31,0.5,31.5);
  hradwall=new TH1F("rad_west_all","radius FTPC West",nradbins*31,0.5,31.5);

  hrade=new TH1F("rad_east","radius FTPC East",nradbins*7,5,12);
  hradw=new TH1F("rad_west","radius FTPC West",nradbins*7,5,12);

  htimee=new TH1F("time_east","Timepos. FTPC East",45,140,185);
  htimew=new TH1F("time_west","Timepos. FTPC West",45,140,185);

}

//_____________________________________________________________________________

/**
 *
 *  Draw t0 histograms
 *
 */

void StFtpcCalibMaker::MakeT0Ps(int nradbins,TString psname, char* t0, char* gas)
{

  TString outname=psname;
   outname +="_";
  outname +=t0;
  outname +="_";
  outname +=gas;
  outname +="_t0.ps";

  LOG_INFO<<" "<<endl;
  LOG_INFO<<"StFtpcCalibMaker::MakeT0Ps - make ps file "<<outname<<endm;
  LOG_INFO<<" "<<endm;

  TCanvas *c1 = new TCanvas("c1","ps",200,10,700,500);

  TPostScript *fps=new TPostScript(outname,112);

  fps->NewPage();

  c1->Divide(2,2);

  TLine *inner=new TLine();

  c1->cd(1);
  hradwall->Draw();inner=new TLine(7.8,0,7.8,hradwall->GetMaximum());inner->SetLineColor(2);inner->Draw();
  c1->cd(2);
  hradeall->Draw();inner=new TLine(7.8,0,7.8,hradeall->GetMaximum());inner->SetLineColor(2);inner->Draw();
  c1->cd(3);
  hradw->Draw();inner=new TLine(7.8,0,7.8,hradw->GetMaximum());inner->SetLineColor(2);inner->Draw();
  c1->cd(4);
  hrade->Draw();inner=new TLine(7.8,0,7.8,hrade->GetMaximum());inner->SetLineColor(2);inner->Draw();

  c1->Update();

  //fps->NewPage();

  fps->NewPage();

  c1->cd(1);

  hradw->Scale(1/hradw->Integral(0,nradbins*7));
  hrade->Scale(1/hrade->Integral(0,nradbins*7));

  hrade->Draw();hradw->SetLineColor(3);hradw->Draw("same");
  inner=new TLine(7.8,0,7.8,hrade->GetMaximum());inner->SetLineColor(2);inner->Draw();

  c1->cd(2);
  htimee->Scale(1/htimee->GetEntries());htimew->Scale(1/htimew->GetEntries());
  htimee->DrawCopy();htimew->SetLineColor(3);htimew->DrawCopy("same");

  //c1->cd(4);
  //htimee->DrawCopy();

  c1->Update();

  fps->Close();

  delete c1;
}
