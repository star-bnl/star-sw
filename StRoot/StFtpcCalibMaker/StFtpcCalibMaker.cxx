// $Id: StFtpcCalibMaker.cxx,v 1.3 2006/04/04 14:34:39 jcs Exp $
//
// $Log: StFtpcCalibMaker.cxx,v $
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

#include "StMessMgr.h"
#include "StFtpcCalibMaker.h"
#include "StFtpcLaserCalib.hh"
#include "StFtpcLaserTrafo.hh"

#include "StChain.h"
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
  //cout<<"GetRunInfo: j->Run.run = "<<j->Run.run<<" j->Run.date = "<<j->Run.date<<" j->Run.time = "<<j->Run.time<<" j->Run.micropertimebin = "<<j->Run.micropertimebin<<" j->Run.deltapW = "<<j->Run.deltapW<<" j->Run.deltapE = "<<j->Run.deltapE<<endl;
  date = j->Run.date;
  time = j->Run.time;
  micropertime = j->Run.micropertimebin;
  deltapW      = j->Run.deltapW;
  deltapE      = j->Run.deltapE;
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
     gMessMgr->Info() << "StFtpcCalibMaker: flavor set to ffp10kv"<<endm;
  }
  else if ( mbfield > 0.2 ) {
     SetFlavor("hfp10kv","ftpcVDrift");
     SetFlavor("hfp10kv","ftpcdVDriftdP");
     SetFlavor("hfp10kv","ftpcDeflection");
     SetFlavor("hfp10kv","ftpcdDeflectiondP");
     gMessMgr->Info() << "StFtpcCalibMaker: flavor set to hfp10kv"<<endm;
  }
  else if ( mbfield > -0.2 ) {
     SetFlavor("zf10kv","ftpcVDrift");
     SetFlavor("zf10kv","ftpcdVDriftdP");
     SetFlavor("zf10kv","ftpcDeflection");
     SetFlavor("zf10kv","ftpcdDeflectiondP");
     gMessMgr->Info() << "StFtpcCalibMaker: flavor set to zf10kv"<<endm;
  }
  else if ( mbfield > -0.8 ) {
     SetFlavor("hfn10kv","ftpcVDrift");
     SetFlavor("hfn10kv","ftpcdVDriftdP");
     SetFlavor("hfn10kv","ftpcDeflection");
     SetFlavor("hfn10kv","ftpcdDeflectiondP");
     gMessMgr->Info() << "StFtpcCalibMaker: flavor set to hfn10kv"<<endm;
  }
  else {
     SetFlavor("ffn10kv","ftpcVDrift");
     SetFlavor("ffn10kv","ftpcdVDriftdP");
     SetFlavor("ffn10kv","ftpcDeflection");
     SetFlavor("ffn10kv","ftpcdDeflectiondP");
     gMessMgr->Info() << "StFtpcCalibMaker: flavor set to ffn10kv"<<endm;
  }
  
  ftpc_db  = GetDataBase("ftpc");
  if (!ftpc_db) {
     gMessMgr->Warning() << "StFtpcCalibMaker::DbInit  run parameter database StarDb/ftpc not found"<<endm;
     return kStWarn;
  }
  St_DataSetIter local(ftpc_db);

  m_clusterpars  = (St_ftpcClusterPars *)local("ftpcClusterPars");

  St_DataSet *ftpc_geometry_db = GetDataBase("Geometry/ftpc");
  St_DataSetIter dblocal_geometry(ftpc_geometry_db);
  
  m_dimensions = (St_ftpcDimensions *)dblocal_geometry("ftpcDimensions");
  m_padrow_z   = (St_ftpcPadrowZ  *)dblocal_geometry("ftpcPadrowZ");

  St_DataSet *ftpc_calibrations_db = GetDataBase("Calibrations/ftpc");
  St_DataSetIter dblocal_calibrations(ftpc_calibrations_db);

  m_gas= (St_ftpcGas *)dblocal_calibrations("ftpcGas");
  m_efield     = (St_ftpcEField *)dblocal_calibrations("ftpcEField");
  m_vdrift     = (St_ftpcVDrift *)dblocal_calibrations("ftpcVDrift");
  m_deflection = (St_ftpcDeflection *)dblocal_calibrations("ftpcDeflection");
  m_dvdriftdp     = (St_ftpcdVDriftdP *)dblocal_calibrations("ftpcdVDriftdP");
  m_ddeflectiondp = (St_ftpcdDeflectiondP *)dblocal_calibrations("ftpcdDeflectiondP");
  m_driftfield = (St_ftpcDriftField *)dblocal_calibrations("ftpcDriftField");
  m_electronics = (St_ftpcElectronics *)dblocal_calibrations("ftpcElectronics");

  // create parameter reader
  paramReader = new StFtpcParamReader(m_clusterpars);

  //cout<<"Nach  parameter reader"<<endl;

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

  //cout<<"Nach db reader"<<endl;
 
  return kStOK;

}
//_____________________________________________________________________________

/**
 *
 *  Analyze laser runs
 *
 */

void StFtpcCalibMaker::DoLaserCalib(TString filename,int ftpc, int lsec, int straight, int gfit, int minz, int maxz, int minrad, int maxrad, char* t0, char* gas, float mbfield)
{
if (ftpc == 1) cout<<"StFtpcCalibMaker::DoLaserCalib entered for FTPC West"<<endl;
if (ftpc == 2) cout<<"StFtpcCalibMaker::DoLaserCalib entered for FTPC East"<<endl;
  Int_t i=0;
  Int_t ntracksold=0;
  Int_t neventold=2;

  if (ftpc == 1) deltap = deltapW;
  if (ftpc == 2) deltap = deltapE;

  // Laser t0 = 1.0
  tZero = 1.0;

  if (atof(t0)!=0 || atof(gas)!=0)
    {
      trafo=new StFtpcLaserTrafo(dbReader,paramReader,atof(t0),atof(gas),micropertime,deltap,mbfield,tZero);
      if (trafo->calcpadtrans())
        cout<<"calcpadtrans done !"<<endl;
      else 
	cout<<"ERROR calcpadtrans !"<<endl;
      //if (trafo->padtrans(1,1,170,100));
      //else
      //cout<<"ERROR padtrans !"<<endl;
      //delete trafo;
    }

  cout<<"DoLaserCalib() ..."<<endl;

  // for (int step=0;step<10;step++)

  cout<<endl;
  cout<<"Reading Magnetic-Field maps..."<<endl;
  cout<<endl;
  StMagUtilities *m_magf=new StMagUtilities(kMapped,mbfield,0);
  // analoges Problem bei Magnetfeld 0 shift nicht machen in Track !!!
  cout<<endl;

  StFtpcLaserCalib *l=new StFtpcLaserCalib(ftpc,lsec,straight,gfit,minz,maxz,minrad,maxrad,atof(t0),atof(gas),trafo,m_magf);
 
  l->Init(filename);
  l->MakeOutput(filename,t0,gas);
 
  Int_t maxentries=l->btcluster->GetEntries();
  cout<<endl;
  cout<<"Process Cluster-on-Track-Tree with "<<maxentries<<" clusters..."<<endl; 
  cout<<endl;

  for (int k=0;k<=maxentries;k++)
    {
      if (k%(maxentries/10)==0 && k>0) cout<<"#"<<flush;//cout<<k<<" cluster on tracks processed"<<endl;
      l->GetTreeEntry(k);
      
      //calculate hardware sector
      int hardsec = 6*(int)((l->tcluster.row-1)/2) + l->tcluster.sec;
      
      //cout<<" hardsec "<< hardsec<<endl;

      // activate following code line to debug with 2 events
      //if (l->tevent.nevent>2) break;

      //cout<<"l->tevent.run = "<<l->tevent.run<<" l->tevent.nevent = "<<l->tevent.nevent<<endl;
      //cout<<"l->Run.run = "<<l->Run.run<<" l->Run.micropertimebin = "<<l->Run.micropertimebin<<" l->Run.deltapW = "<<l->Run.deltapW<<" l->Run.deltapE = "<<l->Run.deltapE<<endl;

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

  cout<<endl;
  cout<<"Laser calibration done :-) !"<<endl;
  cout<<endl;
}

//_____________________________________________________________________________

/**
 *
 *  Use the time step from data runs to check to
 *
 */

void StFtpcCalibMaker::DoT0Calib(TString filename, char* t0, char* gas, float mbfield)
{

  cout<<"DoT0Calib entered with filename "<<filename<<" t0 "<<t0<<" gas "<<gas<<" mbfield "<<mbfield<<endl;

  tZero = dbReader->tZero();
  cout<<"tZero = "<<tZero<<endl;

  if (atof(t0)!=0 || atof(gas)!=0)
  {
      // FTPC West

      deltap = deltapW;
 

      trafo = new StFtpcLaserTrafo(dbReader,paramReader,atof(t0),atof(gas),micropertime,deltap,mbfield,tZero);

      if (trafo->calcpadtrans())
         cout<<"calcpadtrans (west) done !"<<endl;
      else
         cout<<"ERROR calcpadtrans west !"<<endl;

      // FTPC East

      deltap = deltapE;

      trafo2 = new StFtpcLaserTrafo(dbReader,paramReader,atof(t0),atof(gas),micropertime,deltap,mbfield,tZero);

       if (trafo2->calcpadtrans())
         cout<<"calcpadtrans (east) done !"<<endl;
      else
         cout<<"ERROR calcpadtrans east!"<<endl;

  }


  cout<<"DoT0Calib() ..."<<endl;

  HistInit(4,filename,t0,gas);

  StFtpcLaser *l=new StFtpcLaser();
 
  l->Init(filename);

  Int_t maxentries=(int) l->bcluster->GetEntries();

  cout<<endl;
  cout<<"Process Cluster-Tree with "<<maxentries<<" clusters..."<<endl;
  cout<<endl;

  Float_t x,y,rad;//,phi;

  for (int k=0;k<=maxentries;k++)
    {

      if (k%(maxentries/10)==0 && k>0) cout<<"#"<<flush;
      //cout<<k<<" cluster on tracks processed"<<endl;
    
      l->GetClusterTreeEntry(k);

      if (atof(t0)!=0 || atof(gas)!=0)
        {
          if (l->cluster.sec<31)
            trafo->padtrans(l->cluster.row,l->cluster.sec,l->cluster.timepos,l->cluster.padpos,&x,&y);
          else
            trafo2->padtrans(l->cluster.row,l->cluster.sec,l->cluster.timepos,l->cluster.padpos,&x,&y);
        } 
      else
        {
           x=l->hit.x;
           y=l->hit.y;
        }

      rad=sqrt(x*x+y*y);

      if (l->cluster.sec<31)
        {
          hradwall->Fill(rad);
          hradw->Fill(rad);
          htimew->Fill(l->cluster.timepos);
        }
      else
        {
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

  cout<<endl;
  cout<<"T0 calibration done :-) !"<<endl;
  cout<<endl;
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
  //cout<<endl;cout<<"HistInit() ..."<<endl;cout<<endl;

  cout<<"Store histograms in ROOT-file : "<<outname<<endl;

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

  // DEBUG :
  //cout<<endl;cout<<"MakePs ..."<<endl;cout<<endl;
  cout<<endl;cout<<endl;
  cout<<"Make PS-file : "<<outname<<endl;
  cout<<endl;

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
