// $Id: StFtpcClusterMaker.cxx,v 1.3 1999/12/02 13:20:58 hummler Exp $
// $Log: StFtpcClusterMaker.cxx,v $
// Revision 1.3  1999/12/02 13:20:58  hummler
// Move cluster processing from maker to cluster finder class.
// (Preparations for new raw data implementation.)
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StFtpcClusterMaker class for Makers                                  //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include "StFtpcClusterMaker.h"
#include "StFtpcClusterFinder.hh"
#include "StFtpcCluster.hh"
#include "StFtpcFastSimu.hh"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "TH1.h"
#include "TH2.h"

#include "tables/St_fcl_fppoint_Table.h"
#include "tables/St_fcl_ftpcndx_Table.h"
#include "tables/St_fcl_ftpcsqndx_Table.h"
#include "tables/St_fcl_ftpcadc_Table.h"

#include "tables/St_fcl_padtrans_Table.h"
#include "tables/St_fcl_ampslope_Table.h"
#include "tables/St_fcl_ampoff_Table.h"
#include "tables/St_fcl_timeoff_Table.h"
#include "tables/St_fcl_det_Table.h"
#include "tables/St_fcl_zrow_Table.h"
#include "tables/St_g2t_vertex_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_ftp_hit_Table.h"
#include "tables/St_ffs_gaspar_Table.h"
#include "tables/St_ffs_fspar_Table.h"
#include "tables/St_ffs_gepoint_Table.h"

ClassImp(StFtpcClusterMaker)

  //_____________________________________________________________________________
StFtpcClusterMaker::StFtpcClusterMaker(const char *name):
StMaker(name),
    m_ampoff(0),
    m_ampslope(0),
    m_timeoff(0),
    m_padtrans(0),
    m_det(0),
    m_zrow(0),
    m_fspar(0),
    m_gaspar(0)
{
  drawinit=kFALSE;
}
//_____________________________________________________________________________
StFtpcClusterMaker::~StFtpcClusterMaker(){
}
//_____________________________________________________________________________
Int_t StFtpcClusterMaker::Init(){

  St_DataSet *ftpc = GetDataBase("params/ftpc");
  assert(ftpc);
  St_DataSetIter       local(ftpc);

  m_ampoff     = (St_fcl_ampoff   *)local("fclpars/ampoff"  );
  m_ampslope   = (St_fcl_ampslope *)local("fclpars/ampslope");
  m_timeoff    = (St_fcl_timeoff  *)local("fclpars/timeoff" );
  m_padtrans   = (St_fcl_padtrans *)local("fclpars/padtrans");
  m_det        = (St_fcl_det      *)local("fclpars/det"     );
  m_zrow       = (St_fcl_zrow     *)local("fclpars/zrow"    );
  m_fspar      = (St_ffs_fspar    *)local("ffspars/fspar"   );
  m_gaspar     = (St_ffs_gaspar   *)local("ffspars/gaspar"  );

  ffs_gaspar_st *ffs_gaspar = m_gaspar->GetTable();
  ffs_gaspar->sig_rad[0] = 800.00;
  ffs_gaspar->sig_rad[1] = 0.00;
  ffs_gaspar->sig_rad[2] = 0.00;
  ffs_gaspar->sig_rad[3] = 0.00;
  ffs_gaspar->sig_azi[0] = 2000.00;
  ffs_gaspar->sig_azi[1] = 0.00;
  ffs_gaspar->sig_azi[2] = 0.00;
  ffs_gaspar->sig_azi[3] = 0.00;

// 		Create Histograms
  m_flags      = new TH1F("fcl_flags"	,"FTPC cluster finder flags"	,7,0.,8.);
  m_row        = new TH1F("fcl_row"	,"FTPC rows"			,20,1.,21.);
  m_sector     = new TH1F("fcl_sector"	,"FTPC sectors"			,6,1.,7.);
  m_pads       = new TH1F("fcl_pads"	,"FTPC pads"			,80,1.,161.);
  m_timebins   = new TH1F("fcl_timebins","FTPC timebins"		,100,1.,257.);
  m_row_sector = new TH2F("fcl_row_sector","FTPC(fcl) row vs. sector"	,20,1.,21.,6,1.,7.);
  m_npad_nbin  = new TH2F("fcl_pad_bin"	,"FTPC(fcl) pad vs. timebin"	,80,1.,161.,100,1.,257.);

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StFtpcClusterMaker::Make()
{
  int iMake=kStOK;
  St_DataSet *raw = GetDataSet("ftpc_raw");
  if (raw) {
    //			FCL
    St_DataSetIter get(raw);
    
    St_fcl_ftpcndx   *fcl_ftpcndx   = (St_fcl_ftpcndx*  )get("fcl_ftpcndx");
    St_fcl_ftpcsqndx *fcl_ftpcsqndx = (St_fcl_ftpcsqndx*)get("fcl_ftpcsqndx");
    St_fcl_ftpcadc   *fcl_ftpcadc   = (St_fcl_ftpcadc*  )get("fcl_ftpcadc");
    if (fcl_ftpcndx&&fcl_ftpcsqndx&&fcl_ftpcadc) { 
      
      St_fcl_fppoint *fcl_fppoint = new St_fcl_fppoint("fcl_fppoint",150000);
      m_DataSet->Add(fcl_fppoint);
      if(Debug()) cout<<"start running fcl"<<endl;
            
      StFtpcClusterFinder *fcl = new StFtpcClusterFinder();

      StFtpcCluster *clusters=fcl->search(m_det->GetTable(),
					  m_padtrans->GetTable(),
					  m_zrow->GetTable(),
					  m_ampoff->GetTable(),
					  m_ampslope->GetTable(),
					  m_timeoff->GetTable(),
					  fcl_ftpcsqndx->GetTable(),
					  fcl_ftpcadc->GetTable(),
					  fcl_fppoint,
					  m_padtrans->GetNRows(),
					  m_ampslope->GetNRows(),
					  m_ampoff->GetNRows(),
					  m_timeoff->GetNRows(),
					  fcl_ftpcsqndx->GetNRows());
      if (clusters == NULL)
	{
	  iMake=kStWarn;
	}
      delete fcl;
    }
    else {
      
      cout <<"StFtpcClusterMaker: Tables are not found:" 
	   << " fcl_ftpcndx   = " << fcl_ftpcndx 
	   << " fcl_ftpcsqndx = " << fcl_ftpcsqndx 
	   << " fcl_ftpcadc   = " << fcl_ftpcadc << endl;
    }
  }
  else { 
    
    //                      FFS
    St_DataSet *gea = GetDataSet("geant");
    St_DataSetIter geant(gea);
    St_g2t_vertex  *g2t_vertex  = (St_g2t_vertex *) geant("g2t_vertex");
    St_g2t_track   *g2t_track   = (St_g2t_track *)   geant("g2t_track");
    St_g2t_ftp_hit *g2t_ftp_hit = (St_g2t_ftp_hit *) geant("g2t_ftp_hit");
    if (g2t_track && g2t_ftp_hit){
      St_ffs_gepoint *ffs_gepoint = new St_ffs_gepoint("ffs_gepoint",150000);
      m_DataSet->Add(ffs_gepoint);
      St_fcl_fppoint *fcl_fppoint = new St_fcl_fppoint("fcl_fppoint",150000);
      m_DataSet->Add(fcl_fppoint);
      
      if(Debug()) cout<<"NO RAW DATA AVAILABLE - start running StFtpcFastSimu"<<endl;
      
      Int_t numHit=g2t_ftp_hit->GetNRows();
      Int_t numTrack=g2t_track->GetNRows();
      Int_t numGepoint=ffs_gepoint->GetNRows();
      Int_t maxGepoint=ffs_gepoint->GetHeader()->maxlen;
      Int_t numFppoint=fcl_fppoint->GetNRows();
      Int_t maxFppoint=fcl_fppoint->GetHeader()->maxlen;
      StFtpcFastSimu *ffs = new StFtpcFastSimu(g2t_ftp_hit->GetTable(), 
					       &numHit,
					       g2t_track->GetTable(), 
					       &numTrack,
					       g2t_vertex->GetTable(),
					       m_fspar->GetTable(),
					       m_gaspar->GetTable(),
					       ffs_gepoint->GetTable(),
					       &numGepoint, maxGepoint,
					       fcl_fppoint->GetTable(),
					       &numFppoint, maxFppoint);
      ffs_gepoint->SetNRows(numGepoint);				      
      fcl_fppoint->SetNRows(numFppoint);				      
      if(Debug())cout<<"finished running StFtpcFastSimu"<<endl;
      delete ffs;
    }
  }
  
  MakeHistograms(); // FTPC cluster finder histograms
  return iMake;
}
//_____________________________________________________________________________
void StFtpcClusterMaker::MakeHistograms() 
{

 // cout<<"*** NOW MAKING HISTOGRAMS FOR fcl ***"<<endl;

  // Create an iterator
  St_DataSetIter fcl_points(m_DataSet);

  //Get the table
  St_fcl_fppoint *ppointh;
  ppointh = (St_fcl_fppoint *) fcl_points.Find("fcl_fppoint");
  if (! ppointh) 	return;
  fcl_fppoint_st *r = ppointh->GetTable();
  for (Int_t i=0; i<ppointh->GetNRows();i++,r++) {
    Int_t flag = r->flags;
    if (flag > 0) {
      Int_t bin = 6;
      for (Int_t twofac=32; twofac>0; twofac=twofac/2,bin--) {
        Int_t nbit = flag/twofac;
        if (nbit != 1) 	continue;
        m_flags->Fill((float)bin);
        flag = flag - nbit*twofac;        
      }//end loop twofac
    }//endif flag

    Float_t nrow = r->row;
    m_row->Fill(nrow);
    Float_t nsec = r->sector;
    m_sector->Fill(nsec);
    m_row_sector->Fill(nrow,nsec);
    Float_t npad = r->n_pads;
    m_pads->Fill(npad);
    Float_t nbin = r->n_bins;
    m_timebins->Fill(nbin);
    m_npad_nbin->Fill(npad,nbin);
  }//end rows loop 
}
                                   

