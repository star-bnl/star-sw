// $Id: St_l3t_Maker.cxx,v 1.11 1999/08/12 15:42:42 yepes Exp $
// $Log: St_l3t_Maker.cxx,v $
// Revision 1.11  1999/08/12 15:42:42  yepes
// Change printing
//
// Revision 1.10  1999/08/11 20:02:14  yepes
// *** empty log message ***
//
// Revision 1.10 1999/08/10 yepes
// set some new parameters for ftfTpc
//
// Revision 1.9  1999/07/15 13:58:14  perev
// cleanup
//
// Revision 1.8  1999/05/21 21:15:59  yepes
// Fixint problem with no Hits
//
// Revision 1.7  1999/05/05 18:37:21  yepes
// *** empty log message ***
//
// Revision 1.6  1999/05/05 18:30:16  yepes
// define maximum number of tracks as NHits/20
//
// Revision 1.5  1999/04/17 22:37:34  fisyak
// remove annoying printf
//
// Revision 1.4  1999/03/12 15:27:32  perev
// New maker schema
//
// Revision 1.3  1999/02/26 17:25:11  kathy
// fix histograms
//
// Revision 1.2  1999/02/19 14:39:31  fisyak
// New version from Pablo, tpc safe
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_l3t_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include "St_l3t_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_XDFFile.h"
#include "tpc/St_tpt_Module.h"
#include "l3/St_ftfTpc_Module.h"
//#include "tpc/St_tpt_residuals_Module.h"
//#include "tpc/St_tte_track_Module.h"
//#include "tpc/St_tde_new_Module.h"
//#include "tpc/St_tte_Module.h"
#include "TH1.h"
ClassImp(St_l3t_Maker)
  
  //_____________________________________________________________________________
St_l3t_Maker::St_l3t_Maker(const char *name):
    StMaker(name)
{
}
//_____________________________________________________________________________
St_l3t_Maker::~St_l3t_Maker(){
}
//_____________________________________________________________________________
Int_t St_l3t_Maker::Init(){
  // Create tables
  St_DataSetIter       local(GetInputDB("params/l3"));

// l3t parameters
  m_sl3TpcPara = new St_sl3TpcPara("sl3Para",1);
  m_sl3TpcPara->SetNRows(1);
  sl3TpcPara_st   *para  = m_sl3TpcPara->GetTable();
 
  para[0].infoLevel   =  0 ;
  para[0].FirstSector =   1 ;
  para[0].LastSector  =  24 ;
  para[0].InnerMostRow       =   1  ;
  para[0].OuterMostRow       =  45  ;
  para[0].NPrimaryLoops      =   1  ;
  para[0].NSecondaryLoops    =   0  ;
  para[0].ErrorScaleSz       =   5. ;
  para[0].ErrorScaleXy       =   1. ;
  para[0].Phimin             =   0. ;
  para[0].Phimax             = 360. ;
  para[0].Etamin             = -2.2 ;
  para[0].Etamax             =  2.2 ;
  para[0].PhiSlices          =  20  ;
  para[0].EtaSlices          =  80  ;
  para[0].BField             = 0.5  ;
  para[0].SFitSz             =   1  ;
  para[0].SPhiClosed         =   0  ;
  para[0].SDPhiLimit         = 0.05 ;
  para[0].SDEtaLimit         = 0.05 ;
  para[0].SChi2Cut           = 50.0 ;
  para[0].SGoodChi2          = 10.0 ;
  para[0].SGoodDistance      = 100. ;
  para[0].SChi2TrackCut      = 30.0 ;
  para[0].SMinimumHitsPerSegment =   2 ;
  para[0].SMinimumHitsPerTrack   =   5 ;
  para[0].SMaxSearchPadrowsTrack =   2 ;
  para[0].SMaxSearchPadrowsSegment = 2 ;
  para[0].MergePrimaries         =   1 ;
  para[0].NumberOfPsiSlices      =  40 ;
  para[0].NumberOfTanLSlices     =  40 ;
  para[0].MinSlicePsi            =  0.0 ;
  para[0].MaxSlicePsi            = 360. ;
  para[0].MinSliceTanL           = -2.0 ;
  para[0].MaxSliceTanL           =  2.0 ;
  para[0].SDPsiMaxMerge          =  0.03 ;
  para[0].SDTanlMaxMerge         =  0.01 ;
  para[0].numberTrackingSlices   = 1 ;
  para[0].TrackingSlices[0]      = -0.4 ;
  para[0].TrackingSlices[1]      =  2.4 ;
//
  para[0].sectorPhiMin[0]=45.;
  para[0].sectorPhiMin[1]=15.;
  para[0].sectorPhiMin[2]= 0.;
  para[0].sectorPhiMin[3]=315.;
  para[0].sectorPhiMin[4]=285.;
  para[0].sectorPhiMin[5]=255.;
  para[0].sectorPhiMin[6]=225.;
  para[0].sectorPhiMin[7]=195.;
  para[0].sectorPhiMin[8]=165.;
  para[0].sectorPhiMin[9]=135.;
  para[0].sectorPhiMin[10]=105.;
  para[0].sectorPhiMin[11]=75.;
  para[0].sectorPhiMin[12]=105.;
  para[0].sectorPhiMin[13]=135.;
  para[0].sectorPhiMin[14]=165.;
  para[0].sectorPhiMin[15]=195.;
  para[0].sectorPhiMin[16]=225.;
  para[0].sectorPhiMin[17]=255.;
  para[0].sectorPhiMin[18]=285.;
  para[0].sectorPhiMin[19]=315.;
  para[0].sectorPhiMin[20]=0.;
  para[0].sectorPhiMin[21]=15.;
  para[0].sectorPhiMin[22]=45.;
  para[0].sectorPhiMin[23]=75.;
//
  para[0].sectorPhiMax[0]=75.;
  para[0].sectorPhiMax[1]=45.;
  para[0].sectorPhiMax[2]=30.;
  para[0].sectorPhiMax[3]=345.;
  para[0].sectorPhiMax[4]=315.;
  para[0].sectorPhiMax[5]=285.;
  para[0].sectorPhiMax[6]=255.;
  para[0].sectorPhiMax[7]=225.;
  para[0].sectorPhiMax[8]=195.;
  para[0].sectorPhiMax[9]=165.;
  para[0].sectorPhiMax[10]=135.;
  para[0].sectorPhiMax[11]=105.;
  para[0].sectorPhiMax[12]=135.;
  para[0].sectorPhiMax[13]=165.;
  para[0].sectorPhiMax[14]=195.;
  para[0].sectorPhiMax[15]=225.;
  para[0].sectorPhiMax[16]=255.;
  para[0].sectorPhiMax[17]=285.;
  para[0].sectorPhiMax[18]=315.;
  para[0].sectorPhiMax[19]=345.;
  para[0].sectorPhiMax[20]= 30.;
  para[0].sectorPhiMax[21]= 45.;
  para[0].sectorPhiMax[22]= 75.;
  para[0].sectorPhiMax[23]=105.;

  for ( int i = 0 ; i < 24 ; i++ ) {
      para[0].sectorPhiShift[i] = 0. ;
      if ( i < 12 ) { 
         para[0].sectorEtaMin[i] = 0. ;
         para[0].sectorEtaMax[i] = 2. ;
      }
      else {
         para[0].sectorEtaMin[i] = -2. ;
         para[0].sectorEtaMax[i] =  0. ;
     }
  }
  para[0].sectorPhiShift[ 2] = 15. ;
  para[0].sectorPhiShift[20] = 15. ;
//
//
//   Add to run/param
//
  local.Add( m_sl3TpcPara ) ;
//
  m_l3_hits_on_track = new TH1F("L3tL3trackNumHits","Number of hits on reconstructed tracks",50,.5,50.5);
  m_l3_azimuth       = new TH1F("L3tL3trackPhi","Azimuthal distribution of tracks",60,0.,360.0);
  m_l3_tan_dip       = new TH1F("L3tL3trackTanDip","Distribution of the dip angle",100,-1.5,1.5);
  m_l3_r0            = new TH1F("L3tL3trackR0","Radius for the first point",100,50.0,200);
//
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_l3t_Maker::Make(){
  int iMake = kStOK;
  Int_t maxNofTracks = 20000; 

  St_DataSet *tpc_data =  GetDataSet("tpc_hits");
  if (tpc_data) {// Clusters exist -> do tracking
    St_DataSetIter next(tpc_data);
    St_tcl_tphit   *tphit = (St_tcl_tphit     *) next("tphit");
    Int_t nHits = tphit->GetNRows();
    St_tcl_tphit   *l3hit = new St_tcl_tphit("l3Hit",nHits);
    m_DataSet->Add(l3hit);
//
//    Copy tpc table to l3
//
    *l3hit = *tphit ;
//
    maxNofTracks = nHits / 10 ;
    if ( maxNofTracks < 1 ) maxNofTracks = 1 ;
    St_tpt_track   *track = new St_tpt_track("l3Track",maxNofTracks); 
    m_DataSet->Add(track);
    St_sl3Monitor  *mon   = new St_sl3Monitor("sl3Monitor",1000); 
    m_DataSet->Add(mon);

    sl3TpcPara_st   *para  = m_sl3TpcPara->GetTable();
    Int_t l3out = ftfTpc ( m_sl3TpcPara, l3hit, track, mon ) ;
    printf ( "l3 Tracker Done\n" ) ;
    if (l3out != kSTAFCV_OK) iMake = kStWarn;
  }

  MakeHistograms(); // tracking histograms
  return iMake;
}
void St_l3t_Maker::MakeHistograms() {

//		Get the table:
  St_tpt_track *tpr = (St_tpt_track *)m_DataSet->Find("l3Track");
  if (!tpr) return;
  tpt_track_st *r = tpr->GetTable();
  for(Int_t i=0; i<tpr->GetNRows();i++,r++){
    Int_t flag    = r->flag;
    Float_t rnrec = r->nrec;
//Unused    Float_t rnfit = r->nfit;
    //    printf(" iflag %d \n", flag ) ;

    if(flag<=0) continue;

    m_l3_hits_on_track->Fill(rnrec);
    m_l3_azimuth->Fill(r->psi);
    m_l3_tan_dip->Fill(r->tanl);
    m_l3_r0->Fill(r->r0);
  }
}

//_____________________________________________________________________________

