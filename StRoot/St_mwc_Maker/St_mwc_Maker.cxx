// $Id: St_mwc_Maker.cxx,v 1.16 2000/03/28 20:27:56 fine Exp $
// $Log: St_mwc_Maker.cxx,v $
// Revision 1.16  2000/03/28 20:27:56  fine
// Adjuested to ROOT 2.24
//
// Revision 1.15  2000/01/26 22:05:13  ward
// replaced for(int iii with int iii for(iii
//
// Revision 1.14  1999/07/15 13:58:17  perev
// cleanup
//
// Revision 1.13  1999/06/09 19:56:31  druss
// Added histograms : # of wire hits, # of sectors hit, # wires hit as a function of eta and phi
//
// Revision 1.12  1999/03/12 21:47:36  perev
// New maker schema
//
// Revision 1.11  1999/03/12 15:44:01  perev
// New maker schema
//
// Revision 1.10  1999/03/04 18:07:55  druss
// If no g2t_mwc_hit table, just return (instead of return error)
//
// Revision 1.9  1999/03/03 04:12:13  fisyak
// replace kStErr to kStWarn
//
// Revision 1.8  1999/03/02 18:33:33  druss
// Freed up wasted space by lowering number of Rows in the tables from
// 384 to 96.  Also removed the cor table that is not used in this module.
//
// Revision 1.7  1999/02/26 18:09:02  kathy
// made hist limits go neg
//
// Revision 1.6  1999/02/26 17:25:18  kathy
// fix histograms
//
// Revision 1.5  1999/02/24 13:57:43  druss
// removed 2d histograms and replaced them with 1d histograms
//
// Revision 1.4  1999/02/19 18:22:58  druss
// init routine now uses parameter files from StRoot/params
// included a few histograms
//
// Revision 1.3  1999/02/11 19:46:33  druss
// MWC maker for mdc2.  Removed print statements, set parameters
//
// Revision 1.2  1999/01/29 22:55:19  druss
// corrected bugs, included new parameters from mwc parameter table
//
// Revision 1.1  1999/01/14 19:11:01  druss
// root Maker definitions/header for mwc
//
// Revision 1.7  1998/10/31 00:25:45  fisyak
// Makers take care about branches
//
// Revision 1.6  1998/10/06 18:00:29  perev
// cleanup
//
// Revision 1.5  1998/10/02 13:46:08  fine
// DataSet->DataSetIter
//
// Revision 1.4  1998/08/14 15:25:58  fisyak
// add options
//
// Revision 1.3  1998/08/10 02:32:07  fisyak
// Clean up
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_mwc_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "St_mwc_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "mwc/St_mwg_Module.h"
#include "mwc/St_mws_Module.h"
#include "mwc/St_mwu_Module.h"
#include "TH1.h"
#include "TH2.h"
ClassImp(St_mwc_Maker)

//_____________________________________________________________________________
St_mwc_Maker::St_mwc_Maker(const char *name):StMaker(name){

}
//_____________________________________________________________________________
St_mwc_Maker::~St_mwc_Maker(){
}
//_____________________________________________________________________________
Int_t St_mwc_Maker::Init(){

// Read Parameter tables

   St_DataSetIter params(GetDataBase("params/mwc/mwcpars"));
   m_geom = (St_mwc_geo  *) params("geom");
   m_cal  = (St_mwc_cal  *) params("cal");
   m_mpar = (St_mwc_mpar *) params("mpar");

   mwc_mpar_st *partable = m_mpar->GetTable();
   partable->de_thresh_in      = 0;
   partable->de_thresh_out     = 0;
   partable->el_noise_width    = 0;
   partable->min_ion    = 0;
   m_mpar->AddAt(partable,0);

// Create Histograms 

   m_px = new TH1F("MwcHitPx","MWC: px",100,-4.0,4.0);
   m_py = new TH1F("MwcHitPy","MWC: py",100,-4.0,4.0);
   m_pz = new TH1F("MwcHitPz","MWC: pz",100,-4.0,4.0);
   m_nWiresHit = new TH1F("MwcNumberWiresHit","MWC: WiresHit",101,-0.5,100.5);
   m_nSectorsHit = new TH1F("MwcNumSecHit","MWC: SectorsHit",101,-0.5,100.5);
   m_EtaPhi = new TH2F("MwcEtaPhi","MWC: Eta vs Phi",24,0.5,24.5,4,0.5,4.5);

   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_mwc_Maker::Make(){

//  PrintInfo();

// Create Empty tables for us

   St_mwc_mevent *mevent = new St_mwc_mevent("mevent",96);
   St_mwc_sector *sector = new St_mwc_sector("sector",96);
   St_mwc_raw    *raw    = new St_mwc_raw("raw",96);
//   the cor table is not implemented
//     St_mwc_cor    *cor    = new St_mwc_cor("cor",384);

   m_DataSet->Add(mevent);
   m_DataSet->Add(sector);
   m_DataSet->Add(raw);
//     m_DataSet->Add(cor);

// Read in Geant Tables

   St_DataSetIter geant(GetDataSet("geant"));
   St_g2t_mwc_hit *g2t_mwc_hit = (St_g2t_mwc_hit *) geant("g2t_mwc_hit");

   if (!g2t_mwc_hit) {return kStOK;}
   if (!m_geom)      {printf("m_geom does not exist\n")     ;return kStWarn;}
   if (!m_mpar)      {printf("m_mpar does not exist\n")     ;return kStWarn;}
   if (!mevent)      {printf("mevent does not exist\n")     ;return kStWarn;}
   if (!sector)      {printf("sector does not exist\n")     ;return kStWarn;}
   if (!raw)         {printf("raw does not exist\n")        ;return kStWarn;}

   Int_t mwc_result = mws(
                          g2t_mwc_hit,
                          m_geom,
                          m_mpar,
                          mevent,
                          sector,
                          raw);
   if (mwc_result != kSTAFCV_OK)
   {
      printf("**** Problems with mwc ****\n");
      return kStWarn;
   }

   mwc_sector_st *sec = sector->GetTable();
   mwc_raw_st    *rw  = raw->GetTable();
   /*   for (int ii=0;ii<=95;ii++)
     {
       if ( (rw+ii)->count )
	 printf("raw sector: %2d count %3d phi %2d eta %d nhit %2d tot_hit "
		"%3d de %f\n",(rw+ii)->sector,(rw+ii)->count,
		(sec+ii)->iphi,(sec+ii)->ieta,(sec+ii)->nhit,
		(sec+ii)->tot_hit,(sec+ii)->de);
     }*/
   g2t_mwc_hit_st *hitTable = g2t_mwc_hit->GetTable();
   float px,py,pz,x,y;
   int iNHit;
   int nSectorsHit=0;
   int iii;
   for (iii=0;iii<96;iii++)
     {
       iNHit = (sec+iii)->nhit;
       if (iNHit) nSectorsHit++;
       m_nWiresHit -> Fill( float(iNHit) );
       m_EtaPhi -> Fill(float(int(iii/4)+1),float(iii%4)+1,float(iNHit) );
     }
   m_nSectorsHit -> Fill(float(nSectorsHit));
   Int_t nok = g2t_mwc_hit->GetNRows();
   for (iii=0;iii<nok;iii++)
     {
       x  = (hitTable+iii)->x[0];
       y  = (hitTable+iii)->x[1];
       px = (hitTable+iii)->p[0]; 
       py = (hitTable+iii)->p[1]; 
       pz = (hitTable+iii)->p[2];
       m_px ->Fill(px);
       m_py ->Fill(py);
       m_pz ->Fill(pz);
     }
   
   return kStOK;
}
//_____________________________________________________________________________
     
