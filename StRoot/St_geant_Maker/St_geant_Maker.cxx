//  St_geant_Maker.cxx,v 1.37 1999/04/19 06:29:30 nevski Exp 
// $Id: St_geant_Maker.cxx,v 1.78 2002/11/28 02:35:53 jeromel Exp $
// $Log: St_geant_Maker.cxx,v $
// Revision 1.78  2002/11/28 02:35:53  jeromel
// Minor correction
//
// Revision 1.77  2002/11/27 21:54:32  potekhin
// Added new naming convention in ESM
//
// Revision 1.76  2002/11/01 03:17:41  fine
// the previous version has been restored. No need of the special flag
//
// Revision 1.74  2002/10/16 20:39:23  kopytin
// Added code to read out BBC GSTAR tables. Needed by StBbcSimulationMaker
//
// Revision 1.72  2002/06/17 16:12:43  perev
// fix wrong geant time
//
// Revision 1.71  2002/04/14 21:57:09  perev
// Obsolete StBroadcast
//
// Revision 1.70  2002/03/12 21:22:38  fisyak
// Set only one StEvtHddr as default option (due to Embedding)
//
// Revision 1.69  2001/11/18 00:58:14  perev
//
// Revision 1.68  2001/07/06 17:34:24  nevski
// type fixed
//
// Revision 1.67  2001/07/03 23:37:25  nevski
// forward pion detector added
//
// Revision 1.66  2001/07/03 15:51:48  nevski
// phmd added
//
// Revision 1.65  2001/06/01 03:03:57  perev
// overloaded GetDataSet -> FindDataSet
//
// Revision 1.64  2000/12/19 18:35:11  fisyak
// make proper allocation for cgnam
//
// Revision 1.63  2000/07/19 16:57:34  fisyak
// Protection against double resetting the same run no. (Sasha)
//
// Revision 1.62  2000/06/23 16:52:12  fisyak
// Add filling of Run/Event no./Type from geant
//
// Revision 1.61  2000/03/26 02:43:22  fine
//  adjusted to ROOT 2.24
//
// Revision 1.60  2000/03/03 22:00:53  nevski
// protection against bad track number
//
// Revision 1.59  2000/03/03 20:53:48  nevski
// add protection against corrupted Itrac
//
// Revision 1.58  2000/02/29 22:25:52  lasiuk
// added FREO and QUAR to Rich hits
//
// Revision 1.57  2000/02/03 19:34:40  fisyak
// Clean up St_geant_Maker::Init, move its parameters to ctor
//
// Revision 1.56  2000/02/03 16:14:39  fisyak
// Add Kathy's histograms
//
// Revision 1.55  2000/02/02 21:21:19  fisyak
// Hack for CC5
//
// Revision 1.54  2000/01/23 19:20:53  nevski
// pseudo-doc
//
// Revision 1.53  2000/01/14 23:43:54  fisyak
// Add missing defines
//
// Revision 1.52  2000/01/04 21:51:11  fisyak
// Move TGeant3 to root4star
//
// Revision 1.51  1999/12/07 15:44:25  fisyak
// Add geane, new TGeant3 from Alice
//
// Revision 1.50  1999/11/13 17:30:05  fine
// scope for i within for loop fixed
//
// Revision 1.49  1999/11/13 02:40:55  fisyak
// Add gclose
//
// Revision 1.48  1999/11/11 05:16:30  fine
// GetDataSet method has been introduced to build GEANT geometry on fly
//
// Revision 1.47  1999/11/06 23:05:01  fisyak
// fix chars
//
// Revision 1.46  1999/10/20 19:18:17  nevski
// g2t_event table filled
//
// Revision 1.45  1999/09/24 01:23:42  fisyak
// Reduced Include Path
//
// Revision 1.44  1999/07/14 16:47:44  fisyak
// Set protection against empty event
//
// Revision 1.43  1999/07/09 02:18:03  fisyak
// Add Skip
//
// Revision 1.42  1999/07/09 01:15:48  fisyak
// Remove non printing character from generator type
//
// Revision 1.41  1999/07/03 22:40:11  fine
// St_geant_Maker::Work - workaround of LINUX compiler problem
//
// Revision 1.40  1999/04/30 15:17:03  perev
// SetOutput added to announce Geometry exists
//
// Revision 1.39  1999/04/29 19:29:27  nevski
// SetInputFile returns status
//
// Revision 1.38  1999/04/20 21:40:17  nevski
// all shapes are going via Victors hash
//
// Revision 1.37  1999/04/19 06:29:30  nevski
// update of user parameter extraction
//
// Revision 1.36  1999/04/19 06:25:35  nevski
// update of user parameter extraction
//
// Revision 1.35  1999/04/15 20:36:40  fine
// St_geant::Work() was void becomes TVolume *
//
// Revision 1.34  1999/04/12 23:17:11  fine
// Unique postion ID has been introduced
//
// Revision 1.33  1999/04/09 23:52:48  nevski
// checking 3 volume parameters now
//
// Revision 1.32  1999/04/08 00:39:08  fine
// Work metod - workaround for ROOT bug PCON definition
//
// Revision 1.31  1999/04/07 12:59:45  fine
// Fixed bug for PCON and PGON shapes
//
// Revision 1.30  1999/04/06 19:40:08  nevski
// variable size volumes
//
// Revision 1.29  1999/03/22 14:45:23  nevski
// geometry tree corrected
//
// Revision 1.28  1999/03/20 22:43:05  perev
// Do(trig)
//
// Revision 1.27  1999/03/11 00:15:22  perev
// St_geant_Maker in new maker schema
//
// Revision 1.26  1999/03/04 19:32:16  nevski
// esm/eem corrected
//
// Revision 1.25  1999/02/24 17:12:27  fine
// St_Table::New has been activated
//
// Revision 1.24  1999/02/23 18:59:50  nevski
// SVT 4th layer added to svt hit table
//
// Revision 1.23  1999/02/22 23:55:57  fine
// St_geant_Maker::rootmaptable is prepaed to use St_TableNew(), not activated yet
//
// Revision 1.22  1999/02/22 20:51:25  fisyak
// Mismatch between ctb/tof
//
// Revision 1.21  1999/02/22 19:27:20  fisyak
// add gtrigi and gtigc
//
// Revision 1.20  1999/02/20 20:23:45  fisyak
// Fix Aeast
//
// Revision 1.18  1999/02/19 14:41:00  fisyak
// Set kIsNotOwn Bit for geometry tables
//
// Revision 1.17  1999/02/18 15:44:47  fisyak
// Cleanup warinings
//
// Revision 1.16  1999/02/17 22:42:07  fisyak
// fix Linux parameters
//
// Revision 1.15  1999/02/17 15:55:38  fisyak
// Add GSTAR to ROOT geometry tables transformation
//
// Revision 1.14  1999/02/16 18:15:45  fisyak
// Check in the latest updates to fix them
//
// Revision 1.13  1999/02/12 17:57:01  nevski
// particle table
//
// Revision 1.12  1999/02/12 14:18:27  nevski
// merging 2 mods
//
// Revision 1.5  1999/01/10 20:37:31  fisyak
// Give access to Zebra
//
// Revision 1.4  1999/01/05 01:37:02  fisyak
// Intermeidate version with TVolume
//
// Revision 1.3  1999/01/03 20:56:35  fisyak
// Remove St_geom_Maker
//
// Revision 1.7  1998/12/25 21:02:13  nevski
// Add Set/Get method
//
// Revision 1.6  1998/12/17 14:38:00  fisyak
// Change default to no Higz window
//
// Revision 1.5  1998/12/16 20:56:24  fisyak
// Add gstar to ROOT
//
// Revision 1.4  1998/12/12 00:21:15  fisyak
// Remove gstar for the moment
//
// Revision 1.3  1998/12/12 00:18:00  fisyak
// Remove gstar for the moment
//
// Revision 1.2  1998/12/04 19:36:47  fisyak
// Add Pavel/Ruben gstar interface
//
// Revision 1.1  1998/10/31 00:28:31  fisyak
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
//               St_geant_Maker class for Makers                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "St_geant_Maker.h"
#include "StChain.h"
#include "TDataSetIter.h"
#include "TTable.h"
#include <iostream.h>
#include <stdio.h>
#include <string.h>
#include "TSystem.h"
#include "GtHash.h"
#include "TGeometry.h"
#include "TMaterial.h"
#include "TMixture.h"
#include "TString.h"
#include "TInterpreter.h"
#include "TClassTable.h"    
#include "TVolume.h"
#include "TMath.h"
#include "TBRIK.h"
#include "TTRD1.h"
#include "TTRD2.h"
#include "TTRAP.h"
#include "TTUBE.h"
#include "TTUBS.h"
#include "TCONE.h"
#include "TCONS.h"
#include "TSPHE.h"
#include "TPARA.h"
#include "TPGON.h"
#include "TPCON.h"
#include "TELTU.h"
//     #include "THYPE.h"
#include "TGTRA.h"
#include "TCTUB.h"
#ifdef __CC5__
#include <TGeant3.h>
#else
#include "TGeant3.h"
#endif
#include "tables/St_g2t_run_Table.h"
#include "tables/St_g2t_event_Table.h"
#include "tables/St_g2t_gepart_Table.h"
#include "tables/St_g2t_vertex_Table.h"
#include "tables/St_g2t_track_Table.h"

#include "TDataSetIter.h"
#include "g2t/St_g2t_get_event_Module.h"
#include "g2t/St_g2t_get_kine_Module.h"
#include "g2t/St_g2t_particle_Module.h"
#include "g2t/St_g2t_svt_Module.h"
#include "g2t/St_g2t_tpc_Module.h"
#include "g2t/St_g2t_mwc_Module.h"
#include "g2t/St_g2t_ftp_Module.h"
#include "g2t/St_g2t_ctb_Module.h"
#include "g2t/St_g2t_tof_Module.h"
#include "g2t/St_g2t_rch_Module.h"
#include "g2t/St_g2t_emc_Module.h"
#include "g2t/St_g2t_smd_Module.h"
#include "g2t/St_g2t_eem_Module.h"
#include "g2t/St_g2t_esm_Module.h"
#include "g2t/St_g2t_zdc_Module.h"
#include "g2t/St_g2t_vpd_Module.h"
#include "g2t/St_g2t_pmd_Module.h"
#include "g2t/St_g2t_bbc_Module.h"
#include "StarCallf77.h" 
#ifdef F77_NAME
#define    geometry	 F77_NAME(geometry,GEOMETRY)
#define    agstroot	 F77_NAME(agstroot,AGSTROOT)
#define    csaddr	 F77_NAME(csaddr,CSADDR)
#define    csjcal	 F77_NAME(csjcal,CSJCAL)
#define    g2t_volume_id F77_NAME(g2t_volume_id,G2T_VOLUME_ID)
#define    gfrotm	 F77_NAME(gfrotm,GFROTM)
#define    gfxzrm	 F77_NAME(gfxzrm,GFXZRM)
#define    dzddiv	 F77_NAME(dzddiv,DZDDIV)
#define    agnzgete	 F77_NAME(agnzgete,AGNZGETE)
#define    agstroot	 F77_NAME(agstroot,AGSTROOT)
#define    rootmaptable  F77_NAME(rootmaptable,ROOTMAPTABLE)
#define    agvolume      F77_NAME(agvolume,AGVOLUME)
#define    uhtoc         F77_NAME(uhtoc,UHTOC)
#endif
typedef long int (*addrfun)(); 
R__EXTERN "C" {
  void type_of_call geometry();
  Int_t type_of_call agstroot();
  void type_of_call *csaddr(char *name, int l77name=0);
  long int type_of_call csjcal(
				addrfun *fun, /* addres of external routine */
				int  *narg,   /* number   of arguments      */
				...);         /* other narg arguments       */
  
  Int_t type_of_call g2t_volume_id (DEFCHARD, int* DEFCHARL);
  void type_of_call gfrotm   (Int_t&,Float_t&,Float_t&,Float_t&,Float_t&,Float_t&,Float_t&);
  void type_of_call gfxzrm   (Int_t&,Float_t&,Float_t&,Float_t&,Float_t&,Float_t&,Float_t&,
			      Float_t&,Float_t&,Float_t&,Float_t&);
  void type_of_call agnzgete (Int_t &ILK,Int_t &IDE,
			      Int_t &NPART,Int_t &IRUN,Int_t &IEVT,DEFCHARD CGNAM,
			      Float_t *VERT,Int_t &IWTFL,Float_t &WEIGH DEFCHARL);
  void type_of_call dzddiv   (Int_t &,Int_t &,DEFCHARD,DEFCHARD,
			      Int_t &,Int_t &,Int_t &,Int_t & DEFCHARL DEFCHARL);
/*
* Input : ILK   - Link number  : 1 = primary, 2 = secondary (obsolete)    *
*         IDE   - ID of event in gate ( ZEBRA IDN)                        *
* Output: NPART - Number of particles in event record                     *
*         IRUN  - run number as recorded by generator                     *
*         IEVT  - event number as recorded by generator                   *
*         CGNAM - generator name                                          *
*         VERT(4)- x,y,z,t of event (metres,seconds or mm,mm/c)           *
*         IWTFL - weight flag                                             *
*         WEIGH - event weight                                            *
*/
  void type_of_call rootmaptable_(DEFCHARD,DEFCHARD,DEFCHARD, Int_t&,Char_t * 
				  DEFCHARL DEFCHARL DEFCHARL);
  Int_t type_of_call agvolume(TVolume*&,Float_t*&,Float_t*&,Float_t*&,
			      Int_t&,Int_t&,Float_t*&,Int_t&);
  void type_of_call uhtoc(Int_t&,Int_t &,DEFCHARD,Int_t& DEFCHARL);
}


Quest_t  *cquest; 
Gclink_t *clink; 
Gcflag_t *cflag; 
Gcvolu_t *cvolu; 
Gcnum_t  *cnum; 
Int_t    *z_iq, *z_lq; 
Float_t  *z_q; 
Gcsets_t *csets;
Int_t   nlev;
static Int_t irot = 0;
static TVolume *topnode=0;
typedef struct {
  Float_t par[50];
} params;
typedef struct {
  Float_t lseen, lstyle, lwidth, lcolor, lfill;
} attributes;
extern "C" 
{
  }
ClassImp(St_geant_Maker)

TDataSet *St_geant_Maker::fgGeom = 0;
TGeant3  *St_geant_Maker::geant3 = 0;
//_____________________________________________________________________________
St_geant_Maker::St_geant_Maker(const Char_t *name,Int_t nwgeant,Int_t nwpaw, Int_t iwtype):
StMaker(name),
fInputFile(""), fEvtHddr(0)
{
  fVolume    = 0;
  fNwGeant = nwgeant;
  fNwPaw   = nwpaw;
  fIwType  = iwtype;
  fgGeom = new TDataSet("geom");  
  m_ConstSet->Add(fgGeom);
  SetOutput(fgGeom);	//Declare this "geom" for output
// Initialize GEANT
  
  if (! geant3) {
    PrintInfo();
    geant3 = new TGeant3("C++ Interface to Geant3",fNwGeant,fNwPaw,fIwType);
    assert(geant3);
    cquest = (Quest_t  *) geant3->Quest();
    clink  = (Gclink_t *) geant3->Gclink();
    cflag  = (Gcflag_t *) geant3->Gcflag();
    cvolu  = (Gcvolu_t *) geant3->Gcvolu();
    cnum   = (Gcnum_t  *) geant3->Gcnum();
    z_iq   = (Int_t    *) geant3->Iq();
    z_lq   = (Int_t    *) geant3->Lq();
    z_q    = (Float_t  *) geant3->Q();
    csets  = (Gcsets_t *) geant3->Gcsets();
  }
}
//_____________________________________________________________________________
TDataSet  *St_geant_Maker::FindDataSet (const char* logInput,const StMaker *uppMk,
                                        const StMaker *dowMk) const 
{
  TDataSet *ds = StMaker::FindDataSet(logInput,uppMk,dowMk);

  if (ds || strcmp(logInput,"HALL")) return ds;

  if (!fVolume) ((St_geant_Maker *)this)->Work();

  if (fVolume) { 
    //--
    // Remove hall from the list of ROOT nodes
    // to make it free of ROOT control
    //--
    TList *listOfVolume = gGeometry->GetListOfNodes();
    // Remove hall from the list of ROOT nodes to make it free of ROOT control
    listOfVolume->Remove(fVolume);
    listOfVolume->Remove(fVolume);
    // Add "hall" into ".const" area of this maker
    ((St_geant_Maker *)this)->AddConst(fVolume);
    if (Debug()) fVolume->ls(3);
  }
  return fVolume;
}
//_____________________________________________________________________________
Int_t St_geant_Maker::Init(){
  // Create Histograms    
  if (m_Mode != 1) { // Mixer mode == 1 - do not modify EvtHddr
    fEvtHddr = (StEvtHddr*)GetDataSet("EvtHddr");
    if (!fEvtHddr) {// Stand alone run
      fEvtHddr = new StEvtHddr(m_ConstSet);
      SetOutput(fEvtHddr);	//Declare this "EvtHddr" for output
    }
  }
  BookHist();
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_geant_Maker::Make()
{
    Int_t    nhits,nhit1,nhit2,nhit3,nhit4,link=1,ide=1,npart,irun,ievt,iwtfl;
    Float_t  vert[4],weigh;

    int iRes = 0; if(iRes) {/*touch*/};
  
    Do("trig");

    // check EoF
    if (cquest->iquest[0]) {return kStEOF;}
    Int_t Nwhead,Ihead[100];
    Int_t Nwbuf;
    Float_t Ubuf[100];
    // empty g2t_event
    St_g2t_event *g2t_event = new St_g2t_event("g2t_event",1);  
    m_DataSet->Add(g2t_event);
    Char_t   cgnam[21] = "                   \0";                               
    Agnzgete(link,ide,npart,irun,ievt,cgnam,vert,iwtfl,weigh);
    geant3->Gfhead(Nwhead,Ihead,Nwbuf,Ubuf);
    if (fEvtHddr) {
      if (clink->jhead) {
	if (fEvtHddr->GetRunNumber() != *(z_iq+clink->jhead+1)) 
	  fEvtHddr->SetRunNumber(*(z_iq+clink->jhead+1));
	fEvtHddr->SetEventNumber(*(z_iq+clink->jhead+2));
      }
      if (fInputFile != "") fEvtHddr->SetEventType(TString(gSystem->BaseName(fInputFile.Data()),7));
    }
    if (npart>0) 
    {  
      St_particle  *particle   = new St_particle("particle",npart);
      m_DataSet->Add(particle);  iRes = g2t_particle(particle);
//    =======================

      particle_st *p = particle->GetTable();
      if (p->isthep == 10 && p->idhep  == 9999999 && fEvtHddr) 
      {
	fEvtHddr->SetBImpact  (p->phep[0]);
	fEvtHddr->SetPhImpact (p->phep[1]);
	fEvtHddr->SetGenerType(p->phep[2]);
	fEvtHddr->SetCenterOfMassEnergy(p->phep[3]);
	Int_t west = (int)p->phep[4];
	Int_t east = (int)(1000.*p->phep[4]-1000.*((float)west));
	fEvtHddr->SetAWest(west);
	fEvtHddr->SetAEast(east);
	if (fEvtHddr->GetRunNumber() != p->vhep[0])
	  fEvtHddr->SetRunNumber(p->vhep[0]);
	fEvtHddr->SetEventNumber(p->vhep[1]);
	fEvtHddr->SetProdDateTime();
        Int_t id = p->jdahep[0];
        Int_t it = p->jdahep[1];
        if (id <=        0) id = 19991231;
	if (id <= 19000000) id +=19000000;
        if (id >= 20500000) id = 19991231;
	if (it <         0) it = 235959;
	if (it >    246060) it = 235959;
	fEvtHddr->SetDateTime(id,it);
      }
    }
    if (!cnum->nvertx || !cnum->ntrack) return kStErr;
    St_g2t_vertex  *g2t_vertex  = new St_g2t_vertex("g2t_vertex",cnum->nvertx);
    m_DataSet->Add(g2t_vertex);
    St_g2t_track   *g2t_track   = new St_g2t_track ("g2t_track",cnum->ntrack);
    m_DataSet->Add(g2t_track);
    
    iRes = g2t_get_kine(g2t_vertex,g2t_track);
    iRes = g2t_get_event(g2t_event);

    //---------------------- inner part -------------------------//

    geant3->Gfnhit("SVTH","SVTD", nhit1);
    geant3->Gfnhit("SVTH","SFSD", nhit2);
    nhits=nhit1+nhit2;
    if (nhits>0) { 
      St_g2t_svt_hit *g2t_svt_hit = new St_g2t_svt_hit("g2t_svt_hit",nhits);
      m_DataSet->Add(g2t_svt_hit);

      iRes = g2t_svt(g2t_track,g2t_svt_hit);
//	     ===============================
    }


    geant3->Gfnhit("TPCH","TPAD", nhits);
    if (nhits>0){ 
      St_g2t_tpc_hit *g2t_tpc_hit = new St_g2t_tpc_hit("g2t_tpc_hit",nhits);
      m_DataSet->Add(g2t_tpc_hit);

      iRes = g2t_tpc(g2t_track,g2t_tpc_hit);
//	     ==============================
    }
    

    geant3->Gfnhit("TPCH","TMSE", nhits);
    if (nhits>0) { 
      St_g2t_mwc_hit *g2t_mwc_hit = new St_g2t_mwc_hit("g2t_mwc_hit",nhits);
      m_DataSet->Add(g2t_mwc_hit);
      iRes = g2t_mwc(g2t_track,g2t_mwc_hit);
//	     ==============================
    }

    geant3->Gfnhit("FTPH","FSEC", nhits);
    if (nhits>0){
      St_g2t_ftp_hit *g2t_ftp_hit = new St_g2t_ftp_hit("g2t_ftp_hit",nhits);
      m_DataSet->Add(g2t_ftp_hit);
      iRes = g2t_ftp(g2t_track,g2t_ftp_hit);
//           ===============================
    }

    geant3->Gfnhit("BTOH","BXSA", nhits);
    if (nhits>0) { 
      St_g2t_ctf_hit *g2t_ctb_hit = new St_g2t_ctf_hit("g2t_ctb_hit",nhits);
      m_DataSet->Add(g2t_ctb_hit);
      iRes = g2t_ctb(g2t_track,g2t_ctb_hit);
//           ==============================
    }

    geant3->Gfnhit("BTOH","BCSB", nhits);
    if (nhits>0) {
      St_g2t_ctf_hit *g2t_tof_hit = new St_g2t_ctf_hit("g2t_tof_hit",nhits);
      m_DataSet->Add(g2t_tof_hit);
      iRes = g2t_tof(g2t_track,g2t_tof_hit);
//           ==============================
    }


    geant3->Gfnhit("RICH","RGAP", nhit1);
    geant3->Gfnhit("RICH","RCSI", nhit2);
    geant3->Gfnhit("RICH","FREO", nhit3);
    geant3->Gfnhit("RICH","QUAR", nhit4);
//  cout << nhit1 << " " << nhit2 << " " << nhit3 << " " << nhit4 << endl;
    nhits=nhit1+nhit2+nhit3+nhit4;
    if (nhits>0) {
      St_g2t_rch_hit *g2t_rch_hit = new St_g2t_rch_hit("g2t_rch_hit",nhits);
      m_DataSet->Add(g2t_rch_hit);
      iRes = g2t_rch(g2t_track,g2t_rch_hit);
//           ==============================
    }

    //---------------------- calorimeters -------------------------//
    geant3->Gfnhit("CALH","CSUP", nhits);
    if (nhits>0) {
      St_g2t_emc_hit *g2t_emc_hit = new St_g2t_emc_hit("g2t_emc_hit",nhits);
      m_DataSet->Add(g2t_emc_hit);
      iRes = g2t_emc(g2t_track,g2t_emc_hit);
//           ==============================
    }

    geant3->Gfnhit("CALH","CSDA", nhits);
    if (nhits>0) {
      St_g2t_emc_hit *g2t_smd_hit = new St_g2t_emc_hit("g2t_smd_hit",nhits);
      m_DataSet->Add(g2t_smd_hit);
      iRes = g2t_smd(g2t_track,g2t_smd_hit);
//           ==============================
    }

    geant3->Gfnhit("ECAH","ESCI", nhit1);
    geant3->Gfnhit("ECAH","ELGR", nhit2);
    geant3->Gfnhit("ECAH","EPCT", nhit3);
    nhits = nhit1+nhit2+nhit3; 
    if (nhits>0) {
      St_g2t_emc_hit *g2t_eem_hit = new St_g2t_emc_hit("g2t_eem_hit",nhits);
      m_DataSet->Add(g2t_eem_hit);
      iRes = g2t_eem(g2t_track,g2t_eem_hit);
//           ==============================
    }

    geant3->Gfnhit("ECAH","EXSE", nhit1);
    geant3->Gfnhit("ECAH","EHMS", nhit2);
    nhits = nhit1+nhit2;
    if (nhits>0) {
      St_g2t_emc_hit *g2t_esm_hit = new St_g2t_emc_hit("g2t_esm_hit",nhits);
      m_DataSet->Add(g2t_esm_hit);
      iRes = g2t_esm(g2t_track,g2t_esm_hit);
//           ==============================
    }

    geant3->Gfnhit("VPDH","VRAD", nhits);
    if (nhits>0) {
      St_g2t_vpd_hit *g2t_vpd_hit = new St_g2t_vpd_hit("g2t_vpd_hit",nhits);
      m_DataSet->Add(g2t_vpd_hit);
      iRes = g2t_vpd(g2t_track,g2t_vpd_hit);
//           ==============================
    }

    geant3->Gfnhit("PHMH","PDGS", nhits);
    if (nhits>0) {
      St_g2t_pmd_hit *g2t_pmd_hit = new St_g2t_pmd_hit("g2t_pmd_hit",nhits);
      m_DataSet->Add(g2t_pmd_hit);
      iRes = g2t_pmd(g2t_track,g2t_pmd_hit);
//           ==============================
    }

    geant3->Gfnhit("ZCAH","QSCI", nhits);
    if (nhits>0) {
      St_g2t_emc_hit *g2t_zdc_hit = new St_g2t_emc_hit("g2t_zdc_hit",nhits);
      m_DataSet->Add(g2t_zdc_hit);
      iRes = g2t_zdc(g2t_track,g2t_zdc_hit);
//           ==============================
    }


    geant3->Gfnhit("BBCH","BPOL", nhits);
    if (nhits>0) 
      {
	St_g2t_ctf_hit *g2t_bbc_hit = new St_g2t_ctf_hit("g2t_bbc_hit",nhits);
	m_DataSet->Add(g2t_bbc_hit);
	iRes = g2t_bbc(g2t_track,g2t_bbc_hit);
//           ==============================
      }

//------------------------all bloody detectors done--------------------//
#if 0
    Char_t *g2t = "g2t_";
    Int_t  narg = 0;
    addrfun address  = (addrfun ) csaddr(g2t);
    if (address) csjcal(&address,&narg);
#endif

// Fill Histograms    
   FillHist();

   if (cflag->ieorun) return kStEOF; 
   if (cflag->ieotri) return kStErr; 
   return kStOK;
}
//_____________________________________________________________________________
void St_geant_Maker::LoadGeometry(Char_t *option){
  if (strlen(option)) Do (option); 
  Geometry();
  Do("gclose all");
  Agstroot();
}
//_____________________________________________________________________________
void St_geant_Maker::Draw(const char*)
{ 
  Int_t two = 2;
  Int_t zero = 0;
  Int_t one = 1;
  Char_t *path = " ";
  Char_t *opt = "IN";
  Dzddiv (two,zero,path,opt,one,zero,one,one);
}
//_____________________________________________________________________________
void St_geant_Maker::Do(const Char_t *job)
{  
  int l=strlen(job);
  if (l) geant3->Kuexel(job);
}
//_____________________________________________________________________________

//_____________________________________________________________________________
TVolume *St_geant_Maker::MakeVolume(TString *name, Int_t ivo, Int_t Nlevel, Int_t *Names, Int_t *Numbers){
  TVolume *node = 0;
  Int_t   jvolum  = clink->jvolum;
  Int_t jvo = z_lq[jvolum-ivo];
  if (jvo) {
    node = (TVolume *) topnode->FindObject(name->Data());
    if (! node) {
      TShape *shape = (TShape *) gGeometry->GetListOfShapes()->FindObject(name->Data());
      if (!shape ) {shape = MakeShape(name,ivo);}
      node = new TVolume(name->Data(), name->Data(), shape);
    }

    Int_t nin =(Int_t) z_q[jvo+3];
    if (nin > 0) 
    {
      Nlevel++;
      for (Int_t in=1; in<= nin; in++) 
      {
	Int_t jin   =        z_lq[jvo-in];
	Int_t ivom  = (Int_t) z_q[jin+2];
	Int_t nuser = (Int_t) z_q[jin+3];
	TString  namem((const Char_t *) &(z_iq[jvolum+ivom]), 4);

	Names[Nlevel] = z_iq[jvolum+ivom];
	Numbers[Nlevel] = nuser;
	Int_t   nlevv = Nlevel+1;
	Int_t   Ierr;
	Float_t xx[3], theta1,phi1, theta2,phi2, theta3,phi3, type;

	Ierr = geant3->Glvolu(nlevv, Names, Numbers);

	Gfxzrm(Nlevel, xx[0],xx[1],xx[2], 
		       theta1,phi1, theta2,phi2, theta3,phi3, type);
        TVolume *newnode = (TVolume *) topnode->FindObject(namem.Data());

	if (!newnode) 
        {  newnode = MakeVolume(&namem, ivom, nlevv, Names, Numbers);  }

	irot++;
	Char_t ss[12];
	sprintf(ss,"rotm%i",irot);
	TRotMatrix *rotm = new TRotMatrix(ss,ss, 
                           theta1,phi1, theta2,phi2, theta3,phi3);
	node->Add(newnode,xx[0],xx[1],xx[2],rotm);
      }
    }
    if (nin < 0) {
      Nlevel++;
    }
    if (nin == 0) {Nlevel--;}
  }
  return node;
}
//_____________________________________________________________________________
TShape *St_geant_Maker::MakeShape(TString *name, Int_t ivo){
  // make geant3 volume
  typedef enum {BOX=1,TRD1,TRD2,TRAP,TUBE,TUBS,CONE,CONS,SPHE,PARA,
		PGON,PCON,ELTU,HYPE,GTRA=28,CTUB} shapes;
  Int_t jvolum  = clink->jvolum;
  Int_t jvo = z_lq[jvolum-ivo];
  TShape*  t;
  shapes   shape  = (shapes) z_q[jvo+2];
  Int_t    numed  = (Int_t)  z_q[jvo+4];
  Int_t    npar   = (Int_t)  z_q[jvo+5];
  params  *p      = (params *)&z_q[jvo+7];
  attributes *att = (attributes *)(&z_q[jvo+7] + npar);
  Int_t    jtmed  = clink->jtmed;
  Int_t    jtm    =          z_lq[jtmed-numed];
  Int_t    nmat   =          (int)z_q[jtm+6];
  Int_t    jmate  = clink->jmate;
  Int_t    jma    =          z_lq[jmate-nmat];
  Int_t    nmixt  = (Int_t)  z_q[jma+11];
  Int_t    nm     = TMath::Abs(nmixt);

  Char_t   astring[20];
  if (nm <= 1)  sprintf (astring,"mat%i",nmat); 
  else          sprintf (astring,"mix%i",nmat);   

  TString Astring(astring);
  t = (TShape *) gGeometry->GetListOfShapes()->FindObject(name->Data());
  if (!t) {
    switch (shape) {
    case BOX:  t = new TBRIK((Char_t *) name->Data(),"BRIK",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],p->par[2]);      
    break;
    case TRD1: t = new TTRD1((Char_t *) name->Data(),"TRD1",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],p->par[2],p->par[3]);
    break;
    case TRD2: t = new TTRD2((Char_t *) name->Data(),"TRD2",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],p->par[2],p->par[3],p->par[4]);
    break;
    case TRAP: t = new TTRAP((Char_t *) name->Data(),"TRAP",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],p->par[2],p->par[3],p->par[4],
			     p->par[5],p->par[6],p->par[7],p->par[8],p->par[9],
			     p->par[10]);
    break;
    case TUBE: t = new TTUBE((Char_t *) name->Data(),"TUBE",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],p->par[2]); 
    break;
    case TUBS: t = new TTUBS((Char_t *) name->Data(),"TUBS",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],p->par[2],p->par[3],p->par[4]);
    break;
    case CONE: t = new TCONE((Char_t *) name->Data(),"CONE",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],p->par[2],p->par[3],p->par[4]);
    break;
    case CONS: t = new TCONS((Char_t *) name->Data(),"CONS",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],p->par[2],p->par[3],p->par[4],
			     p->par[5],p->par[6]);
    break;
    case SPHE: t = new TSPHE((Char_t *) name->Data(),"SPHE",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],p->par[2],p->par[3],p->par[4],
			     p->par[5]);
    break;
    case PARA: t = new TPARA((Char_t *) name->Data(),"PARA",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],p->par[2],p->par[3],p->par[4],
			     p->par[5]);
    break;
    case PGON: t = new TPGON((Char_t *) name->Data(),"PGON",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],p->par[2],p->par[3]);
    break;
    case PCON: t = new TPCON((Char_t *) name->Data(),"PCON",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],p->par[2]);
    break;
    case ELTU: t = new TELTU((Char_t *) name->Data(),"ELTU",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],p->par[2]);
    break;
    //      case HYPE: t = new THYPE((Char_t *) name->Data(),"HYPE",(Char_t *) Astring.Data(),
    //			       p->par[0],p->par[1],p->par[2],p->par[3]);
    //      break;
    case GTRA: t = new TGTRA((Char_t *) name->Data(),"GTRA",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],p->par[2],p->par[3],p->par[4],
			     p->par[5],p->par[6],p->par[7],p->par[8],p->par[9],
			     p->par[10],p->par[11]); 
    break;
    case CTUB: t = new TCTUB((Char_t *) name->Data(),"CTUB",(Char_t *) Astring.Data(),
			     p->par[0],p->par[1],p->par[2],p->par[3],p->par[4],
			     p->par[5],p->par[6],p->par[7],p->par[8],p->par[9],
			     p->par[10]);
    break;
    //      default:   t = new TBRIK((Char_t *) name->Data(),"BRIK",(Char_t *) Astring.Data(),
    //			       p->par[0],p->par[1],p->par[2]);
    //      break;

    default: assert(0);

    } 
    if (att->lseen  != 1) t->SetVisibility(att->lseen);
    if (att->lstyle != 1) t->SetLineStyle(att->lstyle);
    if (att->lwidth != 1) t->SetLineWidth(att->lwidth);
    if (att->lcolor != 1) t->SetLineColor(att->lcolor);
    if (att->lfill  != 1) t->SetFillStyle(att->lfill);
  }
  return t;
}
//_____________________________________________________________________________
void St_geant_Maker::Call(const Char_t *name)
{  
  Int_t  narg = 0;
  addrfun *address  = (addrfun *) csaddr_((Char_t *)name, strlen(name));
  if (address) csjcal_(address, &narg);
}
//_____________________________________________________________________________
TVolume *St_geant_Maker::Work()
{  
  struct  Medium 
    { Char_t name[20]; Int_t nmat, isvol, ifield; Float_t fieldm; };
  struct  Volume
    { Char_t name[4],nick[4]; Int_t npar; Float_t par[50]; };

  TVolume   *node=0;
  Float_t   *volu=0, *position=0, *mother=0, *p=0;
  Int_t     who=0, copy=0, npar=0;
  Int_t     nvol=cnum->nvolum;
  Float_t   theta1,phi1, theta2,phi2, theta3,phi3, type;
  TObjArray nodes(nvol+1);

  new TGeometry("STAR","nash STAR");
  GtHash *H = new GtHash;
 
  printf(" looping on agvolume \n");
  //   ===============================================================
  while (Agvolume(node,volu,position,mother,who,copy,p,npar)) 
  { // ===============================================================
  
    typedef enum {BOX=1,TRD1,TRD2,TRAP,TUBE,TUBS,CONE,CONS,SPHE,PARA,
                        PGON,PCON,ELTU,HYPE,GTRA=28,CTUB} shapes;
    TShape*  t;
    shapes   shape   = (shapes) volu[1];
    Int_t    nin     = 0;
    //   Int_t    medium  = (Int_t)  volu[3]; 
    Int_t    np      = (Int_t)  volu[4];
    Float_t* p0      = volu+6;
    Float_t* att     = p0+np;
    Char_t   name[]  = {0,0,0,0,0};
    Char_t   nick[]  = {0,0,0,0,0};
    float    xx[3]   = {0.,0.,0.};
    TVolume *newVolume = 0;
    if (mother)  nin = (Int_t) mother[2];
    TVolume *Hp      = 0;

    strncpy(nick,(const Char_t*)&cvolu->names[cvolu->nlevel-1],4);
    strncpy(name,(const Char_t*)(volu-5),4);

    Hp = (TVolume *) H->GetPointer(p,npar+1);
    if (Hp)  newVolume = Hp; 
    else
      { // printf(" creating object %s  %f  %f  %f \n", name,p[0],p[1],p[2]);
      switch (shape) 
      { case BOX:  t=new TBRIK(nick,"BRIK","void",
                         p[0],p[1],p[2]);                         break;
        case TRD1: t=new TTRD1(nick,"TRD1","void",
                         p[0],p[1],p[2],p[3]);                    break;
        case TRD2: t=new TTRD2(nick,"TRD2","void",
                         p[0],p[1],p[2],p[3],p[4]);               break;
        case TRAP: t=new TTRAP(nick,"TRAP","void",
                         p[0],p[1],p[2],p[3],p[4],p[5],
                         p[6],p[7],p[8],p[9],p[10]);              break;
        case TUBE: t=new TTUBE(nick,"TUBE","void",
                         p[0],p[1],p[2]);                         break;
        case TUBS: t=new TTUBS(nick,"TUBS","void",
                         p[0],p[1],p[2],p[3],p[4]);               break;
        case CONE: t=new TCONE(nick,"CONE","void",
                         p[0],p[1],p[2],p[3],p[4]);               break;
        case CONS: t=new TCONS(nick,"CONS","void",    // take care !
                         p[0],p[1],p[2],p[3],p[4],p[5],p[6]);     break;
//                         p[1],p[2],p[3],p[4],p[0],p[5],p[6]);     break;
        case SPHE: t=new TSPHE(nick,"SPHE","void",
                         p[0],p[1],p[2],p[3],p[4],p[5]);          break;
        case PARA: t=new TPARA(nick,"PARA","void",
                         p[0],p[1],p[2],p[3],p[4],p[5]);          break;
        case PGON: t=new TPGON(nick,"PGON","void",p[0],p[1],p[2],p[3]);  
                   { Float_t *pp = p+4;
                     for (Int_t i=0; i<p[3]; i++) {
                          Float_t z    = *pp++;
                          Float_t rmin = *pp++;
                          Float_t rmax = *pp++;
                         ((TPCON *)t)->DefineSection(i,z,rmin,rmax);
// this is because of a compiler bug on Linux (VF 030699)
//                         (( TPGON*)t)->DefineSection(i,*pp++,*pp++,*pp++);
                     }
                   }                                              break;
        case PCON: t=new TPCON(nick,"PCON","void",p[0],p[1],p[2]);
                   { Float_t *pp = p+3;
                     for (Int_t i=0; i<p[2]; i++) {
                          Float_t z    = *pp++;
                          Float_t rmin = *pp++;
                          Float_t rmax = *pp++;
                         ((TPCON *)t)->DefineSection(i,z,rmin,rmax);
// this is because of a compiler bug on Linux (VF 030699)
//                         ((TPCON *)t)->DefineSection(i,*pp++,*pp++,*pp++);
                     }
                   }                                              break;
        case ELTU: t=new TELTU(nick,"ELTU","void",
                         p[0],p[1],p[2]);                         break;
//      case HYPE: t=new THYPE(nick,"HYPE","void",
//                       p[0],p[1],p[2],p[3]);                    break;
        case GTRA: t=new TGTRA(nick,"GTRA","void",
                         p[0],p[1],p[2],p[3],p[4],p[5],
                         p[6],p[7],p[8],p[9],p[10],p[11]);        break;
        case CTUB: t=new TCTUB(nick,"CTUB","void",
                         p[0],p[1],p[2],p[3],p[4],p[5],
                         p[6],p[7],p[8],p[9],p[10]);              break;
        default:   t=new TBRIK(nick,"BRIK","void",
                         p[0],p[1],p[2]);                         break;
      };
      t->SetLineColor(att[4]);
 
      // to build a compressed tree, name should be checked for repetition
      newVolume = new TVolume(name,nick,t);
//      newVolume -> SetVisibility(ENodeSEEN(MapGEANT2StNodeVis(att[1])));
      newVolume -> SetVisibility((TVolume::ENodeSEEN)TVolume::MapGEANT2StNodeVis(att[1]));
      H->SetPointer(newVolume);
    }

    if (node)
    {  Gfxzrm(nlev, xx[0],xx[1],xx[2], theta1,phi1, 
                       theta2,phi2, theta3,phi3, type);
       TRotMatrix *matrix=GetMatrix(theta1,phi1,theta2,phi2,theta3,phi3);
       node->Add(newVolume,xx[0],xx[1],xx[2],matrix,UInt_t(copy));
    }
    node = newVolume;
  };

  fVolume=node;
  gGeometry->GetListOfNodes()->Add(node);
  return GetVolume();
}
//_____________________________________________________________________________
void St_geant_Maker::Mark(TVolume *topvol) {
  Int_t JSET = clink->jset;
  if (JSET <= 0) return;
  Int_t  NSET=z_iq[JSET-1];
  Char_t Uset[5], Udet[5], Uvol[5];
  memset (Uset, 0, 5);
  memset (Udet, 0, 5);
  memset (Uvol, 0, 5);
  for (Int_t ISET=1;ISET<=NSET;ISET++) {
    Int_t JS=z_lq[JSET-ISET];
    if (JS <= 0) continue;
    Int_t NDET=z_iq[JS-1];
    memcpy (Uset, &z_iq[JSET+ISET], 4);
    for (Int_t IDET=1;IDET<=NDET;IDET++) {
      Int_t JD=z_lq[JS-IDET];
      if (JD <=0) continue;
      Int_t NV=z_iq[JD+2];
      Int_t NWHI=z_iq[JD+7];
      Int_t NWDI=z_iq[JD+8];
      memcpy (Udet, &z_iq[JS+IDET], 4);
      cout << "  Set " << Uset << " Detector " << Udet
	   << "  NV " << NV << " NWHI " << NWHI << " NWDI " << NWDI << endl;
      Int_t JDU = z_lq[JD-3];
      if (JDU > 0) {
	Int_t i1 = (int)z_q[JDU+3], i2 = (int)z_q[JDU+5];
	cout << " Volume/Bits :" << i1 << "/" << i2 <<  endl;
	for (Int_t i=i1;i<i2;i += 3) {
	  Int_t j   = JDU+i;
	  Int_t iv  = (int)z_q[j+1];
	  Int_t Nmx = (int)z_q[j+2];
	  Int_t Nam = (int)z_iq[clink->jvolum+iv];
	  Int_t Nb  = (int)z_q[j+3];
	  memcpy (Uvol, &Nam, 4);
	  cout << "\t" << Uvol << "\t" << Nmx << "\t" << Nb << endl;
	}
      }
      else {
	if (NV > 0) {
	  cout << " Volume/Bits ";
	  for (Int_t I=1; I<=NV; I++) {
	    memcpy (Uvol, &z_iq[JD+2*I+9], 4);
	    cout << "\t" << Uvol << "/\t" << z_iq[JD+2*I+10];
	  }
	  cout << endl;
	}
      }
    }
  }
#if 0
  geant3->Gfinds();
  if (csets->iset && csets->idet) {
    cout << "Set/Det \t" << csets->iset << "/" << csets->idet 
	 << "\tidtype = \t" << csets->idtype
	 << "\tnvname = \t" << csets->nvname << endl; 
    Int_t nLev, lNam[15], lNum[15];
    Char_t Name[4];
    geant3->Gfpath(csets->iset,csets->idet,csets->numbv, nLev, lNam, lNum);
    Int_t four = 4;
    for (Int_t i=0; i< nLev; i++) {
      uhtoc(lNam[i],four,PASSCHARD(Name),four PASSCHARL(Name));
      cout << "\t" << Name << "\t" << lNum[i];
    }
    cout << endl;
  }
#endif
}
//_____________________________________________________________________________
static Bool_t CompareMatrix(TRotMatrix &a,TRotMatrix &b)
{  double *pa=a.GetMatrix(); double *pb=b.GetMatrix();
   for (int i=0; i<9; i++)  if (pa[i]!=pb[i]) return kFALSE;
   return kTRUE;
}
//_____________________________________________________________________________
TRotMatrix *St_geant_Maker::GetMatrix(float thet1, float phii1,
                                      float thet2, float phii2,
                                      float thet3, float phii3)
{  char mname[20];
   THashList *list = gGeometry->GetListOfMatrices();
   int n=list->GetSize(); sprintf(mname,"matrix%d",n+1);
   TRotMatrix *pattern=new TRotMatrix(mname,mname,
                                      thet1,phii1,thet2,phii2,thet3,phii3);
   
   TRotMatrix *matrix=0; TIter nextmatrix(list);
   while ((matrix=(TRotMatrix *) nextmatrix())) 
   { if (matrix!=pattern) 
     { if (CompareMatrix(*matrix,*pattern)) 
       { list->Remove(pattern); delete pattern; return matrix; }
   } }
   return pattern;
}
//_____________________________________________________________________________
void  St_geant_Maker::SetDebug(Int_t dbl)
{
  StMaker::SetDebug(dbl);
  if (GetDebug()) { Do("debug on;"); } else {Do("debug off;"); }
}
//_____________________________________________________________________________
Int_t St_geant_Maker::SetInputFile(const char *file)
{
  fInputFile = file;
  TString kuip("gfile p "); kuip += fInputFile;
  Do((const char*)kuip); 
  if (cquest->iquest[0]) {return kStEOF;}
  Do("gclose all");
  Agstroot();
  return kStOK;
}
//_____________________________________________________________________________
Int_t St_geant_Maker::Skip(Int_t Nskip)
{
  if (Nskip >= 0) {
    Char_t kuip[20];
    sprintf (kuip,"skip %i",Nskip);
     if (GetDebug()) printf("St_geant_Maker skip %i\n record(s)",Nskip); 
    Do((const char*)kuip);
    
    if (cquest->iquest[0]) {return kStEOF;}
  }
  return kStOK;
}
//_____________________________________________________________________________
void type_of_call rootmaptable_(const Char_t* cdest,const Char_t* table , const Char_t* spec, 
                                                      Int_t &k, Char_t *iq, 
				const int lCdest,const int lTable, const int lSpec)
{ 
  Char_t *Cdest = new char[(lCdest+1)]; strncpy(Cdest,cdest,lCdest); Cdest[lCdest] = 0;
  Char_t *Table = new char[(lTable+1)]; strncpy(Table,table,lTable); Table[lTable] = 0;
  Char_t *Spec  = new char[(lSpec+1)];  strncpy(Spec,spec,lSpec);    Spec[lSpec]   = 0;
  St_geant_Maker::RootMapTable(Cdest,Table,Spec, k, iq); 
  delete [] Cdest;
  delete [] Table;
  delete [] Spec;
}
//_____________________________________________________________________________
void St_geant_Maker::RootMapTable(Char_t *Cdest,Char_t *Table, Char_t* Spec, 
                                                      Int_t &k, Char_t *iq)
{
  TString TableName(Table); 
  TString t = TableName.Strip();
  t.ToLower();

  // Use St_Table::New(...)  when it is available as follows:
  St_Table *table =  St_Table::New(t.Data(),t.Data(),iq,k);
  if (table) {fgGeom->Add(table); table->SetBit(kIsNotOwn);}
  else       cout << "Dictionary for table :" << t.Data() 
                  << " has not been defined yet. Skip it" 
                  << endl;
}
//_____________________________________________________________________________
Int_t St_geant_Maker::G2t_volume_id(const Char_t *name, Int_t *numbv){
  return g2t_volume_id(PASSCHARD(name),numbv PASSCHARL(name));
}
//_____________________________________________________________________________
Int_t St_geant_Maker::Agvolume(TVolume *&node,Float_t *&par,Float_t *&pos,Float_t *&mot,
		       Int_t &who, Int_t &copy,Float_t *&par1,Int_t &npar){
  return agvolume(node,par,pos,mot,who,copy,par1,npar);
}
//_____________________________________________________________________________
void St_geant_Maker::Agnzgete (Int_t &ILK,Int_t &IDE,
			Int_t &NPART,Int_t &IRUN,Int_t &IEVT,const Char_t *CGNAM,
			Float_t *VERT,Int_t &IWTFL,Float_t &WEIGH){
  agnzgete (ILK,IDE,NPART,IRUN,IEVT,PASSCHARD(CGNAM),VERT,IWTFL,WEIGH
	    PASSCHARL(CGNAM));
}
//______________________________________________________________________________
void St_geant_Maker::Geometry() {geometry();}
//______________________________________________________________________________
Int_t St_geant_Maker::Agstroot() {return agstroot();}
//_____________________________________________________________________________
void St_geant_Maker::Gfxzrm(Int_t & Nlevel, 
		     Float_t &x, Float_t &y, Float_t &z,
		     Float_t &Theta1, Float_t & Phi1,
		     Float_t &Theta2, Float_t & Phi2,
		     Float_t &Theta3, Float_t & Phi3,
		     Float_t &Type){
	gfxzrm(Nlevel, x, y, z,
	       Theta1, Phi1, 
	       Theta2, Phi2, 
	       Theta3, Phi3, Type);
} 
//_____________________________________________________________________________
void St_geant_Maker::Dzddiv(Int_t& idiv ,Int_t &Ldummy,const Char_t* path,const Char_t* opt,
		     Int_t& one,Int_t &two,Int_t &three,Int_t& iw){
  dzddiv (idiv,Ldummy,PASSCHARD(path),PASSCHARD(opt),
	  one,two,three,iw PASSCHARL(path) PASSCHARL(opt));
}

//_____________________________________________________________________________

void St_geant_Maker::BookHist(){
  
  cout << "***********  St_geant_Maker - bookhist!!!! *********" << endl;

  m_histvx =0;
  m_histvy =0;
  m_histvz =0;

  m_histvx = new TH1F("GeantPVtxX"," geant vertex: primary X (cm)",
        50, -5.0,5.0);
  m_histvy = new TH1F("GeantPVtxY"," geant vertex: primary Y (cm)",
        50, -5.0,5.0);
  m_histvz = new TH1F("GeantPVtxZ"," geant vertex: primary Z (cm)",
        50, -50.0,50.0);

}

//_____________________________________________________________________________

void St_geant_Maker::FillHist(){
  //  cout << " St_geant_Maker::FillHist - Will now fill histograms! " << endl;


  // get geant event vertex
  TDataSet *geant = GetDataSet("geant"); 
  if( !geant ){
    cout << " No pointer to GEANT DataSet \n" << endl; 
  }
 
  St_g2t_vertex *geantVertex=(St_g2t_vertex *) geant->Find("g2t_vertex"); 
  if( !geantVertex ){
    cout << " NULL pointer to St_g2t_vertex table\n"<< endl;
  }
 
  if( geantVertex->GetNRows()<=0) { 
   cout << " empty St_g2t_vertex table\n" << endl; 
  } 

  g2t_vertex_st *gvt=geantVertex->GetTable();

  cout << " geant event vertex: " << 
     gvt->ge_x[0] << "\t" << gvt->ge_x[1] << "\t" << gvt->ge_x[2] << endl;

  m_histvx->Fill(gvt->ge_x[0]);
  m_histvy->Fill(gvt->ge_x[1]);
  m_histvz->Fill(gvt->ge_x[2]);
}
//________________________________________________________________________________
void St_geant_Maker::SetNwGEANT(Int_t n){cout << "St_geant_Maker::SetNwGEANT is obsolete now\n";}
void St_geant_Maker::SetNwPAW(Int_t n){cout << "St_geant_Maker::SetNwPAW is obsolete now\n";}
void St_geant_Maker::SetIwtype(Int_t n){cout << "St_geant_Maker::SetIwtype is obsolete now\n";}
