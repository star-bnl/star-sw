 //////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMatchMaker class ( svm + est + egr )                               //
//                                                                      //
// $Id: StMatchMaker.cxx,v 1.20 2000/03/29 14:33:40 caines Exp $
// $Log: StMatchMaker.cxx,v $
// Revision 1.20  2000/03/29 14:33:40  caines
// Fixed topology map for TPC only
//
// Revision 1.19  2000/03/10 21:54:18  lbarnby
// Turn on Kalman fitter as default for global tracks
//
// Revision 1.18  2000/03/09 23:31:17  lbarnby
// Protection against no TPC hits when creating tpc_groups
//
// Revision 1.17  2000/03/01 14:48:09  caines
// Removed references to scs_cluster
//
// Revision 1.16  2000/02/25 03:28:58  caines
// Stuff to fill bit map, cov correctly
//
// Revision 1.15  2000/02/25 02:38:27  caines
// Stuff to fill bit map, cov correctly
//
// Revision 1.14  2000/02/02 21:37:37  lbarnby
// CC5
//
// Revision 1.13  1999/11/27 18:21:41  fisyak
// Add test that primary vertex exists
//
// Revision 1.12  1999/11/16 20:58:41  wdeng
// Spiros's temporary solution to id_start_vertex puzzle
//
// Revision 1.11  1999/10/29 23:23:25  caines
// Removed scenario methods
//
// Revision 1.10  1999/10/01 21:16:03  wdeng
// Take out dst auxiliary table.
//
// Revision 1.9  1999/09/13 15:06:23  caines
// Added creation of garb(tphit) and garb(tptrack) so it is possible
// to run with TPC turned off
//
// Revision 1.8  1999/09/12 23:03:03  fisyak
// Move parameters into makers
//
// Revision 1.7  1999/07/28 02:18:19  caines
// Add in kalman filter flags
//
// Revision 1.6  1999/07/17 00:31:24  genevb
// Use StMessMgr
//
// Revision 1.5  1999/07/15 13:57:52  perev
// cleanup
//
// Revision 1.4  1999/07/08 19:09:51  fisyak
// Add tabs, remove St_glb_Maker
//
//////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
//#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#if !defined(ST_NO_NAMESPACES)
using namespace units;
#endif
#include "TMath.h"
#include "StMatchMaker.h"

#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"

#include "StMessMgr.h"

#include "tables/St_tcl_tpcluster_Table.h"
#include "tables/St_ctu_cor_Table.h"

#include "global/St_svm_am_Module.h"
#include "global/St_svm_eval2_Module.h"
#include "global/St_svm_svt_eval_Module.h"
#include "global/St_svm_efficiency_Module.h"

#include "global/St_est_am_Module.h"
#include "global/St_est_toglob2_Module.h"
#include "global/St_est_eval_Module.h"
#include "global/St_egr_fitter_Module.h"
#define gufld   gufld_
extern "C" {void gufld(Float_t *, Float_t *);}


class St_tcl_tpcluster;
class St_ctu_cor;

ClassImp(StMatchMaker)
  
  //_____________________________________________________________________________
  StMatchMaker::StMatchMaker(const char *name):StMaker(name),
  m_svm_ctrl(0),
  m_egr_egrpar(0),
  m_est_ctrl(0)
{
  drawinit=kFALSE;
  m_svtchicut = 0;
  m_useglobal = 4;
  m_usesvt    = 0;
  m_usetpc    = 4;
  m_usevert   = 0;
  m_flag      = 2;
}
//_____________________________________________________________________________
StMatchMaker::~StMatchMaker(){
}
//_____________________________________________________________________________
Int_t StMatchMaker::Init(){
  // Create tables
   Float_t x[3] = {0,0,0};
   Float_t b[3];
   gufld(x,b);
   Double_t B = b[2]*kilogauss;
  //svm
   m_svm_ctrl = new St_svm_ctrl("svm_ctrl",1);
  {
    svm_ctrl_st row;
    memset(&row,0,m_svm_ctrl->GetRowSize());
    row.ktrl1	 =          0; // control switch 1: allows errors in helix ;
    row.ktrl2	 =          0; // chi-sq form:0->e1*e2,1->(e1**2 + e2**2) ;
    row.ktrl3	 =          0; // control switch 3: =2 for level 2 matchin ;
    row.ktrl4	 =          1; // control switch 4: selects MCS formula ;
    row.ktrl5	 =          3; // .ne.0 limits # loops in bi-direc. match. ;
    row.arfinc	 =          4; // search area increase factor ;
    row.bmag	 =        0.5; // magnetic field (T) ;
    row.dacep	 =          0; // not used at present ;
    row.efaca	 =          1; // relative weight for direc. part of chisq ;
    row.efacp	 =         10; // invpt cut-off for matched tpc tracks ;
    row.efacz	 =        400; // chi-square cut-off for matching ;
    row.pmerr	 =       0.05; // initial size of search area: momentum ;
    row.rifc	 =     46.825; // radial position of tpc IFC (cm) ;
    row.rlgas	 =      36000; // radiation length of gas (cm) ;
    row.rlifc	 =        128; // radiation length of tpc IFC (cm) ;
    row.rlsdd3	 =       9.36; // rad. length for SDD outermost layer (cm) ;
    row.rltpc_gas=      10949; // Radiation length of TPC gas (cm) ;
    row.rltube	 =         25; // rad. length of svt support tube (cm) ;
    row.rmatch	 =         40; // matching radius from beam axis (cm) ;
    row.rming	 =         15; // inner radial position of gas vessel (cm) ;
    row.rsdd3	 =      14.91; // radial position of SDD outer layer (cm) ;
    row.rtpc_gas =     47.476; // Inner radius of TPC gas vessel (cm) ;
    row.rtube	 =       17.5; // radial position of svt support tube (cm) ;
    row.slpcut	 = 1.91668e-38; // dip angle cut-off, 2nd level matching ;
    row.tgas	 =         36; // thickness of gas vessel region (cm) ;
    row.tifc	 =        0.3; // thickness of tpc inner field cage (cm) ;
    row.tsdd3	 =       0.03; // thickness of SDD outermost layer (cm) ;
    row.ttpc_gas =        150; // Thickness of TPC gas vessel ;
    row.ttube	 =       0.31; // thickness of svt support tube (cm) ;
    row.xcut	 =          0; // x cut-off for 2nd level matching (cm) ;
    row.ycut	 =          0; // y cut-off for 2nd level matching (cm) ;
    m_svm_ctrl->AddAt(&row,0);
  }
  AddRunCont(m_svm_ctrl);
  //est
  
  m_est_ctrl= new St_est_ctrl("est_ctrl",1);
  {
    est_ctrl_st row;
//
    memset(&row,0,m_est_ctrl->GetRowSize());
    row.ktrl1	 =          1; // control switch for printing (1=on, 0=off) ;
    row.pass	 =          5; // number of passes;
    row.nhole[0]	 =          1; // number of holes in the segment (pass 1);
    row.nhole[1]	 =          1; // number of holes in the segment (pass 2);
    row.nhole[2]	 =          2; // number of holes in the segment (pass 3);
    row.nhole[3]	 =          3; // number of holes in the segment (pass 4);
    row.nhole[4]	 =          3; // number of holes in the segment (pass 5);
    row.apmin[0]	 =        0.9; // minimum pt (pt loop  1);
    row.apmin[1]	 =        0.7; // minimum pt (pt loop  2);
    row.apmin[2]	 =        0.6; // minimum pt (pt loop  3);
    row.apmin[3]	 =        0.5; // minimum pt (pt loop  4);
    row.apmin[4]	 =        0.1; // minimum pt (pt loop  5);
    row.apmin[5]	 =        0.1; // minimum pt (pt loop  6);
    row.apmin[6]	 =        0.1; // minimum pt (pt loop  7);
    row.afacmax1[0]	 =          2; // division factor for the scaning area (layer 1 pt loop  1) ;
    row.afacmax1[1]	 =          2; // division factor for the scaning area (layer 1 pt loop  2) ;
    row.afacmax1[2]	 =          2; // division factor for the scaning area (layer 1 pt loop  3) ;
    row.afacmax1[3]	 =          2; // division factor for the scaning area (layer 1 pt loop  4) ;
    row.afacmax1[4]	 =          2; // division factor for the scaning area (layer 1 pt loop  5) ;
    row.afacmax1[5]	 =          2; // division factor for the scaning area (layer 1 pt loop  6) ;
    row.afacmax1[6]	 =          2; // division factor for the scaning area (layer 1 pt loop  7) ;
    row.afacmax2[0]	 =          2; // division factor for the scaning area (layer 2 pt loop  1) ;
    row.afacmax2[1]	 =          2; // division factor for the scaning area (layer 2 pt loop  2) ;
    row.afacmax2[2]	 =          2; // division factor for the scaning area (layer 2 pt loop  3) ;
    row.afacmax2[3]	 =          2; // division factor for the scaning area (layer 2 pt loop  4) ;
    row.afacmax2[4]	 =          2; // division factor for the scaning area (layer 2 pt loop  5) ;
    row.afacmax2[5]	 =          2; // division factor for the scaning area (layer 2 pt loop  6) ;
    row.afacmax2[6]	 =          2; // division factor for the scaning area (layer 2 pt loop  7) ;
    row.afacmax3[0]	 =          2; // division factor for the scaning area (layer 3 pt loop  1) ;
    row.afacmax3[1]	 =          2; // division factor for the scaning area (layer 3 pt loop  2) ;
    row.afacmax3[2]	 =          2; // division factor for the scaning area (layer 3 pt loop  3) ;
    row.afacmax3[3]	 =          2; // division factor for the scaning area (layer 3 pt loop  4) ;
    row.afacmax3[4]	 =          2; // division factor for the scaning area (layer 3 pt loop  5) ;
    row.afacmax3[5]	 =          2; // division factor for the scaning area (layer 3 pt loop  6) ;
    row.afacmax3[6]	 =          2; // division factor for the scaning area (layer 3 pt loop  7) ;
    row.afacmax4[0]	 =          2; // division factor for the scaning area (layer 4 pt loop  1) ;
    row.afacmax4[1]	 =          2; // division factor for the scaning area (layer 4 pt loop  2) ;
    row.afacmax4[2]	 =          2; // division factor for the scaning area (layer 4 pt loop  3) ;
    row.afacmax4[3]	 =          2; // division factor for the scaning area (layer 4 pt loop  4) ;
    row.afacmax4[4]	 =          2; // division factor for the scaning area (layer 4 pt loop  5) ;
    row.afacmax4[5]	 =          2; // division factor for the scaning area (layer 4 pt loop  6) ;
    row.afacmax4[6]	 =          2; // division factor for the scaning area (layer 4 pt loop  7) ;
    row.stupid	 =          0; // =1 - perfect tracking, =0 - real tracking ;
    row.ext[0]	 =        1.1; // extension factor of the wafer area (pt loop  1) ;
    row.ext[1]	 =        1.1; // extension factor of the wafer area (pt loop  2) ;
    row.ext[2]	 =        1.2; // extension factor of the wafer area (pt loop  3) ;
    row.ext[3]	 =        1.2; // extension factor of the wafer area (pt loop  4) ;
    row.ext[4]	 =        1.3; // extension factor of the wafer area (pt loop  5) ;
    row.ext[5]	 =        1.4; // extension factor of the wafer area (pt loop  6) ;
    row.ext[6]	 =        1.5; // extension factor of the wafer area (pt loop  7) ;
    row.ext[7]	 =        1.6; // extension factor of the wafer area (pt loop  8) ;
    row.ext[8]	 =        1.7; // extension factor of the wafer area (pt loop  9) ;
    row.ext[9]	 =        1.8; // extension factor of the wafer area (pt loop 10) ;
    row.ext[10]	 =        1.9; // extension factor of the wafer area (pt loop 11) ;
    row.ext[11]	 =        2.5; // extension factor of the wafer area (pt loop 12) ;
    row.ext[12]	 =          2; // extension factor of the wafer area (pt loop 13) ;
    row.ext[13]	 =          2; // extension factor of the wafer area (pt loop 14) ;
    row.ptcut	 =        0.1; // minimum pt value for the tracker ;
    row.tanl_cut =        1.4; // maximum tanl value for the tracker ;
    row.pid_cut	 =          7; // minimum value of the pid for the tracker ;
    row.min_nrec =          5; // minimum number of tpc points in track ;
    row.max_nrec =         50; // maximum number of tpc points in track ;
    row.maxloop	 =          7; // number of loops in pt ;
    row.minlay	 =          1; // innermost layer to scan ;
    row.maxlay	 =          4; // outermost layer to scan ;
    row.mxfnd	 =         10; // maximum number of found hits per tracks ;
    row.maxass	 =         10; // maximum of sharing for a given hit ;
    row.aphi1[0]	 =       0.03; // scanning area in phi (layer 1 pt loop  1) ;
    row.aphi1[1]	 =       0.03; // scanning area in phi (layer 1 pt loop  2) ;
    row.aphi1[2]	 =       0.03; // scanning area in phi (layer 1 pt loop  3) ;
    row.aphi1[3]	 =       0.03; // scanning area in phi (layer 1 pt loop  4) ;
    row.aphi1[4]	 =       0.03; // scanning area in phi (layer 1 pt loop  5) ;
    row.aphi1[5]	 =       0.03; // scanning area in phi (layer 1 pt loop  6) ;
    row.aphi1[6]	 =       0.03; // scanning area in phi (layer 1 pt loop  7) ;
    row.aphi1[7]	 =       0.03; // scanning area in phi (layer 1 pt loop  8) ;
    row.aphi1[8]	 =      0.035; // scanning area in phi (layer 1 pt loop  9) ;
    row.aphi1[9]	 =       0.03; // scanning area in phi (layer 1 pt loop 10) ;
    row.aphi1[10]	 =       0.03; // scanning area in phi (layer 1 pt loop 11) ;
    row.aphi1[11]	 =       0.03; // scanning area in phi (layer 1 pt loop 12) ;
    row.aphi1[12]	 =       0.03; // scanning area in phi (layer 1 pt loop 13) ;
    row.aphi1[13]	 =       0.04; // scanning area in phi (layer 1 pt loop 14) ;
    row.az1[0]	 =        0.2; // scanning area in z (layer 1 pt loop  1) ;
    row.az1[1]	 =        0.2; // scanning area in z (layer 1 pt loop  2) ;
    row.az1[2]	 =        0.2; // scanning area in z (layer 1 pt loop  3) ;
    row.az1[3]	 =        0.2; // scanning area in z (layer 1 pt loop  4) ;
    row.az1[4]	 =        0.2; // scanning area in z (layer 1 pt loop  5) ;
    row.az1[5]	 =        0.2; // scanning area in z (layer 1 pt loop  6) ;
    row.az1[6]	 =        0.2; // scanning area in z (layer 1 pt loop  7) ;
    row.az1[7]	 =        0.2; // scanning area in z (layer 1 pt loop  8) ;
    row.az1[8]	 =       0.25; // scanning area in z (layer 1 pt loop  9) ;
    row.az1[9]	 =        0.2; // scanning area in z (layer 1 pt loop 10) ;
    row.az1[10]	 =        0.2; // scanning area in z (layer 1 pt loop 11) ;
    row.az1[11]	 =        0.2; // scanning area in z (layer 1 pt loop 12) ;
    row.az1[12]	 =        0.2; // scanning area in z (layer 1 pt loop 13) ;
    row.az1[13]	 =        0.3; // scanning area in z (layer 1 pt loop 14) ;
    row.aphi2[0]	 =       0.03; // scanning area in phi (layer 2 pt loop  1) ;
    row.aphi2[1]	 =       0.03; // scanning area in phi (layer 2 pt loop  2) ;
    row.aphi2[2]	 =       0.03; // scanning area in phi (layer 2 pt loop  3) ;
    row.aphi2[3]	 =       0.03; // scanning area in phi (layer 2 pt loop  4) ;
    row.aphi2[4]	 =       0.03; // scanning area in phi (layer 2 pt loop  5) ;
    row.aphi2[5]	 =       0.03; // scanning area in phi (layer 2 pt loop  6) ;
    row.aphi2[6]	 =       0.03; // scanning area in phi (layer 2 pt loop  7) ;
    row.aphi2[7]	 =      0.035; // scanning area in phi (layer 2 pt loop  8) ;
    row.aphi2[8]	 =      0.035; // scanning area in phi (layer 2 pt loop  9) ;
    row.aphi2[9]	 =       0.04; // scanning area in phi (layer 2 pt loop 10) ;
    row.aphi2[10]	 =       0.04; // scanning area in phi (layer 2 pt loop 11) ;
    row.aphi2[11]	 =       0.03; // scanning area in phi (layer 2 pt loop 12) ;
    row.aphi2[12]	 =       0.03; // scanning area in phi (layer 2 pt loop 13) ;
    row.aphi2[13]	 =       0.04; // scanning area in phi (layer 2 pt loop 14) ;
    row.az2[0]	 =        0.2; // scanning area in z (layer 2 pt loop  1) ;
    row.az2[1]	 =        0.2; // scanning area in z (layer 2 pt loop  2) ;
    row.az2[2]	 =        0.2; // scanning area in z (layer 2 pt loop  3) ;
    row.az2[3]	 =        0.2; // scanning area in z (layer 2 pt loop  4) ;
    row.az2[4]	 =        0.2; // scanning area in z (layer 2 pt loop  5) ;
    row.az2[5]	 =        0.2; // scanning area in z (layer 2 pt loop  6) ;
    row.az2[6]	 =        0.2; // scanning area in z (layer 2 pt loop  7) ;
    row.az2[7]	 =        0.3; // scanning area in z (layer 2 pt loop  8) ;
    row.az2[8]	 =        0.3; // scanning area in z (layer 2 pt loop  9) ;
    row.az2[9]	 =        0.4; // scanning area in z (layer 2 pt loop 10) ;
    row.az2[10]	 =        0.4; // scanning area in z (layer 2 pt loop 11) ;
    row.az2[11]	 =        0.2; // scanning area in z (layer 2 pt loop 12) ;
    row.az2[12]	 =        0.2; // scanning area in z (layer 2 pt loop 13) ;
    row.az2[13]	 =        0.3; // scanning area in z (layer 2 pt loop 14) ;
    row.aphi3[0]	 =       0.03; // scanning area in phi (layer 3 pt loop  1) ;
    row.aphi3[1]	 =       0.03; // scanning area in phi (layer 3 pt loop  2) ;
    row.aphi3[2]	 =       0.03; // scanning area in phi (layer 3 pt loop  3) ;
    row.aphi3[3]	 =       0.03; // scanning area in phi (layer 3 pt loop  4) ;
    row.aphi3[4]	 =       0.03; // scanning area in phi (layer 3 pt loop  5) ;
    row.aphi3[5]	 =       0.03; // scanning area in phi (layer 3 pt loop  6) ;
    row.aphi3[6]	 =       0.03; // scanning area in phi (layer 3 pt loop  7) ;
    row.aphi3[7]	 =       0.03; // scanning area in phi (layer 3 pt loop  8) ;
    row.aphi3[8]	 =       0.03; // scanning area in phi (layer 3 pt loop  9) ;
    row.aphi3[9]	 =       0.03; // scanning area in phi (layer 3 pt loop 10) ;
    row.aphi3[10]	 =       0.03; // scanning area in phi (layer 3 pt loop 11) ;
    row.aphi3[11]	 =       0.03; // scanning area in phi (layer 3 pt loop 12) ;
    row.aphi3[12]	 =       0.03; // scanning area in phi (layer 3 pt loop 13) ;
    row.aphi3[13]	 =       0.04; // scanning area in phi (layer 3 pt loop 14) ;
    row.az3[0]	 =        0.2; // scanning area in z (layer 3 pt loop  1) ;
    row.az3[1]	 =        0.2; // scanning area in z (layer 3 pt loop  2) ;
    row.az3[2]	 =        0.2; // scanning area in z (layer 3 pt loop  3) ;
    row.az3[3]	 =        0.2; // scanning area in z (layer 3 pt loop  4) ;
    row.az3[4]	 =        0.2; // scanning area in z (layer 3 pt loop  5) ;
    row.az3[5]	 =        0.2; // scanning area in z (layer 3 pt loop  6) ;
    row.az3[6]	 =        0.2; // scanning area in z (layer 3 pt loop  7) ;
    row.az3[7]	 =        0.2; // scanning area in z (layer 3 pt loop  8) ;
    row.az3[8]	 =        0.2; // scanning area in z (layer 3 pt loop  9) ;
    row.az3[9]	 =        0.2; // scanning area in z (layer 3 pt loop 10) ;
    row.az3[10]	 =        0.2; // scanning area in z (layer 3 pt loop 11) ;
    row.az3[11]	 =        0.2; // scanning area in z (layer 3 pt loop 12) ;
    row.az3[12]	 =        0.2; // scanning area in z (layer 3 pt loop 13) ;
    row.az3[13]	 =        0.3; // scanning area in z (layer 3 pt loop 14) ;
    row.aphi4[0]	 =       0.02; // scanning area in phi (layer 4 pt loop  1) ;
    row.aphi4[1]	 =       0.02; // scanning area in phi (layer 4 pt loop  2) ;
    row.aphi4[2]	 =      0.025; // scanning area in phi (layer 4 pt loop  3) ;
    row.aphi4[3]	 =      0.025; // scanning area in phi (layer 4 pt loop  4) ;
    row.aphi4[4]	 =       0.02; // scanning area in phi (layer 4 pt loop  5) ;
    row.aphi4[5]	 =       0.03; // scanning area in phi (layer 4 pt loop  6) ;
    row.aphi4[6]	 =       0.05; // scanning area in phi (layer 4 pt loop  7) ;
    row.az4[0]	 =        0.3; // scanning area in z (layer 4 pt loop  1) ;
    row.az4[1]	 =       0.35; // scanning area in z (layer 4 pt loop  2) ;
    row.az4[2]	 =        0.4; // scanning area in z (layer 4 pt loop  3) ;
    row.az4[3]	 =       0.45; // scanning area in z (layer 4 pt loop  4) ;
    row.az4[4]	 =        0.3; // scanning area in z (layer 4 pt loop  5) ;
    row.az4[5]	 =       0.45; // scanning area in z (layer 4 pt loop  6) ;
    row.az4[6]	 =        0.7; // scanning area in z (layer 4 pt loop  7) ;
    row.current_loop	 =          7; // current loop in pt ;
    row.prerefit[0]	 =         40; // flag for prerefitting (pt loop  1) ;
    row.prerefit[1]	 =         35; // flag for prerefitting (pt loop  2) ;
    row.prerefit[2]	 =         30; // flag for prerefitting (pt loop  3) ;
    row.prerefit[3]	 =         25; // flag for prerefitting (pt loop  4) ;
    row.prerefit[4]	 =         20; // flag for prerefitting (pt loop  5) ;
    row.prerefit[5]	 =         15; // flag for prerefitting (pt loop  6) ;
    row.prerefit[6]	 =          5; // flag for prerefitting (pt loop  7) ;
    //    row.bmag	 =        0.5; // magnetic field (T) ;
    row.bmag	 =    B/tesla;// magnetic field (T) ;
    row.rifc	 =     46.825; // radial position of tpc IFC (cm) ;
    row.rlgas	 =      36000; // radiation length of gas (cm) ;
    row.rlifc	 =        128; // radiation length of tpc IFC (cm) ;
    row.rltpc_gas	 =      10949; // Radiation length of TPC gas (cm) ;
    row.tifc	 =        0.3; // thickness of tpc inner field cage (cm) ;
    row.layer	 =          1; // current layer ;
    row.svt_er	 =     0.01   ; // svt resolution error ;
    m_est_ctrl->AddAt(&row,0);
    // ----------------- end of code ---------------
  }
  AddRunCont(m_est_ctrl);

  //egr 
  m_egr_egrpar = new St_egr_egrpar("egr_egrpar",2);
  {
    egr_egrpar_st row;
//
    memset(&row,0,m_egr_egrpar->GetRowSize());
    row.debug[0]	 =          1; // flags for debug printing ;
    row.debug[1]	 =          0;
    row.debug[2]	 =          0;
    row.debug[3]	 =          0;
    row.debug[4]	 =          0;
    row.debug[5]	 =          0;
    row.debug[6]	 =          0;
    row.debug[7]	 =          0;
    row.debug[8]	 =          0;
    row.minfit	 =          2; // min no. of points on track ;
    row.mxtry	 =         10; // max no. of attempts to fit ;
    //Kalman - switch on by default
    if(m_Mode == 0){
    row.usetpc = 4; 
    row.useglobal = 4;
    gMessMgr->Info() << "Kalman fitting turned ON as default" << endm;
    }
    else{
    row.useglobal = 2; // set if to usematching to be used ;
    row.usetpc	 =  1; // set if TPC used in refit ;
    gMessMgr->Info() << "Kalman fitting turned OFF" << endm;
    }
    row.useemc	 =          0; // set if EMC used in refit ;
    row.usesvt	 =          0; // set if SVT used in refit ;
    row.usetof	 =          0; // set if TOF used in refit ;
  // Helix

    row.usevert	 =          0; // Set if primary vertex used in refit ;
    row.prob[0]	 =         10; // probability cut in fit ;
    row.prob[1]	 =         10;
    row.svtchicut	 =  0; // SVT chi2 cut for adding SVT-only tracks ;
  //  row.svtchicut = m_svtchicut;
  //  row.useglobal = m_useglobal;
  //  row.usetpc    = m_usetpc;
  //  row.usesvt    = m_usesvt; 
  //  row.usevert   = m_usevert;
   m_egr_egrpar->AddAt(&row,0);
  //Use this as the GEANT pid to be used for the kalman filter for now 
   row.useglobal = 8;
   m_egr_egrpar->AddAt(&row,1);
  }
  
  AddRunCont(m_egr_egrpar);
 

  St_DataSetIter  svtpars(GetInputDB("params/svt"));
  m_svt_shape      = (St_svg_shape   *) svtpars("svgpars/shape");
  m_svt_config     = (St_svg_config  *) svtpars("svgpars/config");
  m_svt_geom       = (St_svg_geom    *) svtpars("svgpars/geom");
  m_srs_activea    = (St_srs_activea *) svtpars("srspars/srs_activea");
  m_srspar         = (St_srs_srspar  *) svtpars("srspars/srs_srspar");
  
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StMatchMaker::Make(){
  PrintInfo();  
  
  int iMake = kStOK;
  int iRes = 0, i;
  
  St_dst_track     *globtrk     = new St_dst_track("globtrk",20000);  
  AddData(globtrk);
  
  dst_track_st* globtrkPtr  = globtrk->GetTable();
  for(  i=0; i<globtrk->GetTableSize(); i++, globtrkPtr++) {
    globtrkPtr->id_start_vertex = 0;
  } 
 
  St_dst_vertex *vertex = new St_dst_vertex("vertex",1); 
  AddGarb(vertex);   
  
  St_DataSet    *tpctracks = GetInputDS("tpc_tracks");
  St_tpt_track  *tptrack   = 0;
  St_tte_eval   *evaltrk   = 0;
  St_tte_mctrk  *mctrk     = 0;
  if (tpctracks) {
    St_DataSetIter tpc_tracks(tpctracks); 
    tptrack   = (St_tpt_track  *) tpc_tracks("tptrack");
    evaltrk   = (St_tte_eval   *) tpc_tracks("evaltrk");
    mctrk     = (St_tte_mctrk  *) tpc_tracks("mctrk");
  }
  if (! evaltrk)    {evaltrk = new St_tte_eval("evaltrk",1); AddGarb(evaltrk);}
  if (! mctrk)    {mctrk = new St_tte_mctrk("mctrk",1); AddGarb(mctrk);}
  if (! tptrack)    {tptrack = new St_tpt_track("tptrack",1); AddGarb(tptrack);}
  St_DataSet    *tpchits = GetInputDS("tpc_hits");
  St_tcl_tphit     *tphit     = 0;
  St_tcl_tpcluster *tpcluster = 0;
  if (tpchits) {
    tphit     = (St_tcl_tphit     *) tpchits->Find("tphit");
    tpcluster = (St_tcl_tpcluster *) tpchits->Find("tpcluster");
  }
  if (! tpcluster)    {tpcluster = new St_tcl_tpcluster("tpcluster",1); AddGarb(tpcluster);}
 if (! tphit)    {tphit = new St_tcl_tphit("tphit",1); AddGarb(tphit);}
  
  St_DataSet     *svtracks = GetInputDS("svt_tracks");
  St_DataSet     *svthits  = GetInputDS("svt_hits");
  
  St_stk_track   *stk_track   = 0;
  St_sgr_groups  *svt_groups  = 0;
  St_scs_spt     *scs_spt     = 0;
  
  // Case svt tracking performed
  if (svtracks) {
    stk_track = (St_stk_track  *) svtracks->Find("stk_track");
    svt_groups= (St_sgr_groups *) svtracks->Find("groups");
  }
  if (svthits) {
    scs_spt     = (St_scs_spt    *)  svthits->Find("scs_spt");
  }
  
  St_est_match   *est_match    = 0;
  
  // Case silicon not there
  if (!stk_track) {stk_track = new St_stk_track("stk_track",1); AddGarb(stk_track);}
  if (!svt_groups)    {svt_groups = new St_sgr_groups("svt_groups",1); AddGarb(svt_groups);}
  if (!scs_spt)   {scs_spt = new St_scs_spt("scs_spt",1); AddGarb(scs_spt);}
  // 			Case running est tpc -> Si space point tracking
  if ( !(svtracks) && svthits ){
    svt_groups = new St_sgr_groups("svt_groups",10000); AddGarb(svt_groups);
    stk_track    = (St_stk_track *) m_GarbSet->Find("stk_tracks");
    if( !stk_track){ stk_track = new St_stk_track("stk_tracks",5000); AddGarb(stk_track);}
    est_match    = (St_est_match *) m_GarbSet->Find("est_match");
    if ( !est_match){ est_match = new St_est_match("est_match",10000); AddGarb(est_match); }
  } 

  St_DataSet *ctf = GetInputDS("ctf");
  St_ctu_cor *ctb_cor = 0;
  if (!ctf) {
    if(Debug()) gMessMgr->Debug() << "St_ctf_Maker has not been called " << endm;
  } else {
    ctb_cor = (St_ctu_cor *)ctf->Find("ctb_cor"); 
    if (! ctb_cor) {ctb_cor = new St_ctu_cor("ctb_cor",1); AddGarb(ctb_cor);}
  }
  
  St_svm_evt_match *evt_match  = new St_svm_evt_match("evt_match",3000);    AddData(evt_match);
  
  if (tptrack && svtracks) {
    
    //			svm
    iRes =  svm_am (stk_track, tptrack, m_svm_ctrl, evt_match);
    //          ==================================================
    
    if (iRes !=kSTAFCV_OK) iMake = kStWarn;
    
  } else if (tptrack && svthits){
    
    //est
    
    egr_egrpar_st *egr_egrpar = m_egr_egrpar->GetTable();
    egr_egrpar->useglobal = 3;
    St_g2t_track   *g2t_track    = 0;
    St_DataSet *geant = GetInputDS("geant");
    if (geant) {
      St_DataSetIter geantI(geant);
      g2t_track    = (St_g2t_track  *) geantI("g2t_track");
    }
    if (!g2t_track) {g2t_track = new St_g2t_track("g2t_track",1); AddGarb(g2t_track);}
    iRes = est_am(m_svt_geom, m_svt_shape,  m_srs_activea,
		  m_srspar,m_svt_config,scs_spt,tphit,
		  tptrack,evaltrk,mctrk,m_est_ctrl,
		  est_match,g2t_track,m_egr_egrpar);
    //          ==============================================
    if (iRes !=kSTAFCV_OK) iMake = kStWarn;
    if(Debug()) gMessMgr->Debug() << "Calling EST_TOGLOB2" << endm;
    iRes = est_toglob2(est_match, tphit,     tptrack, scs_spt,
		       svt_groups,stk_track,evt_match);
    //         ==================================================
    
    if (iRes !=kSTAFCV_OK) iMake = kStWarn;
    if(Debug()) gMessMgr->Debug() << "finished est_toglob2 " << endm;
#if 0     
    if(Debug()) gMessMgr->Debug() << "Calling EST_EVAL" << endm;
    iRes_est_eval = est_eval(g2t_track, tptrack, mctrk, 
			     m_est_ctrl,est_match,est_ev,scs_spt); 
    if (iRes !=kSTAFCV_OK) iMake = kStWarn;
#endif
  }
  

  // Create groups table for tpc
  St_sgr_groups *tpc_groups;
  if (tphit->GetNRows() != 0){
    tpc_groups  = new St_sgr_groups("tpc_groups",tphit->GetNRows());   
    AddData(tpc_groups); 
  }
  else {
    tpc_groups = new St_sgr_groups("tpc_groups",1);
    AddGarb(tpc_groups);
  }
  
  // egr


   tcl_tphit_st  *spc   = tphit->GetTable();
   sgr_groups_st *tgroup = tpc_groups->GetTable();
   int count = 0;

   for( i=0; i<tphit->GetNRows(); i++, spc++){
     
       tgroup->id1 = spc->track;
       tgroup->id2 = i+1;
       tgroup->ident = 0;
       count++;
       tgroup++;
   }

   tpc_groups->SetNRows(count);

  
  iRes = egr_fitter (tphit,    vertex,      tptrack , tpc_groups,
		     scs_spt,m_egr_egrpar,stk_track, svt_groups,
		     evt_match,globtrk);
  //	 ======================================================
  
  if (iRes !=kSTAFCV_OK) iMake = kStWarn;
  if (iRes !=kSTAFCV_OK) {
    gMessMgr->Warning() << "Problem on return from EGR_FITTER" << endm;}
  if(Debug()) gMessMgr->Debug() << " finished calling egr_fitter" << endm;
  


  // Fill bit map in glob trk

  spc   = tphit->GetTable();
  tgroup = tpc_groups->GetTable();
  dst_track_st * track  = globtrk->GetTable();
  
  int spt_id = 0;
  int row = 0;
  bool isset;
  
  for( i=0; i<tpc_groups->GetNRows(); i++, tgroup++){

    if( tgroup->id1 != 0){
      spt_id = tgroup->id2-1;
      row = spc[spt_id].row/100;
      row = spc[spt_id].row - row*100;

      if( row < 25){
	  isset = track[spc[spt_id].id_globtrk-1].map[0] & 1UL<<(row+7);
	  track[spc[spt_id].id_globtrk-1].map[0] |= 1UL<<(row+7);
      }
      else{
	  isset = track[spc[spt_id].id_globtrk-1].map[1] & 1UL<<(row-25);
	  track[spc[spt_id].id_globtrk-1].map[1] |= 1UL<<(row-25);
      }
      if (isset) track[spc[spt_id].id_globtrk-1].map[1] |= 1UL<<30; 
    }
  }


  // scs_spt_st *s_spc = scs_spt->GetTable();
  //  sgr_groups_st *sgroup = svt_groups->GetTable();
    
  //for( i=0; i<svt_groups->GetNRows(); i++, sgroup++){

  // if( sgroup->id1 != 0){
  //  spt_id = sgroup->id2;
  //  row = s_spc[spt_id].id_wafer/1000;
  //  if( row>7)row=7;
  //   track[s_spc[spt_id].id_globtrk-1].map[0] += (1UL<<row);

  // }
 
  //}


  return iMake;
}

//_____________________________________________________________________________

