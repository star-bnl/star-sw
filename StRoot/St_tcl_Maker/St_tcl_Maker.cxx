// $Id: St_tcl_Maker.cxx,v 1.40 1999/07/15 13:58:24 perev Exp $
// $Log: St_tcl_Maker.cxx,v $
// Revision 1.40  1999/07/15 13:58:24  perev
// cleanup
//
// Revision 1.39  1999/07/02 15:25:11  ward
// Add -1 so 1-512 -> 0-511.
//
// Revision 1.38  1999/04/14 13:54:56  sakrejda
// Iterator Reset() taken out of the if for pixel table creation
//
// Revision 1.37  1999/04/08 17:21:27  snelling
// calculated size for tcl_tp table and create it with this size
//
// Revision 1.36  1999/04/07 23:31:49  snelling
// calculate size for adcxyz table and create it with that size
//
// Revision 1.35  1999/03/29 23:11:41  snelling
// auxiliary hit table eliminated
//
// Revision 1.34  1999/03/24 01:17:07  sakrejda
// tss_pars added to the xyz_newtab call
//
// Revision 1.33  1999/03/17 19:23:49  sakrejda
// unpacking of raw data into adcxyz table with an on/off switch added
//
// Revision 1.33  1999/03/17 11:02:43  snellings
// switch for the pixel table and pixel table added
//
// Revision 1.32  1999/03/17 02:02:43  fisyak
// New scheme
//
// Revision 1.31  1999/03/16 00:20:39  sakrejda
// switch for the cluster morphology stuff added
//
// Revision 1.30  1999/03/15 22:31:04  sakrejda
// names of variables for the cluster morph. table changed
//
// Revision 1.29  1999/03/13 23:34:03  perev
// New makers
//
// Revision 1.28  1999/03/11 20:40:18  ward
// Add code for cluster morphology.
//
// Revision 1.27  1999/03/04 14:05:59  sakrejda
// table of sequences increased
//
// Revision 1.26  1999/03/03 00:29:04  sakrejda
// size of the tables reduced following wasted space diagnostics
//
// Revision 1.25  1999/03/02 19:50:42  sakrejda
// Histograms cleaned up
//
// Revision 1.24  1999/03/01 18:53:32  sakrejda
// hit eveluation switchable
//
// Revision 1.23  1999/02/27 23:10:48  sakrejda
// auxiliary hit table eliminated
//
// Revision 1.22  1999/02/26 17:25:30  kathy
// fix histograms
//
// Revision 1.21  1999/02/25 03:36:05  sakrejda
// Threshold lowered, was set for the test data
//
// Revision 1.20  1999/02/19 16:30:25  fisyak
// sanitary check
//
// Revision 1.19  1999/02/16 01:53:57  fisyak
// Make sure that tfs does not run if there tss
//
// Revision 1.18  1999/02/10 20:57:39  kathy
// added histograms to Maker
//
// Revision 1.17  1999/01/20 23:59:56  fisyak
// Just clean up
//
// Revision 1.16  1999/01/08 23:18:30  sakrejda
// index  table created only once and only for the mc run
//
// Revision 1.15  1999/01/02 19:08:22  fisyak
// Add ctf
//
// Revision 1.14  1998/12/18 18:37:16  fisyak
// account module changes
//
// Revision 1.13  1998/12/16 22:19:19  fisyak
// New tfs
//
// Revision 1.12  1998/12/04 15:31:50  fisyak
// Add g2t_vertex for tcl
//
// Revision 1.11  1998/10/31 00:26:22  fisyak
// Makers take care about branches
//
// Revision 1.10  1998/10/06 18:00:48  perev
// cleanup
//
// Revision 1.9  1998/09/18 14:35:31  fisyak
// Fix makers
//
// Revision 1.8  1998/09/15 20:55:27  fisyak
// Split St_DataSet -> St_DataSet + St_DataSetIter
//
// Revision 1.7  1998/08/26 12:15:10  fisyak
// Remove asu & dsl libraries
//
// Revision 1.6  1998/08/18 14:05:04  fisyak
// Add to bfc dst
//
// Revision 1.5  1998/08/14 15:25:41  fisyak
// Move out tpg from run
//
// Revision 1.4  1998/08/10 02:34:34  fisyak
// Add St_laser_Maker
//
// Revision 1.3  1998/08/07 19:34:55  fisyak
// Add St_run_Maker
//
// Revision 1.2  1998/07/21 01:04:39  fisyak
// Clean up
//
// Revision 1.1  1998/07/21 00:36:46  fisyak
// tcl and tpt
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_tcl_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <stdlib.h>
#include "St_tcl_Maker.h"
#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "St_XDFFile.h"
#include "tpc/St_tpg_main_Module.h"
#include "tpc/St_tcl_Module.h"
#include "tpc/St_tph_Module.h"
#include "tpc/St_xyz_newtab_Module.h"
#include "tpc/St_tte_hit_match_Module.h"
#include "tpc/St_tfs_g2t_Module.h"
#include "tpc/St_tfs_filt_Module.h"
#include "TH1.h"

ClassImp(St_tcl_Maker)
  
//_____________________________________________________________________________
  St_tcl_Maker::St_tcl_Maker(const char *name):
    StMaker(name),
    m_tclPixTransOn(kFALSE),
    m_tclEvalOn(kFALSE),
    m_tclMorphOn(kFALSE),
    m_tpg_detector(0),
    m_tpg_pad(0),
    m_tpg_pad_plane(0),
    m_tsspar(0),
    m_tclpar(0),
    m_type(0),
    m_tfs_fspar(0),
    m_tfs_fsctrl(0)
{
}
//_____________________________________________________________________________
St_tcl_Maker::~St_tcl_Maker(){
}
//_____________________________________________________________________________
Int_t St_tcl_Maker::Init()
{

// 		Create tables
  St_DataSet *tpc = GetDataBase("params/tpc");
  assert(tpc);
  St_DataSetIter   local(tpc);

// 		geometry parameters

  St_DataSet *tpgpar = local("tpgpar");
  assert(tpgpar);

  m_tpg_pad_plane = (St_tpg_pad_plane *) tpgpar->Find("tpg_pad_plane");
  m_tpg_detector  = (St_tpg_detector  *) tpgpar->Find("tpg_detector");
  assert ((m_tpg_pad_plane && m_tpg_detector)) ;

  m_tpg_pad = (St_tpg_pad*) tpgpar->Find("tpg_pad");
  if (!m_tpg_pad) {
    m_tpg_pad =new St_tpg_pad("tpg_pad",1); AddConst(m_tpg_pad);} 
  
  Int_t res = tpg_main(m_tpg_pad_plane,m_tpg_detector,m_tpg_pad); 
//	      ===================================================
  if(res!=kSTAFCV_OK) Warning("Init","tpg_main = %d",res);

// 		TCL parameters
  St_DataSet *tclpars = local("tclpars");
  assert(tclpars);

  m_tcl_sector_index = new St_tcl_sector_index("tcl_sector_index",1);
  m_tcl_sector_index->SetNRows(1); AddConst(m_tcl_sector_index);
  
  m_tclpar           = (St_tcl_tclpar *)         tclpars->Find("tclpar");
  m_type             = (St_tcl_tpc_index_type *) tclpars->Find("type");
  assert(m_tclpar && m_type);

// 		TSS parameters
  St_DataSet *tsspars = local("tsspars");
  assert(tsspars);
  m_tsspar = (St_tss_tsspar *) tsspars->Find("tsspar");
  assert(m_tsspar); 
  tss_tsspar_st *tsspar = m_tsspar->GetTable();
  tsspar->threshold=1;


// 		TFS parameters
  St_DataSet *tfspars = local("tfspars");
  assert(tfspars);
  m_tfs_fspar = (St_tfs_fspar *) local("tfspars/tfs_fspar");
  m_tfs_fsctrl= (St_tfs_fsctrl*) local("tfspars/tfs_fsctrl");


// 		Create Histograms

// 		for TPH pam

  m_nseq_hit   = new TH1F("TclTphitNseq","num seq in hit",200,0.5,200.5);
  m_tpc_row    = new TH1F("TclTphitRow" ,"tpc row num"   ,2345,100.5,2445.5);
  m_x_of_hit   = new TH1F("TclTphitHitX","x dist of hits",50,-200.,200.);
  m_y_of_hit   = new TH1F("TclTphitHitY","y dist of hits",50,-200.,200.);
  m_z_of_hit   = new TH1F("TclTphitHitZ","z dist of hits",50,-250.,250.);
  m_charge_hit = new TH1F("TclTphitTotChargeHit","total charge in hit",100,0.,0.00004);

  // The following quantities are known only AFTER tracking
  //  m_alpha      = new TH1F("TclTphitAlpha","crossing angle in xy",50,-200.,200.);
  // m_phi        = new TH1F("TclTphitPhi","orientation of hit wrt padplane",64,0.,64.);
  // m_lambda     = new TH1F("TclTphitLambda","dip angle(radians)",64,0.,64.);

// 		for TCL pam
  m_nseq_cluster = new TH1F("TclTpclusterNseqCluster"," num seq in cluster"			  ,100,0.5,200.5);
  m_nhits        = new TH1F("TclTpclusterNhits"      ," estimated num overlapping hits in cluster",20,-0.5,19.5);
  
  return StMaker::Init();
}

//_____________________________________________________________________________
Int_t St_tcl_Maker::FillOneRowOfMorphTable(
  int               	iClusterTbl,
  St_tcc_morphology    *morph,
  int 			padrow,
  int 			sector,
  int 			nseq,
  int 			npix,
  int 			npad,
  unsigned int 		totChargeEq1,
  int 			maxCharge,
  float 		averageCharge,
  float 		meanPadPos,
  float 		meanTimePos,
  float 		padSigma1Eq5,
  float 		timeSigma1Eq6,
  float 		padTimeSigma1Eq7,
  float 		padSigma2Eq12,
  float 		timeSigma2Eq13,
  float 		padTimeSigma2Eq14,
  float 		ecc1Eq15,
  float 		ecc2Eq16,
  float 		linEcc1Eq8,
  float 		linEcc2Eq9)
  
{
  tcc_morphology_st singlerow;

  singlerow.clusterId		= iClusterTbl+1;
  singlerow.rowNumber		= padrow;
  singlerow.sectorNumber	= sector;
  singlerow.numberOfSequences	= nseq;
  singlerow.numberOfPixels	= npix;
  singlerow.numberOfPads	= npad;
  singlerow.numberOfHits	= 0;  // will be filled later
  singlerow.totalCharge		= totChargeEq1;
  singlerow.maxCharge		= maxCharge;
  singlerow.averageCharge	= averageCharge;
  singlerow.meanPadPosition	= meanPadPos;
  singlerow.meanTimePosition	= meanTimePos;
  singlerow.padSigma1		= padSigma1Eq5;
  singlerow.timeSigma1		= timeSigma1Eq6;
  singlerow.padTimeSigma1Sq	= padTimeSigma1Eq7;
  singlerow.padSigma2		= padSigma2Eq12;
  singlerow.timeSigma2		= timeSigma2Eq13;
  singlerow.padTimeSigma2Sq	= padTimeSigma2Eq14;
  singlerow.ecc1		= ecc1Eq15;
  singlerow.ecc2		= ecc2Eq16;
  singlerow.linEcc1		= linEcc1Eq8;
  singlerow.linEcc2		= linEcc2Eq9;
  singlerow.meanX		= 0;
  singlerow.meanY		= 0;
  singlerow.meanZ		= 0;
  singlerow.clusterFlag		= 0;

  morph->AddAt(&singlerow,iClusterTbl);
  return 0; // no error
}
//_____________________________________________________________________________
Int_t St_tcl_Maker::CalculateQuadrupoleMoms(
        int padrow,
        int npad,
        int pads[TCC_PAD],
        unsigned short charge[TCC_PAD][TCC_BIN],
        unsigned int  &totChargeEq1,
        float &meanPadEq3,
        float &meanTimeEq4,
        float &padSigma1Eq5,
        float &timeSigma1Eq6,
        float &padTimeSigma1Eq7,
        float &padSigma2Eq12,
        float &timeSigma2Eq13,
        float &padTimeSigma2Eq14,
        float &ecc1Eq15,
        float &ecc2Eq16,
        float &linEcc1Eq8,
        float &linEcc2Eq9)	// Eq #s refer to SN0357
{

  int ii,jj;
  float alpha,beta,qq,centimetersPerPad,tanLinearizationFactor;

  tanLinearizationFactor=tan(LINEARIZATION);
  if(padrow<=13) centimetersPerPad=0.335; else centimetersPerPad=0.67;

  totChargeEq1=0; meanPadEq3=0; meanTimeEq4=0;
  for(ii=npad-1;ii>=0;ii--) {
    for(jj=TCC_BIN-1;jj>=0;jj--) {
      totChargeEq1+=charge[ii][jj];
      meanPadEq3  +=centimetersPerPad*pads[ii]*charge[ii][jj];
      meanTimeEq4 +=(jj+1)*CENTIMETERS_PER_TIME_BIN*charge[ii][jj];
    }
  }
  if((totChargeEq1)>0) meanPadEq3 /=(totChargeEq1); else meanPadEq3 =0;
  if((totChargeEq1)>0) meanTimeEq4/=(totChargeEq1); else meanTimeEq4=0;

  padSigma1Eq5 =0; timeSigma1Eq6 =0; padTimeSigma1Eq7 =0;
  padSigma2Eq12=0; timeSigma2Eq13=0; padTimeSigma2Eq14=0;

  for(ii=npad-1;ii>=0;ii--) {
    for(jj=TCC_BIN-1;jj>=0;jj--) {
      alpha=centimetersPerPad*pads[ii]-(meanPadEq3);
      beta=(jj+1)*CENTIMETERS_PER_TIME_BIN-(meanTimeEq4);
      qq=charge[ii][jj];
      padSigma1Eq5      += alpha        * alpha        * qq ;
      timeSigma1Eq6     += beta         * beta         * qq ;
      padTimeSigma1Eq7  += alpha        * beta         * qq ;
      padSigma2Eq12     += (alpha+beta) * (alpha+beta) * qq ;
      timeSigma2Eq13    += (alpha-beta) * (alpha-beta) * qq ;
      padTimeSigma2Eq14 += (alpha+beta) * (alpha-beta) * qq ;
    }
  }
  if((totChargeEq1)>0) padSigma1Eq5     /=(totChargeEq1); else padSigma1Eq5=0;
  if((totChargeEq1)>0) timeSigma1Eq6    /=(totChargeEq1); else timeSigma1Eq6=0;
  if((totChargeEq1)>0) padTimeSigma1Eq7 /=(totChargeEq1); else padTimeSigma1Eq7=0;
  if((totChargeEq1)>0) padSigma2Eq12    /=(totChargeEq1); else padSigma2Eq12=0;
  if((totChargeEq1)>0) timeSigma2Eq13   /=(totChargeEq1); else timeSigma2Eq13=0;
  if((totChargeEq1)>0) padTimeSigma2Eq14/=(totChargeEq1); else padTimeSigma2Eq14=0;

  padSigma2Eq12    /=2;      // This represents the sqrt(2) in Eqs 10 and 11.
  timeSigma2Eq13   /=2;      // This represents the sqrt(2) in Eqs 10 and 11.
  padTimeSigma2Eq14/=2;      // This represents the sqrt(2) in Eqs 10 and 11.

  padSigma1Eq5 =sqrt(padSigma1Eq5);
  timeSigma1Eq6=sqrt(timeSigma1Eq6);
  // sometimes this is sqrt(-): padTimeSigma1Eq7=sqrt(padTimeSigma1Eq7);
  padSigma2Eq12 =sqrt(padSigma2Eq12);
  timeSigma2Eq13=sqrt(timeSigma2Eq13);
  // sometimes this is sqrt(-): padTimeSigma2Eq14=sqrt(padTimeSigma2Eq14);

  if(padSigma1Eq5!=0 && timeSigma1Eq6 !=0) {
    ecc1Eq15=(padTimeSigma1Eq7)/((padSigma1Eq5)*(timeSigma1Eq6));
  } else ecc1Eq15=0;

  if(padSigma2Eq12!=0 && timeSigma2Eq13!=0) {
    ecc2Eq16=(padTimeSigma2Eq14)/((padSigma2Eq12)*(timeSigma2Eq13));
  } else ecc2Eq16=0;

  linEcc1Eq8=tan(LINEARIZATION*(ecc1Eq15))/tanLinearizationFactor;
  linEcc2Eq9=tan(LINEARIZATION*(ecc2Eq16))/tanLinearizationFactor;

  return 0; // OK.
}


//_____________________________________________________________________________
Int_t St_tcl_Maker::cluster_morphology( 
     	Int_t 		   sectorNumber,
        St_type_shortdata *pixel_data_in, 
        St_type_shortdata *pixel_data_out,
        St_tcl_tpcluster  *tpcluster, 
        St_tcl_tp_seq     *tpseq,
        St_tcc_morphology *morph)
{
  type_shortdata_st *pixTbl;
  unsigned short charge[TCC_PAD][TCC_BIN];
  float meanPadEq3,meanTimeEq4,padSigma1Eq5,timeSigma1Eq6,padTimeSigma1Eq7,
      padSigma2Eq12,timeSigma2Eq13;
  float padTimeSigma2Eq14,ecc1Eq15,ecc2Eq16,linEcc1Eq8,linEcc2Eq9;
  tcl_tp_seq_st *seqTbl = (tcl_tp_seq_st*) tpseq->GetTable();
  int pixBeg,pixEnd,nseq,padrow,sector,seqCnt;
  int iPixTbl,nCluster,iClusterTbl,iSeqTbl;
  int maxCharge,whichPad,whichTimeBin,npad,pads[TCC_PAD];
  int ipad,numberOfPixels;
  unsigned int totChargeEq1;
  static int lastRowPrevTime=-1;

  nCluster=tpcluster->GetNRows();
  tcl_tpcluster_st *clusterTbl = (tcl_tpcluster_st*) tpcluster->GetTable();
  for(iClusterTbl=lastRowPrevTime+1;iClusterTbl<nCluster;iClusterTbl++) {
    if(iClusterTbl%473==0) {
      printf("St_tcl_Maker::cluster_morphology Sector %2d, %3d percent done\n",
             sectorNumber,(100*(iClusterTbl-lastRowPrevTime))/(nCluster-lastRowPrevTime));
    }
    sector=clusterTbl[iClusterTbl].tpc_row/100;
    if(sector!=sectorNumber) { printf("cluster table may be corrupted.\n"); return 1; }
    padrow=clusterTbl[iClusterTbl].tpc_row%100;
    if(padrow<1||padrow>45) { printf("padrow (%d) out of range.\n",padrow); return 2; }
    iSeqTbl=clusterTbl[iClusterTbl].jseq-1;
    nseq=clusterTbl[iClusterTbl].nseq;
    if(padrow<=13) pixTbl= pixel_data_in->GetTable();
    else           pixTbl=pixel_data_out->GetTable();
    npad=0; 		// count number of pads without assuming one sequence per pad

    memset(charge,0,sizeof(charge));
    maxCharge=0; numberOfPixels=0;

    for(seqCnt=0;seqCnt<nseq;seqCnt++) { // seqCnt is not table index, iSeqTbl is.

      // Calculate whichPad, index into pads[]

      for(ipad=npad-1;ipad>=0;ipad--) { if(pads[ipad]==seqTbl[iSeqTbl].secpad) { break; } }
      if(ipad>=0) whichPad=ipad;
      else { // create a new entry in pads[]
        if(npad>=TCC_PAD) 						return 55;
        whichPad=npad; pads[npad++]=seqTbl[iSeqTbl].secpad;
      }

      pixBeg=seqTbl[iSeqTbl].jpix-1;
      pixEnd=pixBeg+seqTbl[iSeqTbl].tdc_hi-seqTbl[iSeqTbl].tdc_low;

      if(seqTbl[iSeqTbl].tpc_row!=clusterTbl[iClusterTbl].tpc_row) 	return 3;

      for(iPixTbl=pixBeg;iPixTbl<=pixEnd;iPixTbl++) {
        whichTimeBin=seqTbl[iSeqTbl].tdc_low+iPixTbl-pixBeg-1;
        if(whichTimeBin<0||whichTimeBin>=TCC_BIN) 			return 81;
        charge[whichPad][whichTimeBin]=pixTbl[iPixTbl].data;
        if(maxCharge<pixTbl[iPixTbl].data) maxCharge=pixTbl[iPixTbl].data;
        numberOfPixels++;

      } // end of iPixTbl loop

      iSeqTbl=seqTbl[iSeqTbl].next-1;

    }// end of seqCnt loop

    if(iSeqTbl!=-1) 							return 69;
    if(CalculateQuadrupoleMoms(
             padrow,npad,pads,charge,
             totChargeEq1,meanPadEq3,meanTimeEq4,
             padSigma1Eq5,timeSigma1Eq6,padTimeSigma1Eq7, padSigma2Eq12,
             timeSigma2Eq13,padTimeSigma2Eq14, ecc1Eq15,ecc2Eq16,
             linEcc1Eq8,linEcc2Eq9)) 					return 111;

    if(FillOneRowOfMorphTable(
       iClusterTbl,	morph,	padrow,	sector,	nseq,	numberOfPixels,
       npad,		totChargeEq1,	maxCharge,
       (float)((1.0*totChargeEq1)/numberOfPixels),
       meanPadEq3,	meanTimeEq4,
       padSigma1Eq5,	timeSigma1Eq6,	  padTimeSigma1Eq7,padSigma2Eq12,
       timeSigma2Eq13,	padTimeSigma2Eq14,ecc1Eq15,ecc2Eq16,
       linEcc1Eq8,	linEcc2Eq9
    )) 									return 102;
  }
  lastRowPrevTime=nCluster-1;
  return 0; // OK.
}
/*=====================================================================================
                   NOTES FOR FUNCTION cluster_morphology()
---------------+----------------+--------------+---------------------+-----------------
type           | wrapper name   | tableName    | range               | indexStep,
               |                |              |                     | index name
---------------+----------------+--------------+---------------------+-----------------
tcl_tpcluster  | tpcluster      | clusterTbl   | 1 to nok            | ++, iClusterTbl
---------------+----------------+--------------+---------------------+-----------------
tcl_tp_seq     | tpseq          | seqTable     | clusterTbl->jseq,   | tpseq[].next,
               |                |              | clusterTbl->nseq    | iSeqTbl
---------------+----------------+--------------+---------------------+-----------------
type_shortdata | pixel_data_out | pixTbl       | tpseq->jpix,        | ++,
               | or             |              | tpseq->tdc_hi-      | iPixTbl
               | pixel_data_in  |              | tpseq->tdc_low      |
---------------+----------------+--------------+---------------------+-----------------
=====================================================================================*/
//_____________________________________________________________________________
Int_t St_tcl_Maker::Make(){

  const Int_t max_hit = 400000;

  St_DataSetIter local(m_DataSet);

  if (Debug()) printf("Start of TCL Maker");

  St_tcl_tphit     *tphit     = new St_tcl_tphit("tphit",max_hit);         local.Add(tphit);
  St_tcl_tpcluster *tpcluster = new St_tcl_tpcluster("tpcluster",max_hit); local.Add(tpcluster);
  St_tcc_morphology *morph    = 0;
  if(m_tclMorphOn) {
                        morph = new St_tcc_morphology("morph",max_hit);    local.Add(morph);
  }

  St_DataSet       *sector;
  St_DataSet       *raw_data_tpc = GetInputDS("tpc_raw");
  Int_t sector_tot = 0;
  
  if (raw_data_tpc) {// Row data exits -> make clustering
    St_DataSetIter next(raw_data_tpc);
    St_raw_sec_m  *raw_sec_m = (St_raw_sec_m *) next("raw_sec_m");
    //Create the adcxyz table
    St_tfc_adcxyz *adcxyz = (St_tfc_adcxyz *) next("adcxyz");

    //counters for calculating size tables
    Int_t isumpix = 0;
    Int_t isumseq = 0;

    while ((sector=next())) {// loop over sectors
      Char_t *name= 0;
      if ((name = strstr(sector->GetName(),"Sector"))) {
	// look for the sector number
	name  = strchr(name,'_')+1; Int_t indx = atoi(name);
	if (Debug()) printf(" Sector = %d \n", indx);
	St_DataSetIter sect(sector);
	St_type_shortdata  *pixel_data_in  = (St_type_shortdata *) sect("pixel_data_in");
	St_type_shortdata  *pixel_data_out = (St_type_shortdata *) sect("pixel_data_out");
	Int_t ipin = pixel_data_in->GetNRows();
	Int_t ipout = pixel_data_out->GetNRows();
	isumpix += ipin + ipout;
	if (Debug()) cout << "Total number of pixels, " << isumpix << endl;
	St_raw_seq  *raw_seq_in  = (St_raw_seq *) sect("raw_seq_in");
	St_raw_seq  *raw_seq_out = (St_raw_seq *) sect("raw_seq_out");
	Int_t nseqin = raw_seq_in->GetNRows();
	Int_t nseqout = raw_seq_out->GetNRows();
	isumseq += nseqin + nseqout;
	if (Debug()) cout << "Total number of sequences, " << isumseq << endl;

	/* 
	   // calculate number of pixels from sequence tables
	raw_seq_st *praw_seq_in = raw_seq_in->GetTable();
	raw_seq_st *praw_seq_out = raw_seq_out->GetTable();
       	for (Int_t l=0; l < nseqin; l++, praw_seq_in++ )
	  {
	    isumseq += praw_seq_in->i + 1;
	  }
	for (l=0; l < nseqout; l++, praw_seq_out++ )
	  {
	    isumseq += praw_seq_out->i + 1;
	  }
	*/
      }
    }
    
    //calculate size for this one before creating it
    if (Debug()) cout << "making tcl_tp table with " << isumseq << " entries" << endl;
    St_tcl_tp_seq    *tpseq     = new St_tcl_tp_seq("tpseq",isumseq);      
    local.Add(tpseq);

    if (!adcxyz && m_tclPixTransOn) {  // create tfc_adcxyz Table
      if (Debug()) cout << "making adcxyz table with " << isumpix << " entries" << endl;
      adcxyz = new St_tfc_adcxyz("adcxyz",isumpix);  next.Add(adcxyz);
      adcxyz->SetNRows(0);
    }

    next.Reset();

    while ((sector=next())) {// loop over sectors
      Char_t *name= 0;
      if ((name = strstr(sector->GetName(),"Sector"))) {
	// look for the sector number
	name  = strchr(name,'_')+1; Int_t indx = atoi(name);
	if (Debug()) printf(" Sector = %d \n", indx);
	tcl_sector_index_st *tcl_sector_index = m_tcl_sector_index->GetTable();
	m_tcl_sector_index->SetNRows(1);
	tcl_sector_index->CurrentSector = indx;
	St_DataSetIter sect(sector);
	St_raw_row         *raw_row_in     = (St_raw_row *) sect("raw_row_in");
	St_raw_row         *raw_row_out    = (St_raw_row *) sect("raw_row_out");
	St_raw_pad         *raw_pad_in     = (St_raw_pad *) sect("raw_pad_in");
	St_raw_pad         *raw_pad_out    = (St_raw_pad *) sect("raw_pad_out");
	St_raw_seq         *raw_seq_in     = (St_raw_seq *) sect("raw_seq_in");
	St_raw_seq         *raw_seq_out    = (St_raw_seq *) sect("raw_seq_out");
	St_type_shortdata  *pixel_data_in  = (St_type_shortdata *) sect("pixel_data_in");
	St_type_shortdata  *pixel_data_out = (St_type_shortdata *) sect("pixel_data_out");
	
	if (m_tclPixTransOn){
	  // call the pixel translation
	  if(Debug()) printf("Starting %20s for sector %2d.\n","xyz_newtab",indx);
	  
	  Int_t res =  xyz_newtab(m_tpg_detector,
				  m_tcl_sector_index,raw_sec_m,
				  raw_row_in,raw_pad_in,raw_seq_in,pixel_data_in,
				  raw_row_out,raw_pad_out,raw_seq_out,pixel_data_out,
				  adcxyz,m_tsspar);
	  if (res!=kSTAFCV_OK) Warning("Make","xyz_newtab == %d",res);
	}
	
	// 			TCL
        if(Debug()) printf("Starting %20s for sector %2d.\n","tcl",indx);
	
	Int_t tcl_res = tcl(m_tpg_pad_plane, m_tcl_sector_index, raw_sec_m,
                            raw_row_in, raw_pad_in, raw_seq_in, pixel_data_in,
                            raw_row_out,raw_pad_out,raw_seq_out,pixel_data_out,
                            tpcluster,tpseq);
//			=======================================================
        if (tcl_res!=kSTAFCV_OK) Warning("Make","tcl == %d",tcl_res);
	if (m_tclMorphOn){
  // Create morphology table only if needed

	  if(Debug()) printf("Starting %20s for sector %2d.\n","cluster_morphology",indx);

	  Int_t tcc_res = cluster_morphology( indx, pixel_data_in, pixel_data_out,
					      tpcluster, tpseq, morph);
//			=======================================================
	  if(tcc_res) { printf("ERROR %d, tcl maker\n",tcc_res); return kStErr; }
	}
	sector_tot++;

// 			TPH
	Int_t k = indx;
	if (sector_tot == 1) k = -k;
	tcl_sector_index->CurrentSector = k;
        if(Debug()) printf("Starting %20s for sector %2d.\n","tph",indx);
	Int_t tph_res =  tph(m_tcl_sector_index,m_tclpar,m_tsspar,
			     m_tpg_pad_plane,
			     pixel_data_in,pixel_data_out,
			     tpseq,tpcluster,tphit);
//			  ==============================================
        if (tph_res!=kSTAFCV_OK) Warning("Make","tph == %d",tph_res);
      }
    }

    //=======================================================================
    // end raw data

    if (sector_tot && m_tclEvalOn) { //slow simulation exist
      if (Debug()) cout << "start run_tte_hit_match" << endl;
      St_DataSet    *geant = GetInputDS("geant");
      if (geant) {
	St_DataSetIter geantI(geant);
	St_g2t_tpc_hit *g2t_tpc_hit = (St_g2t_tpc_hit *) geantI("g2t_tpc_hit");
	if (g2t_tpc_hit){//geant data exists too

	  // create the index table, if any
	  St_tcl_tpc_index  *index = (St_tcl_tpc_index *) local("index");
	  if (!index) {index = new St_tcl_tpc_index("index",2*max_hit); local.Add(index);}
	  
	  Int_t Res_tte =  tte_hit_match(g2t_tpc_hit,index,m_type,tphit); 
	  //		       ==============================================
	  if (Res_tte !=  kSTAFCV_OK)  cout << "Problem with tte_hit_match.." << endl;
	  if (Debug()) cout << "finish run_tte_hit_match" << endl;
	}
      }
    }
  }
  else {

// 		Row data does not exit, check GEANT. if it does then use fast cluster simulation
    St_DataSet    *geant = GetInputDS("geant");
    if (geant) {
      St_DataSetIter geantI(geant);
      St_g2t_tpc_hit *g2t_tpc_hit = (St_g2t_tpc_hit *) geantI("g2t_tpc_hit");
      St_g2t_track   *g2t_track   = (St_g2t_track   *) geantI("g2t_track");
      St_g2t_vertex  *g2t_vertex  = (St_g2t_vertex  *) geantI("g2t_vertex");
      if (g2t_tpc_hit && g2t_track){
	// create the index table, if any
	St_tcl_tpc_index  *index = (St_tcl_tpc_index *) local("index");
	if (!index) {index = new St_tcl_tpc_index("index",2*max_hit); local.Add(index);}
	if (Debug()) cout << "start tfs_run" << endl;
	
	Int_t Res_tfs_g2t = tfs_g2t(g2t_tpc_hit, g2t_track, g2t_vertex,
				    m_tfs_fspar,m_tfs_fsctrl,
				    index, m_type, tphit);
	//			  ===========================================
	
	if (Res_tfs_g2t != kSTAFCV_OK){cout << "Problem running tfs_g2t..." << endl;}
	else {
	  Int_t Res_tfs_filt = tfs_filt(tphit);
//			     ================
	  if ( Res_tfs_filt !=  kSTAFCV_OK){cout << " Problem running tfs_filt..." << endl;} 
	}
	cout << "finish tfs_run" << endl;
      }
    }
  }

//		Histograms     
   MakeHistograms(); // clustering histograms
  return kStOK;
}
//_____________________________________________________________________________

//----------------------------------------------------------------------

void St_tcl_Maker::MakeHistograms() 
{
// 		Create an iterator
  St_DataSetIter tpc_hits(m_DataSet);

//		Get the table:
  St_tcl_tphit *ptphh = 0;
  St_tcl_tpcluster *ptpcl =0;
  ptphh  = (St_tcl_tphit *) tpc_hits["tphit"];
  ptpcl  = (St_tcl_tpcluster *) tpc_hits["tpcluster"];

  //  cout << " **** NOW MAKING HISTOGRAMS FOR TCL !!!!! " << endl;
  if (ptphh) {
    tcl_tphit_st *r = ptphh->GetTable();
    for(Int_t i=0; i<ptphh->GetNRows();i++,r++){
      Float_t tphit_z      = r->z;
      Float_t tphit_x      = r->x;
      Float_t tphit_y      = r->y;
//    Float_t tphit_lambda = r->lambda;
//    Float_t tphit_alpha  = r->alpha;
//    Float_t tphit_phi    = r->phi;
      Int_t tphit_nseq     = r->nseq;
      Int_t tphit_row      = r->row;
      Float_t tphit_q      = r->q;
        m_nseq_hit  ->Fill(tphit_nseq);
        m_tpc_row   ->Fill(tphit_row);
        m_x_of_hit  ->Fill(tphit_x);
        m_y_of_hit  ->Fill(tphit_y);
        m_z_of_hit  ->Fill(tphit_z);
        m_charge_hit->Fill(tphit_q);
	//        m_alpha->Fill(tphit_alpha);
	//        m_phi->Fill(tphit_phi);
	//        m_lambda->Fill(tphit_lambda);
    }
  }
  if (ptpcl) {
    tcl_tpcluster_st *r2 = ptpcl->GetTable();
    for(Int_t i=0; i<ptpcl->GetNRows();i++,r2++){
     m_nseq_cluster->Fill(r2->nseq);
     m_nhits       ->Fill(r2->nhits);
    }
  }

}

