/***************************************************************************
 *
 * $Id: FCFMaker.cxx,v 1.4 2003/09/19 15:48:13 tonko Exp $
 *
 * Author: Jeff Landgraf, BNL Feb 2002
 ***************************************************************************
 *
 * Description:  Offline version of DAQ cluster finder
 *               
 * Input:  TPC RAW table dataset
 * Output: tphit Table
 *
 ***************************************************************************
 *
 * $Log: FCFMaker.cxx,v $
 * Revision 1.4  2003/09/19 15:48:13  tonko
 * Skip row 13. Charge cut is default.
 *
 * Revision 1.3  2003/09/17 19:57:48  tonko
 * Changed name of the class from DaqClf to RTSClientFCF
 *
 * Revision 1.2  2003/09/17 19:03:59  tonko
 * Fixed a small warning in BuildCPP
 *
 * Revision 1.1  2003/09/17 18:22:26  tonko
 * First seemingly working set
 *
 * Revision 1.3  2003/09/16 15:35:40  tonko
 * Deleted misc. ifdef code
 *
 * Revision 1.2  2003/09/15 20:17:33  tonko
 * First compilable Tonkos version
 *
 * Revision 1.8  2002/11/26 21:22:03  jml
 * pad width --> 11 for inner sector
 *
 * Revision 1.7  2002/11/25 19:49:04  jml
 * Changed mintmbk, maxtmbk, ntmbk = 10 to fix problem with loss of high pt tracks
 *
 * Revision 1.6  2002/09/05 15:48:07  jml
 * Now fill the deconvolution flag to help dedx
 *
 * Revision 1.5  2002/08/22 21:31:33  jml
 * Installed new version of fcfClass
 * This version is frozen for the 2002-2003 run and will be running on the i960s
 * (assuming no new bugs are found).  Also set best values for several
 * control flags
 *
 * Revision 1.4  2002/03/20 16:41:54  jml
 * Added pad by pad t0 corrections controlled by flags
 * 	no flag    -- full pad by pad corrections
 * 	'nopadt0'  -- no pad by pad corrections
 * 	'avgpadt0' -- correct according to clusters pad
 *
 * Revision 1.3  2002/03/08 20:33:43  jml
 * Instantiate and write to StEvent if "ittf" flag is set.
 *
 * Revision 1.2  2002/03/02 15:45:07  jml
 * updated the CVS log comments
 *
 **************************************************************************/

#include "StBFChain.h"
#include "St_DataSetIter.h"
#include "St_DataSet.h"
#include "StDAQMaker/StDAQReader.h"
#include "tables/St_raw_sec_m_Table.h"
#include "tables/St_raw_pad_Table.h"
#include "tables/St_raw_row_Table.h"
#include "tables/St_raw_seq_Table.h"
#include "tables/St_tcl_tphit_Table.h"
#include "tables/St_type_shortdata_Table.h"
#include "tables/St_tpcGain_Table.h"
#include "TH1.h"
#include "TH2.h"
#include "TFile.h"
#include <TError.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <StMessMgr.h>
#include <StEvent.h>
#include <StEvent/StTpcHitCollection.h>
#include <StEvent/StTpcHit.h>
#include <StThreeVectorF.hh>


#include <rtsSystems.h>
#include <fcfClass.hh>
#include <fcfAfterburner.hh>
#include <TPC/padfinder.h>
#include <TPC/rowlen.h>

#include "FCFMaker.h"

#include "StDbUtilities/StCoordinates.hh"
#include "StDetectorDbMaker/StDetectorDbTpcRDOMasks.h"
#include "StDaqLib/TPC/fee_pin.h"

ClassImp(StRTSClientFCFMaker);


StRTSClientFCFMaker::StRTSClientFCFMaker(const char *name):StMaker(name)
{
  gMessMgr->Debug() << "Constructor for StRTSClientFCFMaker()" << endm;
  fcf = NULL;
  mCTransform = NULL;
}

StRTSClientFCFMaker::~StRTSClientFCFMaker() 
{
  gMessMgr->Debug() << "Destructor for StRTSClientFCFMaker()" << endm;
  if(fcf != NULL)
  {
    delete fcf;
    fcf = NULL;
  }

  if(mCTransform != NULL)
  {
    delete mCTransform;
    mCTransform = NULL;
  }
}

static FILE *ff, *dataf ;
static class fcfAfterburner fcf_after ;

Int_t StRTSClientFCFMaker::Init()
{
  PrintInfo();

  //
  //  Disable the command line options in favor of hardcoded 
  //  values.   Here is an example of how to make command
  //  line options though...
  //
  //   StBFChain *chain = (StBFChain *)GetChain();
  //
  //   char *x = chain->GetOptionString("prfin");
  //   if(x) sscanf(x,"%le",&mPrfin);
  //   else mPrfin = .26;
  //

  mPrfin = .26;         // hardcoded pad response functions
  mTrfin = .825;
  mPrfout = .48;
  mTrfout = .90;

  mDp = .1;             // hardcoded errors
  mDt = .2;
  mDperp = .1;

  splitRows = 1;        // split padrows as if real DAQ on i960s
  doT0Corrections = 1;  // do the t0 corrections
  doGainCorrections = 0; // done by St_tpcdaq_Maker - shouldn't be!!! Tonko
  doZeroTruncation = 0; // don't 
  fillDeconFlag = 1;

  mCreate_stevent = 0;  // Use  StEvent for ittf
  mFill_stevent = 0;
  mFill_tphit = 1;

  mStEvent = NULL;
  mT_tphit = NULL;

  gMessMgr->Info() << "prfin="<<mPrfin<<" prfout=" << mPrfout << endm;
  gMessMgr->Info() << "trfin="<<mTrfin<<" trfout=" << mTrfout << endm;
  // gMessMgr->Info() << "dpad=" <<mDp<<endm;
  // gMessMgr->Info() << "dperp="<<mDperp<<endm;
  // gMessMgr->Info() << "dt="<<mDt<<endm;
  gMessMgr->Info() << "splitRows="<<splitRows<<" (Are rows to be split as on i960's)"<<endm;

  // Croat initializations
  //
  // croat_adcOff & croat_cppOff are static
  //
  fcf = new fcfClass(TPC_ID,NULL);

  for(int i=0;i<MAX_PADS_EVER+1;i++)
  {
    croat_adcOff[i] = (unsigned int)(&croat_adc[i][0]) - (unsigned int)(&croat_adc[0][0]);
    croat_cppOff[i] = (unsigned int)(&croat_cpp[i][0]) - (unsigned int)(&croat_cpp[0][0]);
  }
  fcf->adcOff = croat_adcOff;
  fcf->cppOff = (short unsigned int *)croat_cppOff;
  fcf->maxClusters = MAX_CLUSTERS;

  memset(t0Corr, 0, sizeof(t0Corr));
  for(int i=0;i<MAX_PADS_EVER+1;i++) gainCorr[i] = 64;   // == 1.0 !!

  fcf->t0Corr = t0Corr;
  fcf->gainCorr = gainCorr;

  // Tonko: added startFlags
  memset(startFlags,0,sizeof(startFlags)) ;
  fcf->startFlags = startFlags ;

  // Get TPC Parameters
  St_DataSet *tpc = GetDataBase("tpc");
  assert(tpc);
  St_DataSet *tsspars = tpc->Find("tsspars");
  assert(tsspars);
  m_tsspar = (St_tss_tsspar *)tsspars->Find("tsspar");
  assert(m_tsspar);
  
  tss_tsspar_st *tsspar = m_tsspar->GetTable();
  tsspar->threshold = 1;

  dataf = fopen("fcf.raw","w") ;
  ff = fopen("fcf.dta","w") ;

  return StMaker::Init();
}

// Tonko: this should hold aaaall the gain/T0 correction tables
// so that we don't have to redo them every time...

Int_t StRTSClientFCFMaker::InitRun(int run)
{
	fprintf(stderr,"StRTSClientFCFMaker::InitRun called with run %u...\n",run) ;
	
	return kStOK ;
}

Int_t StRTSClientFCFMaker::Make()
{
  PrintInfo();

  // Hack for now untill ittf is in more complete shape...
  if(mCreate_stevent)
  {
    
    if(mStEvent != NULL)
    {
      delete mStEvent;
      mStEvent = NULL;
    }

    St_DataSetIter ods(m_DataSet);
    mStEvent = new StEvent();
    ods.Add(mStEvent);
  }

  St_DataSet *rawData;
  St_DataSet *sector;
// Tonko: BUG fixed - missing parenthesis
  static u_int croat_out[(MAX_CLUSTERS+2) * 2];                 // maximum number of clusters pad padrow * 2
  int sz;
  clustercount=0;
  
  // Coordinate transformer
  if(!gStTpcDb)
  {
    gMessMgr->Error() << "There is no gStTpcDb pointer\n" << endm;
    exit(0);
  }

  // need a coordinate transformer...
  if(!mCTransform)
  {
    mCTransform = new StTpcCoordinateTransform(gStTpcDb);
  }

  mDriftVelocity = gStTpcDb->DriftVelocity();
  // gMessMgr->Info() << "The drift velocity used = " << mDriftVelocity << endm;

  rawData = (St_DataSet *)GetInputDS("tpc_raw");
  St_DataSetIter rawIter(rawData);

  if(mFill_tphit)
  {
    // Set up tphit dataset...If exists use old one, else create it.
    St_DataSetIter outputDataSet(m_DataSet);
  
    mT_tphit = (St_tcl_tphit *)outputDataSet("tphit");
    if(mT_tphit == NULL)
    {
      mT_tphit = new St_tcl_tphit("tphit",10);
      outputDataSet.Add(mT_tphit);
    }
  }

  if(mFill_stevent)
  {
    mTpcHitColl = new StTpcHitCollection();
    assert(mTpcHitColl);
  }
 
  fprintf(ff,"-1 0.0 0.0 0.0 1.0e-05 0 0 0 0 0\n") ;
 
  while((sector = rawIter()) != NULL) {

    // The dataset tpc_sec_m is also in the "tpc_raw" dataset, don't use it...
    // Only use Datasets labeled "Sector_xx"

    if(!strstr(sector->GetName(),"Sector_")) continue;

    // Get the table structures...
    St_DataSetIter sectorIter(sector);
    St_raw_row *Trow_in = (St_raw_row *)sectorIter.Find("raw_row_in");
    St_raw_row *Trow_out = (St_raw_row *)sectorIter.Find("raw_row_out");
    St_raw_pad *Tpad_in = (St_raw_pad *)sectorIter.Find("raw_pad_in");
    St_raw_pad *Tpad_out = (St_raw_pad *)sectorIter.Find("raw_pad_out");
    St_raw_seq *Tseq_in = (St_raw_seq *)sectorIter.Find("raw_seq_in");
    St_raw_seq *Tseq_out = (St_raw_seq *)sectorIter.Find("raw_seq_out");
    St_type_shortdata *Tadc_in = (St_type_shortdata *)sectorIter.Find("pixel_data_in");
    St_type_shortdata *Tadc_out = (St_type_shortdata *)sectorIter.Find("pixel_data_out");

    // Get the c arrays for this sector
    raw_row_st *row_in = Trow_in->GetTable();
    raw_row_st *row_out = Trow_out->GetTable();
    raw_pad_st *pad_in = Tpad_in->GetTable();
    raw_pad_st *pad_out = Tpad_out->GetTable();
    raw_seq_st *seq_in = Tseq_in->GetTable();
    raw_seq_st *seq_out = Tseq_out->GetTable();
    unsigned short *adc_in = (unsigned short *)Tadc_in->GetTable();
    unsigned short *adc_out = (unsigned short *)Tadc_out->GetTable();

    memset(&cpp[0],0xff,sizeof(cpp));

    int sectorIdx = atoi(&(sector->GetName()[7]));

    sz = 0;
    sz += BuildCPP(Trow_in->GetNRows(), row_in, pad_in, seq_in);
    sz += BuildCPP(Trow_out->GetNRows(), row_out, pad_out, seq_out);

    // Run Clusterfinder...for this sector
    StDaqClfCppRow *cppRow;
    // Tonko: changed to pointer
    static StDaqClfCppRow *cppRowStorage ;
    //static StDaqClfCppRow cppRowStorage;

    // We send one padrow to croat at a time.
    // If the "splitRow" flag is on, these padrows are split to simulate
    // assignment of pads to different i960's.  This task is accomplished
    // by GetCPPRow()

    static u_int *fcf_resptr[46][3] ;
    memset(fcf_resptr,0,sizeof(fcf_resptr)) ;

    for(int r=44;r>=0;r--) {
      // skip row 13!
      if(r==12) continue ;

      // does both Gain & T0 corrections (depending on flags)
      getGainCorrections(sectorIdx, r);     // places corrections into fcf->gainCorr[1...lastpad]

      unsigned short *adc = ((r<13) ? adc_in : adc_out);

      cppRow = &cpp[r] ;
      cppRowStorage = &cpp[r] ;

      u_int *res_ptr = croat_out ;
      u_int *rows_count = croat_out ;
      u_int *croat_outp ;
      u_int nclusters ;

      *rows_count = 0 ;	// row count 0 as default...
	
      res_ptr++ ;	// advance space...

      int i ;
      for(i=0;i<3;i++) {
	//
	// Get the CPP pointers
	//
	//cppRow = GetCPPRow(r,i,&cppRowStorage);
	//cppRow = cpp ;
	//if(!cppRow) continue;

	fcf->row = r+1;   // row starts from 1
	fcf->padStart = 1000000;
	fcf->padStop = 0;

	memset(startFlags,0,sizeof(startFlags)) ;
// Tonko: CODE CHANGE - I'd like to do this differently
	int start, stop ;

	if(!splitRows) {
		if(i>=1) break ;	// allow only one pass...

		start = fcf->padStart = 1 ;
		stop = fcf->padStop = tpc_rowlen[r+1] ;
	}
	else {	// broken row
		if(padfinder[r][i].rdo == 0) break ;	// no more row fragments
		start = fcf->padStart = padfinder[r][i].minpad ;
		stop = fcf->padStop = padfinder[r][i].maxpad ;

	}


       if(start == 1) fcf->startFlags[start] |= FCF_ROW_EDGE ;
       else fcf->startFlags[start] |= FCF_BROKEN_EDGE ;

       if(stop == tpc_rowlen[r+1]) fcf->startFlags[stop] |= FCF_ROW_EDGE ;
       else fcf->startFlags[stop] |= FCF_BROKEN_EDGE ;

       for(int k=start;k<=stop;k++) {
              if(fcf->gainCorr[k] == 0) {
                      fcf->startFlags[k] |= FCF_DEAD_EDGE ;
                      if((k-1)>=start) fcf->startFlags[k-1] |= FCF_DEAD_EDGE ;
                      if((k+1)<=stop) fcf->startFlags[k+1] |= FCF_DEAD_EDGE ;
              }
       }


	memset(&croat_adc[0][0], 0, sizeof(croat_adc));
// Tonko: small bug - 2nd argument is a byte
	memset(&croat_cpp[0][0], 0xff, sizeof(croat_cpp));

	// Write the ADC array for this row...
	for(int pp=fcf->padStart;pp<=fcf->padStop;pp++)
	{
	  for(int ss=0;ss<MAX_SEQ;ss++)
	  {
	    if(cppRowStorage->r[pp-1][ss].offset == 0xffffffff) break;
	    
	    for(int ii=0;ii<cppRowStorage->r[pp-1][ss].length;ii++)
	    {
	      int time = ii + cppRowStorage->r[pp-1][ss].start_bin;
	      int pnt = ii + cppRowStorage->r[pp-1][ss].offset;
	      croat_adc[pp][time] = adc[pnt];
		//fprintf(dataf,"%d %d %d %d %d %d\n",sectorIdx,r+1,pp,time,adc[pnt],log10to8_table[adc[pnt]]) ;
	    }
	  }
	}

	// Write the pointers for this row...
	for(int pp=fcf->padStart;pp<=fcf->padStop;pp++)
	{
	  for(int ss=0;ss<MAX_SEQ;ss++)
	  {
	    if(cppRowStorage->r[pp-1][ss].start_bin == 0xffff) break;
	    
	    croat_cpp[pp][2*ss] = cppRowStorage->r[pp-1][ss].start_bin;
	    croat_cpp[pp][2*ss+1] = (cppRowStorage->r[pp-1][ss].start_bin +
				     cppRowStorage->r[pp-1][ss].length -1);
	  }
	}

	u_int words = fcf->finder((u_char *)croat_adc, 
				  (u_short *)croat_cpp, 
				  (u_int *)res_ptr);


	//
	// Add results to tphit table
	//
	croat_outp = res_ptr;  
	u_int wrow = *croat_outp++;
	nclusters = *croat_outp++;

	if(words == 1)
	{
	  wrow = r+1;
	  nclusters = 0;
	}

	if((int)wrow != r+1)
	{
	  gMessMgr->Error() << "Fatal error: padrow "<<wrow<<" does not match "<< r+1 << endm;
	  exit(0);
	}

	if((nclusters * 2 + 2 != words) &&
	   (nclusters != 0))
	{
	  gMessMgr->Error() << "Fatal error: nclusters="<<nclusters<<" words="<<words<<endm;
	  exit(0);
	}

	if(nclusters) {
		fcf_resptr[r+1][i] = res_ptr ;
		(*rows_count)++ ;
		res_ptr += 2+2*nclusters ;	// advance pointer
	}
      }

      fcfHit h ;
      fcf_after.burn(fcf_resptr[r+1]) ;
      while(fcf_after.next(&h)) {
        clustercount++ ;
        saveCluster(h.pad,h.tm,h.f,h.c,h.p1,h.p2,h.t1,h.t2,r,sectorIdx);
      } 
    }
  }
  
  // Save the hit collection to StEvent...
  if(mFill_stevent)
  {
    mStEvent->setTpcHitCollection(mTpcHitColl);
    mTpcHitColl = NULL;    // I don't control the pointer anymore...
  }

  gMessMgr->Info() <<  "Done with make:  "<<clustercount<<" clusters found" << endm;
  return kStOK;
}

// Build cpp array from tables
// Note: padrows[1-13] point to the inner sectors pixel buffer 
//       padrows[14-45] point to the outer sectors pixel buffer

Int_t StRTSClientFCFMaker::BuildCPP(int nrows, raw_row_st *row, raw_pad_st *pad, raw_seq_st *seq)
{
  int i,j,k;
  int r,p,s;
  int offset;

  offset = -1 ;
  for(i=0;i<nrows;i++) {
    int pad_off = row[i].ipad;
    r = row[i].RowId;

    for(j=0;j<row[i].npad;j++) {
      int seq_off = (row[i].iseq + pad[pad_off + j].SeqOffset);
      p = pad[pad_off + j].PadId;

      offset = (row[i].ipixel +
		pad[pad_off + j].PadOffset);

      for(k=0;k<pad[pad_off + j].nseq;k++) {
	int tb = seq[seq_off+k].m + ((k>=pad[pad_off + j].SeqModBreak) ? 256 : 0);
	int n = seq[seq_off+k].i;

	s = k;
	
	if( (r>45) || (p>184) || (s>31) ||
	    (r<1)  || (p<1)   || (s<0)) {
	  gMessMgr->Error() << "got an illegal sequence row=" << r << ", pad=" << p << ", seq=" << s << endm;
	}

	if(n==0) {
	  gMessMgr->Error() << "Got an illegal CPP of length 0" << endm;
	}

	cpp[r-1].r[p-1][s].start_bin = tb;
	cpp[r-1].r[p-1][s].offset = offset;
	cpp[r-1].r[p-1][s].length = n+1;

	offset += n+1;
      } 
    }
  }

  if(offset == -1) {	// Huh?
	  	gMessMgr->Error() << "Offset not updated???" << endm ;
  }

  return offset;
}

StDaqClfCppRow *StRTSClientFCFMaker::GetCPPRow(int r, int i, StDaqClfCppRow *storage)
{
  if(splitRows) {   // split row up to 3 times as per i960
    int found = 0;

    memset(storage, 0xff, sizeof(StDaqClfCppRow));
    
    if(padfinder[r][i].mezz == 0) return NULL;   
    
    for(int p=padfinder[r][i].minpad;
	p<=padfinder[r][i].maxpad;
	p++) 
    {  
      for(int s=0;s<32;s++) 
      {
	if(cpp[r].r[p-1][s].start_bin == 0xffff) continue;
	storage->r[p-1][s] = cpp[r].r[p-1][s];
	found = 1;
      }
    }
    if(found)
      return storage;
    else 
    {
      return NULL;
    }
  }
  else {
    if(i==0) {
      memcpy(storage, &cpp[r], sizeof(StDaqClfCppRow));
      return storage;
    }
  }
  return 0;
}

// Copies from StTpcCoordinateTransform,
//   except pad and tb need not be integers...
double StRTSClientFCFMaker::lxFromPad(int row, double pad)
{
  double pitch = (row<14) ?
    gStTpcDb->PadPlaneGeometry()->innerSectorPadPitch() :
    gStTpcDb->PadPlaneGeometry()->outerSectorPadPitch();
 
  double pads2move = pad - (gStTpcDb->PadPlaneGeometry()->numberOfPadsAtRow(row))/2.;
  double dist2move = -pitch*(pads2move-.5);

  //dist2move = -pitch*(pads2move) ;
  return(dist2move);
}

double StRTSClientFCFMaker::lyFromRow(int row)
{
  return (gStTpcDb->PadPlaneGeometry()->radialDistanceAtRow(row));
}

// With offsets and t0 corrections
double StRTSClientFCFMaker::lzFromTB(double timeBin, int sector, int row, int pad)
{
  double tbWidth = (1./gStTpcDb->Electronics()->samplingFrequency());
  
  double zoffset = ((row > 13) ? 
		    gStTpcDb->Dimensions()->zOuterOffset() :
		    gStTpcDb->Dimensions()->zInnerOffset());

  double t0zoffset=0.0;

  //   The padbypad t0 corrections are done inside the cluster finder...
  //   using the values in fcf->t0Corr
  //
  //   if(doPadT0Corrections)  
  //   {
  //     t0zoffset =
  //       gStTpcDb->DriftVelocity()*1e-6*
  //       (gStTpcDb->T0(sector)->getT0(row,pad)*tbWidth);
  //   }

  double z = 
    gStTpcDb->DriftVelocity()*1e-6*         //cm/s->cm/us
    (gStTpcDb->triggerTimeOffset()*1e6      // units are s
     + gStTpcDb->Electronics()->tZero()     // units are us 
     + (timeBin)*tbWidth ); 

  return(z - zoffset + t0zoffset);
}


// Tonko: this doesc both Gain _and_ T0 corrections!
// "sector" starts from 1 whereas "row" starts from 0 (argh!)
void StRTSClientFCFMaker::getGainCorrections(int sector, int row)
{
  // copy this routine from St_tpcdaq_Maker()
  int pad;

  TDataSet *tpc_calib  = GetDataBase("Calibrations/tpc"); assert(tpc_calib);
  
  St_tpcGain *gainObj = (St_tpcGain*) tpc_calib->Find("tpcGain"); assert(gainObj);
  
  assert(gainObj->GetNRows()==24);
  
  tpcGain_st *gains = gainObj->GetTable(); assert(gains);
  
  assert(sector>=1&&sector<=24);
  
  static StDetectorDbTpcRDOMasks* mask=0;
  static int tRDOFromRowAndPad[45][182];
  if(!mask) {
    mask = StDetectorDbTpcRDOMasks::instance();
    assert(mask);
    for(int tiFee=0;tiFee<182;tiFee++) {
      for(int tiPin=0;tiPin<32;tiPin++) {
        if(row_vs_fee[tiFee][tiPin]!=0 && pad_vs_fee[tiFee][tiPin]!=0) {
          tRDOFromRowAndPad[(row_vs_fee[tiFee][tiPin]-1)]
	    [(pad_vs_fee[tiFee][tiPin]-1)]=
	    rdo_vs_fee[tiFee][tiPin];
        }
      }
    }
  }
  
  // NOTE: pad starts from 0 (as well as row)
  for(pad=0;pad<tpc_rowlen[row+1];pad++) {
    double gain, t0;

    if(mask->isOn(sector,tRDOFromRowAndPad[row][pad]))
      gain = gains[sector-1].Gain[row][pad];
    else
      gain = 0.0;

// Tonko: HACK! to eliminate gain calc. in FCF to cross-check TCL
// but _still_ kill bad channels
    if(!doGainCorrections) if(gain > 0.001) gain = 1.0 ;

    // gainCorr starts from 1!
    gainCorr[pad+1] = (int)(gain*64.0 + 0.5);

    // Tonko: added T0 correction here...
    // NOTE: it seems that getT0 wants row,pad to start from 1 whereas the previous Gain
    // 		started from 0 - need to crosscheck!
    if(doT0Corrections) {
      t0 = gStTpcDb->T0(sector)->getT0(row+1,pad+1);
    }
    else t0 = 0.0 ;

    // t0Corr starts from 1!
    t0Corr[pad+1] = (short)(gain*fabs(t0)*64.0 + 0.5) ;	// this is convoluted with the gain!
    if(t0 < 0.0) t0Corr[pad+1] *= -1 ;

  }

}

// Save the cluster
void StRTSClientFCFMaker::saveCluster(int cl_x, int cl_t, int cl_f, int cl_c, int p1, int p2, int t1, int t2, int r, int sector)
{
  tss_tsspar_st *tsspar = m_tsspar->GetTable();

  double lx = lxFromPad(r+1,(((double)(cl_x))/64.0));
  double ly = lyFromRow(r+1);
  double lz = lzFromTB((((double)(cl_t))/64.0), sector, r+1, (cl_x+32)/64);
  lz -= 3.0 * tsspar->tau * mDriftVelocity * 1.0e-6;   // correct for convolution lagtime
	  

  StTpcLocalSectorCoordinate local(lx,ly,lz,sector);
  StTpcLocalCoordinate global;   // tpt does the local --> global (DB adjustments?)
  (*mCTransform)(local,global);

  // Use the tphit table structure to accumulate info...	
  tcl_tphit_st hit;
  memset(&hit,0,sizeof(hit));

  hit.cluster = clustercount;	  

  // Filling in the flag causes very bad tracking performance
  // for some events.  I don't know why.
  if(fillDeconFlag == 0)
    hit.flag = 0;     
  else
  {
    if( (cl_f & FCF_DOUBLE_PAD) ||
	(cl_f & FCF_DOUBLE_T))
    {
      hit.flag = 1;
    }
  }

  
  hit.id = clustercount;
  hit.row = (r+1) + sector * 100;

// Tonko: move all to double.
// BREAKS the equality with TCL but it's TCL's problem.
  double tmp_q ;

  tmp_q = (double)cl_c  ;
	
	
//  hit.q = (float)cl_c;
  double gain = (r<13) ? tsspar->gain_in : tsspar->gain_out;
  double wire_coupling = (r<13) ? tsspar->wire_coupling_in : tsspar->wire_coupling_out;

  tmp_q *= ((double)tsspar->ave_ion_pot * (double)tsspar->scale)/(gain*wire_coupling) ;

//fprintf(stderr,"%f goes to %e for row %d\n",(float)cl_c,tmp_q,r) ;

  hit.q = tmp_q;
  hit.x = global.position().x();
  hit.dx = mDp;
  hit.y = global.position().y();
  hit.dy = mDperp;
  hit.z = global.position().z();
  hit.dz = mDt;

  if(doZeroTruncation)
  {
    if((hit.z < 0) && (sector <=12))  // sector 1..12 have positive z
      return;
    if((hit.z > 0) && (sector > 12))
      return;
  }
    

  hit.nseq = 5;

  hit.minpad = p1;
  hit.maxpad = p2;

  hit.npads = p2 - p1 + 1 ;

  hit.mintmbk = t1;
  hit.maxtmbk = t2;

  hit.ntmbk = hit.maxtmbk - hit.mintmbk + 1;

  hit.prf = (r>=13) ? mPrfout : mPrfin ;
  hit.zrf = (r>=13) ? mTrfout : mTrfin ;


#define XXXX_WRITEFILE_XXXX
#ifdef XXXX_WRITEFILE_XXXX
if(sector==24 || sector==12 || sector==6 || sector==18) {
fprintf(ff,"%d %f %f %f %e %d %d %d %d %d\n",
	hit.row,hit.x,hit.y,hit.z,hit.q,hit.minpad,hit.maxpad,hit.mintmbk,hit.maxtmbk,cl_f) ;
#ifdef AsdASD
  fprintf(ff, 
	  "%d %d %d %d %d "
	  "%d %e %e %e %e "
	  "%e %f %e %f %e "
	  "%f %e %e %e %e "
	  "%e %d %d %d %d "
	  "%d %d %d %d %d\n",
	  hit.cluster,
	  hit.flag,
	  hit.id,
	  hit.id_globtrk,
	  hit.track,
	  hit.truncTag,
	  hit.alpha,
	  hit.dalpha,
	  hit.lambda,
	  hit.q,
	  hit.dq,
	  hit.x,
	  hit.dx,
	  hit.y,
	  hit.dy,
	  hit.z,
	  hit.dz,
	  hit.phi,
	  hit.prf,
	  hit.zrf,
	  hit.dedx,
	  hit.row/100,
	  hit.row%100,
	  hit.nseq,
	  hit.npads,
	  hit.minpad,
	  hit.maxpad,
	  hit.ntmbk,
	  hit.mintmbk,
	  hit.maxtmbk);
#endif
}
#endif

  if(mFill_tphit)
  {
    filltphit(&hit);
  }

  if(mFill_stevent)
  {
    fillStEvent(&hit);
  }
}

void StRTSClientFCFMaker::fillStEvent(tcl_tphit_st *hit)
{
  assert(mStEvent);
  assert(mTpcHitColl);
  
  StThreeVectorF p(hit->x,hit->y,hit->z);
  StThreeVectorF e(hit->dx,hit->dy,hit->dz);
  
  unsigned int hw = 1;         // detid_tpc
  hw += (hit->row/100 << 4);   // sector
  hw += (hit->row%100 << 9);   // row
  hw += (hit->npads   << 15);  // npads
  hw += (hit->ntmbk   << 22);  // ntmbks...

  // Tonko: this didn't compile in Jun03 !?
  //StTpcHit *tpcHit = new StTpcHit(p,e,hw,hit->q);  
  //if(!mTpcHitColl->addHit(tpcHit))
  //{
  //  assert(false);
  //}
}

void StRTSClientFCFMaker::filltphit(tcl_tphit_st *hit)
{
  assert(mT_tphit);

  int nAlloc = mT_tphit->GetTableSize();
  int nUsed = mT_tphit->GetNRows();
  if(nUsed>nAlloc-10)
  {
    mT_tphit->ReAllocate(Int_t(nAlloc*1.2+10));
  }
  mT_tphit->AddAt(hit, nUsed);
}

