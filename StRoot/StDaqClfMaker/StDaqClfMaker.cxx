/***************************************************************************
 *
 * $Id: StDaqClfMaker.cxx,v 1.3 2002/03/08 20:33:43 jml Exp $
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
 * $Log: StDaqClfMaker.cxx,v $
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


#include "rtsSystems.h"
#include "StDaqClfMaker.h"
#include "fcfClass.hh"
#include "padfinder.h"
#include "StDbUtilities/StCoordinates.hh"

ClassImp(StDaqClfMaker);

// Structures for daq clusters...
struct fmt21_c {
  u_short x;
  u_short t;
};
struct fmt21_f {
  u_short f;
  u_short c;
};


StDaqClfMaker::StDaqClfMaker(const char *name):StMaker(name)
{
  gMessMgr->Debug() << "Constructor for StDaqClfMaker()" << endm;
  fcf = NULL;
  mCTransform = NULL;
}

StDaqClfMaker::~StDaqClfMaker() 
{
  gMessMgr->Debug() << "Destructor for StDaqClfMaker()" << endm;
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

Int_t StDaqClfMaker::Init()
{
  PrintInfo();
  StBFChain *chain = (StBFChain *)GetChain();

  char *x = chain->GetOptionString("prfin");
  if(x) sscanf(x,"%le",&mPrfin);
  else mPrfin = .26;

  x = chain->GetOptionString("trfin");
  if(x) sscanf(x,"%le",&mTrfin);
  else mTrfin = .825;

  x = chain->GetOptionString("prfout");
  if(x) sscanf(x,"%le",&mPrfout);
  else mPrfout = .48;

  x = chain->GetOptionString("trfout");
  if(x) sscanf(x,"%le",&mTrfout);
  else mTrfout = .90;

  x = chain->GetOptionString("clfDp");
  if(x) sscanf(x,"%le",&mDp);
  else mDp = .1;

  x = chain->GetOptionString("clfDt");
  if(x) sscanf(x,"%le",&mDt);
  else mDt = .2;

  x = chain->GetOptionString("clfDperp");
  if(x) sscanf(x,"%le",&mDperp);
  else mDperp = .1;

  x = chain->GetOptionString("dhSector");
  if(x) sscanf(x,"%d",&histSector);
  else histSector = -1;

  x = chain->GetOptionString("dhPadrow");
  if(x) sscanf(x,"%d",&histPadrow);
  else histPadrow = -1;

  x = chain->GetOptionString("splitRows");
  if(x) sscanf(x,"%d",&splitRows);
  else splitRows = 1;

  // Check ittf flag...
  if(chain->GetOption("ittf"))
  {
    gMessMgr->Info() << "Writing to StEvent..." << endm;
    mFill_tphit = 0;
    mFill_stevent = 1;
    mCreate_stevent = 1;
  }
  else
  {
    gMessMgr->Info() << "Writing to tphit... " << endm;
    mFill_tphit = 1;
    mFill_stevent = 0;
    mCreate_stevent = 0;
  }
  mStEvent = NULL;
  mT_tphit = NULL;

  gMessMgr->Info() << "prfin="<<mPrfin<<" prfout=" << mPrfout << endm;
  gMessMgr->Info() << "trfin="<<mTrfin<<" trfout=" << mTrfout << endm;
  gMessMgr->Info() << "dpad=" <<mDp<<endm;
  gMessMgr->Info() << "dperp="<<mDperp<<endm;
  gMessMgr->Info() << "dt="<<mDt<<endm;
  gMessMgr->Info() << "splitRows="<<splitRows<<" (Are rows to be split as on i960's)"<<endm;
  gMessMgr->Info() << "dhSector=" << histSector << endm;  
  gMessMgr->Info() << "dhPadrow=" << histPadrow << endm;

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
  fcf->cppOff = croat_cppOff;

  // Get TPC Parameters
  St_DataSet *tpc = GetDataBase("tpc");
  assert(tpc);
  St_DataSet *tsspars = tpc->Find("tsspars");
  assert(tsspars);
  m_tsspar = (St_tss_tsspar *)tsspars->Find("tsspar");
  assert(m_tsspar);
  
  tss_tsspar_st *tsspar = m_tsspar->GetTable();
  tsspar->threshold = 1;

  return StMaker::Init();
}

Int_t StDaqClfMaker::Make()
{
  PrintInfo();

  // Hack for now...
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
  u_int croat_out[6000 * 2];                 // maximum number of clusters pad padrow * 2
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
  gMessMgr->Info() << "The drift velocity used = " << mDriftVelocity << endm;


  //InitHistograms();

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
    static StDaqClfCppRow cppRowStorage;

    // We send one padrow to croat at a time.
    // If the "splitRow" flag is on, these padrows are split to simulate
    // assignment of pads to different i960's.  This task is accomplished
    // by GetCPPRow()
    for(int r=44;r>=0;r--) {
      unsigned short *adc = ((r<13) ? adc_in : adc_out);

      for(int i=0;i<3;i++) {
	//
	// Get the CPP pointers
	//
	cppRow = GetCPPRow(r,i,&cppRowStorage);
	if(!cppRow) continue;

	fcf->row = r+1;   // row starts from 1
	//	fcf->sb = ;
	//	fcf->rb = ;
	//	fcf->mz = ;

	fcf->padStart = 1000000;
	fcf->padStop = 0;

	for(int k=1;k<=MAX_PADS;k++)
	{
	  if(cppRowStorage.r[k-1][0].offset == 0xffffffff) continue;
	  if(k<fcf->padStart) fcf->padStart = k;
	  if(k>fcf->padStop) fcf->padStop = k;
	}
	
	memset(&croat_adc[0][0], 0, sizeof(croat_adc));
	memset(&croat_cpp[0][0], 0xffff, sizeof(croat_cpp));

	// Write the ADC array for this row...
	for(int pp=fcf->padStart;pp<=fcf->padStop;pp++)
	{
	  for(int ss=0;ss<MAX_SEQ;ss++)
	  {
	    if(cppRowStorage.r[pp-1][ss].offset == 0xffffffff) break;
	    
	    for(int ii=0;ii<cppRowStorage.r[pp-1][ss].length;ii++)
	    {
	      int time = ii + cppRowStorage.r[pp-1][ss].start_bin;
	      int pnt = ii + cppRowStorage.r[pp-1][ss].offset;
	      croat_adc[pp][time] = adc[pnt];
	    }
	  }
	}

	// Write the pointers for this row...
	for(int pp=fcf->padStart;pp<=fcf->padStop;pp++)
	{
	  for(int ss=0;ss<MAX_SEQ;ss++)
	  {
	    if(cppRowStorage.r[pp-1][ss].start_bin == 0xffff) break;
	    
	    croat_cpp[pp][2*ss] = cppRowStorage.r[pp-1][ss].start_bin;
	    croat_cpp[pp][2*ss+1] = (cppRowStorage.r[pp-1][ss].start_bin +
				     cppRowStorage.r[pp-1][ss].length -1);
	  }
	}
	
	u_int words = fcf->finder((u_char *)croat_adc, 
				  (u_short *)croat_cpp, 
				  (u_int *)croat_out);

	//
	// Add results to tphit table
	//
	u_int *croat_outp = croat_out;  
	u_int wrow = *croat_outp++;
	u_int nclusters = *croat_outp++;

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

	for(int c=0;c<(int)nclusters;c++)
	{
	  clustercount++;

	  int cl_x = ((fmt21_c *)croat_outp)->x;
	  int cl_t = ((fmt21_c *)croat_outp)->t;
	  croat_outp++;
	  int cl_f = ((fmt21_f *)croat_outp)->f;
	  int cl_c = ((fmt21_f *)croat_outp)->c;
	  croat_outp++;

	  saveCluster(cl_x,cl_t,cl_f,cl_c,r,sectorIdx);
	}
      }
    }

    // Run afterburner for this sector
    
    // Histograms...
    if((histSector != -1) && (histPadrow != -1))
    {
      if(sectorIdx == histSector)
      {
	BuildHistogram(histPadrow,adc_in,adc_out);
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

Int_t StDaqClfMaker::BuildCPP(int nrows, raw_row_st *row, raw_pad_st *pad, raw_seq_st *seq)
{
  int i,j,k;
  int r,p,s;
  int offset;

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

  return offset;
}

StDaqClfCppRow *StDaqClfMaker::GetCPPRow(int r, int i, StDaqClfCppRow *storage)
{
  if(splitRows) {   // split row up to 3 times as per i960
    memset(storage, 0xff, sizeof(StDaqClfCppRow));
    
    if(padfinder[r][i].mezz == 0) return NULL;   
    
    for(int p=padfinder[r][i].minpad;
	p<=padfinder[r][i].maxpad;
	p++) 
    {  
      for(int s=0;s<32;s++) 
      {
	if(cpp[r].r[p][s].start_bin == 0xffff) continue;
	storage->r[p][s] = cpp[r].r[p][s];
      }
    }
    return storage;
  }
  else {
    if(i==0) {
      memcpy(storage, &cpp[r], sizeof(StDaqClfCppRow));
      return storage;
    }
  }
  return 0;
}

Int_t StDaqClfMaker::BuildHistogram(int padrow, 
				    unsigned short *adc_in, 
				    unsigned short *adc_out)
{
  St_DataSet *d=GetDataSet("StDAQReader");
  assert(d);
  StDAQReader *dr=(StDAQReader*)(d->GetObject());
  assert(dr);

  char tag[100];
  sprintf(tag,"raw_%d_e%d_s%d_pr%d.hist",
	  dr->getRunNumber(),
	  dr->getEventNumber(),
	  histSector,
	  histPadrow);

  int r = padrow-1;

  TH2F *hist = new TH2F(tag,tag,184,0,184,512,0,512);

  for(int p=0;p<184;p++) {
    int seq = 0;
    while((seq<31) && (cpp[r].r[p][seq].start_bin != 0xffff)) {
      for(int k=0;k<cpp[r].r[p][seq].length;k++) {
	int t = cpp[r].r[p][seq].start_bin + k;
	int pnt = cpp[r].r[p][seq].offset + k;
	
	// Deal with inner vs. outer padrows
	unsigned short *adc = ((r<13) ? adc_in : adc_out);
	
	hist->Fill(p+1,t,adc[pnt]);
      }
      seq++;
    }
  }

  TFile f(tag, "RECREATE");

  hist->Write();
 
  f.Close();
  delete hist;

  return kStOK;
}

// Copies from StTpcCoordinateTransform,
//   except pad and tb need not be integers...
double StDaqClfMaker::lxFromPad(int row, double pad)
{
  double pitch = (row<14) ?
    gStTpcDb->PadPlaneGeometry()->innerSectorPadPitch() :
    gStTpcDb->PadPlaneGeometry()->outerSectorPadPitch();
 
  double pads2move = pad - (gStTpcDb->PadPlaneGeometry()->numberOfPadsAtRow(row))/2.;
  double dist2move = -pitch*(pads2move-.5);

  return(dist2move);
}

double StDaqClfMaker::lyFromRow(int row)
{
  return (gStTpcDb->PadPlaneGeometry()->radialDistanceAtRow(row));
}

// With offsets and t0 corrections
double StDaqClfMaker::lzFromTB(double timeBin, int sector, int row, int pad)
{
  double tbWidth = (1./gStTpcDb->Electronics()->samplingFrequency());
  
  double zoffset = ((row > 13) ? 
		    gStTpcDb->Dimensions()->zOuterOffset() :
		    gStTpcDb->Dimensions()->zInnerOffset());

  double t0zoffset =
    gStTpcDb->DriftVelocity()*1e-6*
    (gStTpcDb->T0(sector)->getT0(row,pad)*tbWidth);
  
  // t0zoffset = 0.0;  // This isn't done in the offline cluster finder...

  double z = 
    gStTpcDb->DriftVelocity()*1e-6*         //cm/s->cm/us
    (gStTpcDb->triggerTimeOffset()*1e6      // units are s
     + gStTpcDb->Electronics()->tZero()     // units are us 
     + (timeBin)*tbWidth ); 

  return(z - zoffset + t0zoffset);
}

// Save the cluster
void StDaqClfMaker::saveCluster(int cl_x, int cl_t, int cl_f, int cl_c, int r, int sector)
{
  tss_tsspar_st *tsspar = m_tsspar->GetTable();

  double lx = lxFromPad(r+1,((double)cl_x/64.0));
  double ly = lyFromRow(r+1);
  double lz = lzFromTB(((double)cl_t/64.0), sector, r+1, (cl_x+32)/64);
  lz -= 3.0 * tsspar->tau * mDriftVelocity * 1.0e-6;
	  
  int cl_xb = cl_x/64;
  int cl_tb = cl_t/64;

  StTpcLocalSectorCoordinate local(lx,ly,lz,sector);
  StTpcLocalCoordinate global;   // tpt does the local --> global (DB adjustments?)
  (*mCTransform)(local,global);

  // Use the tphit table structure to accumulate info...	
  tcl_tphit_st hit;
  memset(&hit,0,sizeof(hit));

  hit.cluster = clustercount;	  
  hit.flag = 0;                 // hit.flag = cl_f;
  hit.id = clustercount;
  hit.row = (r+1) + sector * 100;

  hit.q = (float)cl_c;
  double gain = (r<13) ? tsspar->gain_in : tsspar->gain_out;
  double wire_coupling = (r<13) ? tsspar->wire_coupling_in : tsspar->wire_coupling_out;
  hit.q *= tsspar->ave_ion_pot * tsspar->scale / (gain * wire_coupling);

  hit.x = global.position().x();
  hit.dx = mDp;
  hit.y = global.position().y();
  hit.dy = mDperp;
  hit.z = global.position().z();
  hit.dz = mDt;

  hit.nseq = 5;
  hit.npads = 5;

  hit.minpad = cl_xb - 2;
  if(hit.minpad < 1) 
  {
    hit.npads = cl_xb + 2;
    hit.minpad = 1;
  }
  hit.maxpad = cl_xb + 2;

  hit.ntmbk = 5;
  hit.mintmbk = cl_tb - 2;
  if(hit.mintmbk < 0)
  {
    hit.ntmbk = cl_tb + 3;
    hit.mintmbk = 0;
  }
  hit.maxtmbk = cl_tb + 2;

  hit.prf = (r>=13) ? mPrfout : mPrfin ;
  hit.zrf = (r>=13) ? mTrfout : mTrfin ;

  if(mFill_tphit)
  {
    filltphit(&hit);
  }

  if(mFill_stevent)
  {
    fillStEvent(&hit);
  }
}

void StDaqClfMaker::fillStEvent(tcl_tphit_st *hit)
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
    
  StTpcHit *tpcHit = new StTpcHit(p,e,hw,hit->q);  
  if(!mTpcHitColl->addHit(tpcHit))
  {
    assert(false);
  }
}

void StDaqClfMaker::filltphit(tcl_tphit_st *hit)
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
