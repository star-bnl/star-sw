/***************************************************************************
 *
 * $Id: StRchMaker.cxx,v 1.2 1999/02/12 17:29:00 fisyak Exp $
 *
 * Author: Dan Lyons
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              StRchMaker.cxx - ROOT/STAR Maker for offline chain.
 *              Start at
 *  http://rsgi01.rhic.bnl.gov/STAR/html/comp_l/root/index2.html
 *              for more info, or at
 *  http://rsgi01.rhic.bnl.gov/star/starlib/doc/www/star.html
 *              if the other one disappears for some reason
 ***************************************************************************
 *
 * $Log: StRchMaker.cxx,v $
 * Revision 1.2  1999/02/12 17:29:00  fisyak
 * Make it compiled
 *
 * Revision 1.1  1999/02/12 00:12:32  lyons
 * Trail version... untested
 *
 * comment out assert statement
#define RCH_DEBUG

 * debug macros;
 * used in first DAQ data
 *
 * Revision 1.9  1999/09/24 01:23:22  fisyak
// Data set definitions:
#include "St_g2t_rch_hit_Table.h"
#include "St_dst_rch_Table.h"

// dst tables in $STAR/include/tables/


StRchMaker::StRchMaker(const char *name,
			 const char *title)
    : StMaker(name,title) {
	: StMaker(name), mDaq(daq), mUseMatrix(matrix), mCfOnly(cf)
{

    mRchNTupleFile = 0;
    mPadPlane = 0;
    drawinit=kFALSE;
    // Create tables
    St_DataSetIter       local(gStChain->DataSet("params"));
    // Create Histograms    
    return StMaker::Init();
    mcratio        = new TH1F("cq2max","Cluster q/maxadc",50,0,5);

    *pad = ( code        & 0xff);
  if (!m_DataSet->GetList())  {
    
    // Read the Ionization
    St_DataSetIter geant(gStChain->DataSet("geant"));
    St_g2t_rch_hit *g2t_rch_hit = (St_g2t_rch_hit *) geant("g2t_rch_hit");
    if (g2t_rch_hit) {
      Int_t no_rch_hits =  g2t_rch_hit->GetNRows();
      g2t_rch_hit_st *rch_hit =  g2t_rch_hit->GetTable();
      St_dst_rch *dst_rch = new St_dst_rch("dst_rch_hit",no_rch_hits);
      m_DataSet->Add(dst_rch);
#ifdef RCH_DEBUG
      dst_rch_st dst;
      for(int i=0;i<no_rch_hits;i++,rch_hit++) {
	cout << "Hit number " << i << "of" << no_rch_hits << endl;
	cout << " id: " << rch_hit->id << endl;
	cout << " track_p: " << rch_hit->track_p << endl;
	cout << " volume_id: " << rch_hit->volume_id << endl;
	cout << " de: " << rch_hit->de << endl;
	cout << " tof: " << rch_hit->tof << endl;
	cout << " x[3]: ("
	     << rch_hit->x[0] << ","
	     << rch_hit->x[1] << ","
	     << rch_hit->x[2] << ")" << endl;
	cout << " p[3]: ("
	     << rch_hit->p[0] << ","
	     << rch_hit->p[1] << ","
	     << rch_hit->p[2] << ")" << endl;
	memcpy (&dst, rch_hit, sizeof(dst));
	dst_rch->AddAt(&dst,i);
      }
#endif
    }
  }
 return kStOK;
// 	PR(mSingleHitCollection->mTheHits[zz]);
//_____________________________________________________________________________
void StRchMaker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: StRchMaker.cxx,v 1.2 1999/02/12 17:29:00 fisyak Exp $\n");
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}
    mClusterFinder = 0;
	
#ifdef RCH_HISTOGRAM
    cout << "close the Histogram files!!!!!!" << endl;
    mRchNTupleFile->Write();
    mRchNTupleFile->Close();
}


 * Revision 1.10  2000/01/11 21:18:04  lasiuk
 * Fills new dst_rch_pixel;
 * debug macros;
 * used in first DAQ data
 *
 * Revision 1.9  1999/09/24 01:23:22  fisyak
 * Reduced Include Path
 **************************************************************************/
