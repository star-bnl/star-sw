 /***************************************************************************
 *
 * $Id: StRchMaker.cxx,v 1.8 1999/07/21 13:33:55 gans Exp $
 *
 * Author: Jon Gans
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              StRchMaker.cxx - ROOT/STAR Maker for offline chain.
 ***************************************************************************
 *
 * $Log: StRchMaker.cxx,v $
 * Revision 1.8  1999/07/21 13:33:55  gans
 * *** empty log message ***
 *
 * Revision 1.8 1999/07/19 00:00:00 gans
 * makes 3-d histogram to check data 	
 * Revision 1.7  1999/07/15 13:57:22  perev
 * cleanup
 *	 
 * Revision 1.6  1999/03/20 22:00:19  perev
 * new maker schema
 *
 * Revision 1.5  1999/02/12 23:59:30  lyons
 * Hopefully working version.  Compiles, but untested.
 *
 * Revision 1.4  1999/02/12 21:47:16  lyons
 * *** empty log message ***
 *
 * Revision 1.3  1999/02/12 18:28:53  lyons
 * Another revision, merge fisyak additions with a couple changes...
 * trying to get to compile...
 *
 * Revision 1.2  1999/02/12 17:29:00  fisyak
 * Make it compiled
 *
 * Revision 1.1  1999/02/12 00:12:32  lyons
 * Trail version... untested
 *
 * comment out assert statement
 *
//#define RCH_DEBUG

 * debug macros;
 * used in first DAQ data
 *
 * Revision 1.9  1999/09/24 01:23:22  fisyak
// Data set definitions:
#include "St_g2t_rch_hit_Table.h"
// Internal Rch

// dst tables in $STAR/include/tables/
#include "tables/St_g2t_rch_hit_Table.h"
#include "tables/St_dst_rch_pixel_Table.h"

StRchMaker::StRchMaker(const char *name) : StMaker(name) {
	: StMaker(name), mDaq(daq), mUseMatrix(matrix), mCfOnly(cf)
{


#ifdef RCH_HISTOGRAM   // in the .h file
    mRchNTupleFile = 0;
    mPadPlane = 0;
    
#endif
    drawinit=kFALSE;
    // Create tables
    // Create Histograms    
  cerr << "before declare\n";
  
  hist = new TH3S("hist","test hist",200,0.,200,250,-250,0,150,-75,75);
  
  cerr << "after declare\n";
  return StMaker::Init();
    mcratio        = new TH1F("cq2max","Cluster q/maxadc",50,0,5);

			   unsigned long* pad, unsigned long* row, unsigned long* adc)
{
    *pad = ( code        & 0xff);

 St_DataSet *dst = GetDataSet("dst");    
 St_DataSetIter dstI(dst);  
 
 St_g2t_rch_hit *g2t_rch_hit = (St_g2t_rch_hit *) dstI["g2t_rch_hit"];

  if (!g2t_rch_hit) return kStWarn;
  cout << " found g2t_rch_hit table" << endl;

  Int_t no_rch_hits =  g2t_rch_hit->GetNRows();
  cout << " no rows in g2t_rch_hit = " << no_rch_hits << endl;
  if (!no_rch_hits) return kStWarn;

  g2t_rch_hit_st *rch_hit =  g2t_rch_hit->GetTable();
  assert(rch_hit);

  int maxX = 0 ;
  int maxY = 0 ;
  int maxZ = 0 ;

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
      
      /*    if(rch_hit->x[0] < maxX)
	maxX = rch_hit->x[0];

       if(rch_hit->x[1] < maxY)
	maxY = rch_hit->x[1];
 
       if(rch_hit->x[2] < maxZ)
	maxZ = rch_hit->x[2];
      */
       	
      hist->Fill3(rch_hit->x[0],rch_hit->x[1],rch_hit->x[2],1);
      hist->Draw();
    }		

  no_rch_hits =  g2t_rch_hit->GetNRows();
  cout << " no rows in g2t_rch_hit = " << no_rch_hits << endl;

  cerr << "Max (x,y,z) " << maxX <<"  " << maxY << "  " << "  " << maxZ ;

 return kStOK;
// 	PR(mSingleHitCollection->mTheHits[zz]);
	}
//     for(int zz=0; zz<mSimpleHitCollection->mTheHits.size(); zz++) {
// 	PR(mSimpleHitCollection->mTheHits[zz]);
//     }
	mTheRichReader = 0;
  printf("**************************************************************\n");
  printf("* $Id: StRchMaker.cxx,v 1.8 1999/07/21 13:33:55 gans Exp $\n");
	}
    AddData(new St_ObjectSet("StRichEvent", richCollection));
  printf("* $Id: StRchMaker.cxx,v 1.8 1999/07/21 13:33:55 gans Exp $\n");
}
//-----------------------------------------------------------------
  printf("* $Id: StRchMaker.cxx,v 1.8 1999/07/21 13:33:55 gans Exp $\n");
  printf("**************************************************************\n");
Int_t StRchMaker::Finish() {  
    cout << "Delete the cluster finder" << endl;
    delete mClusterFinder;

//-----------------------------------------------------------------
















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
