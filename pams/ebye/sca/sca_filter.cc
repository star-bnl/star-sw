#include <math.h>
#include "sca_filter.h"


long type_of_call sca_filter_(
  TABLE_HEAD_ST   *dst_track_h,         DST_TRACK_ST         *dstTrack,
  TABLE_HEAD_ST   *sca_filter_const_h,  SCA_FILTER_CONST_ST  *filter_const,
  TABLE_HEAD_ST   *sca_switch_h,        SCA_SWITCH_ST        *sca_switch,
  TABLE_HEAD_ST   *sca_const_h,         SCA_CONST_ST         *sca_const,
  TABLE_HEAD_ST   *sca_in_h,            SCA_IN_ST            *sca_in  )   
{

  //:>--------------------------------------------------------------------
  //: ROUTINE:     sca_filter_
  //: DESCRIPTION: Filter out TPC/DST tracks from TPT/DST track tables and 
  //:              load sca_in table for Scaled Correlation Analysis 
  //:
  //: AUTHOR:      Dhammika Weerasundara -- University of Washington
  //:              dhammika@gibbs.npl.washington.edu
  //:
  //: ARGUMENTS:
  //:          IN:
  //:             dstTrack             -  DST tracks table       
  //:             dst_track_h          -  Header Structure for  dstTrack   
  //:             filter_const         -  sca_filter constant table      
  //:             sca_filter_const_h   -  Header Structure for  filter_const
  //:             sca_const            -  sca constant table    
  //:             sca_const_h          -  Header Structure for sca_const   
  //:       INOUT:
  //:         OUT:
  //:             sca_in               -  sca_in table [input to SCA]
  //:             sca_in_h             -  Header Structure for sca_in  
  //:
  //: RETURNS:    STAF Condition Value
  //:
  //: HISTORY:    
  //:      Jun 26, 1998       DW        Original
  //:
  //:>--------------------------------------------------------------------

#define PI0_M    0.13957
#define CUTMIN   0.0
#define CUTMAX   1.0


  double xmin, ymin, zmin,xmax, ymax, zmax;
  float px,py,pz,T;
  int   idstTrack=0, iGoodTrack = 0, nhits=0;
  float pt,mt,energy,rapidity,Transversity ;

  // =================  Begin Executable Code  ================== 
  
  // Initialize the sca_in table header
  sca_in_h[0].nok = 0;

  // Setup data set boundaries.   
  xmin = sca_const->xmin;
  xmax = sca_const->xmax;
  if(xmin >= xmax) {
    xmin = CUTMIN;
    xmax = CUTMAX;
  } 
  ymin = sca_const->ymin;
  ymax = sca_const->ymax;
  if(ymin >= ymax) {
    ymin = CUTMIN;
    ymax = CUTMAX;
  }
  zmin = sca_const->zmin;
  zmax = sca_const->zmax;
  if(zmin >= zmax) {
    zmin = CUTMIN;
    zmax = CUTMAX;
  }
  // set the inverse slop cut (in GeV)
  T  = filter_const->InversSlope;

  // Loop over the input tracks, filter out tracks that failed the quality 
  // cuts and load the sca_in table.
  for (idstTrack=0; idstTrack<dst_track_h[0].nok; idstTrack++){
    pt    = 1./dstTrack[idstTrack].invpt;
    pz    = pt*dstTrack[idstTrack].tanl;
    //nhits = dstTrack[idstTrack].n_point;
    // Apply track quality cuts   
    if ( pt    < filter_const->minPT       ||
	 pt    > filter_const->maxPT       )
      // nhits < filter_const->pointsPerTrack )
      continue;
    // calculate mt (in GeV/c^2) assuming pion mass.
    mt       = sqrt(pt*pt + PI0_M*PI0_M);
    energy   = sqrt(mt*mt + pz*pz);
    rapidity = 0.5*log((energy + pz)/(energy - pz));
    mt       = mt - PI0_M ;
    // For a 1-D  mt spectra analysis
    Transversity =  1.0 - exp(-mt/T);
    if ( Transversity < xmin || Transversity > xmax )
      continue;
    // passed the cuts, so store the element
    // rapidity and psi are meaningless for TPC (tpt) tracks; they are just
    // place holders.
    sca_in[iGoodTrack].index = iGoodTrack;
    sca_in[iGoodTrack].x     = Transversity;
    sca_in[iGoodTrack].y     = rapidity;
    sca_in[iGoodTrack].z     = dstTrack[idstTrack].psi;
    //sca_in[iGoodTrack].z = dstTrack[idstTrack].psi;
    // zero these out temporarily
    sca_in[iGoodTrack].evn =0; //eventSum->n_event[0];
    sca_in[iGoodTrack].run =0;
    sca_in[iGoodTrack].iflag =0;
    iGoodTrack++;
  } 
  
  // Set the number of entries in sca_in_h 
  sca_in_h->nok = iGoodTrack;

  // Check if enough tracks passed the quality cuts
  if ( iGoodTrack >= filter_const->nGoodTraks )
    return STAFCV_OK;
  else
    return STAFCV_BAD;
}  // end sca_filter

