/***************************************************************************
 *
 * Description:  This calculates the de/dx for SVT tracks
 *              
 ***************************************************************************
 * Adapted from Brian Lasiuk's code.
 **************************************************************************/

#include "StMessMgr.h"
#include "StSvtdEdxMaker.h"
#include "tables/St_svg_geom_Table.h"
#include "tables/St_scs_spt_Table.h"
#include "tables/St_stk_track_Table.h" 
#include "tables/St_sgr_groups_Table.h" 

#include "TH2.h"

ClassImp(StSvtdEdxMaker)

//___________________________________________________________________________________________
StSvtdEdxMaker::StSvtdEdxMaker(const char *name) : StMaker(name)
{

}

//_____________________________________________________________________________
StSvtdEdxMaker::~StSvtdEdxMaker(){
           
}

//_____________________________________________________________________________________________
Int_t StSvtdEdxMaker::Init()
{

  if(Debug()) gMessMgr->Debug() << "In StSvtdEdxMaker::Init() ... "
                               << GetName() << endm; 

  
  St_DataSetIter  svtpars(GetInputDB("svt"));
  mGeom       = (St_svg_geom    *) svtpars("svgpars/geom");

  mSvtdEdx = new TH2F("SvtdEdx"," Svt dEdx",100,0.,2.,100,0.,0.03);
  mSvtdEdx->SetYTitle("dEdx");
  mSvtdEdx->SetXTitle("p (GeV/c)");

  return  StMaker::Init();

}

//_____________________________________________________________________________

Int_t StSvtdEdxMaker::Make()
{

  if (Debug()) gMessMgr->Debug() << " In StSvtdEdxMaker::Make()" << GetName() << endm;


  St_DataSet     *svtracks = GetInputDS("est");
  St_DataSet     *svthits  = GetInputDS("svt_hits");
  
  St_stk_track   *Tracks   = 0;
  St_sgr_groups  *Groups   = 0;
  St_scs_spt     *Hits     = 0;

 // Case svt tracking performed
  if (svtracks) {
    Tracks = (St_stk_track  *) svtracks->Find("EstSvtTrk");
    Groups = (St_sgr_groups *) svtracks->Find("EstGroups");
    
  }
  if (svthits) {
    Hits     = (St_scs_spt    *)  svthits->Find("scs_spt");
  }
  

  svg_geom_st* svtGeometry = mGeom->GetTable();

   //
    // define counter variables
    //
    
    int ii=0;
    int jj=0;
    int kk=0;
    
    float u_ab_x, u_ab_y, u_ab_z;
    float norm, cos_theta;
    int wafer_no;
    
   
    //
    // Check to make sure there are tracks
    //

    cout<< "***Found "  << Tracks->GetNRows()   << " svt tracks"<< endl;
    cout << "*Found " << Groups->GetNRows() << " svt groups " << endl;


   if ( Tracks->GetNRows()== 0) {
	cerr << "No SVT tracks, aborting..." << endl;
	return 1;
    }  

   
   int   spacePoints[MAXPOINTS];
   float spacePointsCharge[MAXPOINTS];
   float pathLength[MAXPOINTS];
   float ionization[MAXPOINTS];


   stk_track_st*  svtTrack   = Tracks->GetTable();
   sgr_groups_st* svtGroups  = Groups->GetTable();
   scs_spt_st*    svtCluster = Hits->GetTable();

   //
   // calculate de/dx for tracks:
   //
   
   for (ii = 0; ii<Tracks->GetNRows(); ii++) {
     svtTrack[ii].dedx[0] = 0.;
     svtTrack[ii].dedx[1] = 0.;
     if ( svtTrack[ii].flag > 0 ){
       cout<< ii << " # of points on track " << (svtTrack[ii].nspt) << endl;
       int numberOfPointsOnTrack = svtTrack[ii].nspt;
       int trackId=(svtTrack[ii].id);  // needed as an index
       
       if( svtTrack[ii].nspt > MAXPOINTS){
	 
	 svtTrack[ii].dedx[0] = -999;
	 svtTrack[ii].dedx[1] = -999;
	 continue;
       }

       // Need to zero to nspt+1 because of clumsy way of working out unit vector later 
       for(kk=0; kk<svtTrack[ii].nspt+1; kk++) {
	 spacePoints[kk]       = 0;
	 spacePointsCharge[kk] = 0;
	 pathLength[kk]        = 0;
	 ionization[kk]        = 0;
	}
       
       //
       // get id's of points for the iith track
       // To gain access, must loop over "groups" of space points
       //
       
	//
	// Match the group and track ID numbers!
       //
       
       int index = 0;
       
       for (jj=0; jj<(Groups->GetNRows()); jj++) {
	  
	 if (trackId == svtGroups[jj].id1) {
	   spacePoints[index] = svtGroups[jj].id2-1;
	   index++;    // can incorporate this into the spacePoints[index]?
	 }
	 if (index == numberOfPointsOnTrack)
	   break;      // get out if I found all the points!
       }
       
       // Calc unit vector for first point
       u_ab_x = (svtCluster[spacePoints[1]].x[0] - 
		 svtCluster[spacePoints[0]].x[0]);
       u_ab_y = (svtCluster[spacePoints[1]].x[1] - 
		  svtCluster[spacePoints[0]].x[1]); 
       u_ab_z = (svtCluster[spacePoints[1]].x[2] - 
		 svtCluster[spacePoints[0]].x[2]); 
	norm = sqrt((u_ab_x*u_ab_x)+(u_ab_y*u_ab_y)+(u_ab_z*u_ab_z));
	
	u_ab_x /= norm;
	u_ab_y /= norm;
	u_ab_z /= norm; 
	
	//
	// reset and get the ionization from the clusters
	// using the indices above
	//
	index = 0;
	for (jj=0; jj<numberOfPointsOnTrack; jj++) {
	  
	  ionization[index]        = svtCluster[spacePoints[jj]].de[0];
	  
	  // Path length correction
	  
	  
	  for ( wafer_no=0; wafer_no<mGeom->GetNRows(); wafer_no++){
	    if( svtCluster[spacePoints[jj]].id_wafer == 
		svtGeometry[wafer_no].id) break;
	  }
	  cos_theta = u_ab_x*svtGeometry[wafer_no].n[0] +
	    u_ab_y*svtGeometry[wafer_no].n[1] +
	    u_ab_z*svtGeometry[wafer_no].n[2] ;
	  
	  
	  pathLength[index]        = fabs(0.03/cos_theta);
	  
	  spacePointsCharge[index] = (ionization[index]/pathLength[index]);
	  
	  index++;
	  
	  // unit vector for next point
	  
	  u_ab_x = (svtCluster[spacePoints[jj+1]].x[0] - 
		    svtCluster[spacePoints[jj]].x[0]);
	  u_ab_y = (svtCluster[spacePoints[jj+1]].x[1] - 
		    svtCluster[spacePoints[jj]].x[1]); 
	  u_ab_z = (svtCluster[spacePoints[jj+1]].x[2] - 
		    svtCluster[spacePoints[jj]].x[2]); 
	  norm = sqrt((u_ab_x*u_ab_x)+(u_ab_y*u_ab_y)+(u_ab_z*u_ab_z));
	  
	  u_ab_x /= norm;
	  u_ab_y /= norm;
	  u_ab_z /= norm;
	}
	
	//
	// The dE/dx calculation
	// For now, only use the logmean
	// a truncated mean will require a sort!
	//
	float dEdx=0;
	float dEdxError=0;
	for (jj=0; jj<numberOfPointsOnTrack; jj++) {
	  dEdx      += log(spacePointsCharge[jj]);
	  dEdx  += spacePointsCharge[jj];
	  dEdxError += log(spacePointsCharge[jj]);
	}
	
	//
	// Normalize to the number of Points
	//
	dEdx      /= numberOfPointsOnTrack;
	dEdxError /= numberOfPointsOnTrack;      
	
	dEdx = exp(dEdx);
	dEdxError = sqrt(exp(dEdxError));

	// Assign to tables
	//
	
	dEdxError = exp(dEdxError) - (dEdx * dEdx);
	svtTrack[ii].dedx[0] = dEdx;
	svtTrack[ii].dedx[1] = dEdxError;
	double p= (svtTrack[ii].tanl/svtTrack[ii].invpt)*
	  (svtTrack[ii].tanl/svtTrack[ii].invpt);
	p += 1/(svtTrack[ii].invpt*svtTrack[ii].invpt);
	p = sqrt(p);
	FillHistograms(svtTrack[ii].dedx[0],p);
	
      }
    }


  return kStOK;

}
//_____________________________________________________________________________


void StSvtdEdxMaker::FillHistograms(double dEdx, double p)
{

  mSvtdEdx->Fill(p,dEdx);
}
