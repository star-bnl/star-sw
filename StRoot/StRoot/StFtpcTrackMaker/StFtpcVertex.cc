// $Id: StFtpcVertex.cc,v 1.25 2009/11/23 16:38:11 fisyak Exp $
// $Log: StFtpcVertex.cc,v $
// Revision 1.25  2009/11/23 16:38:11  fisyak
// Remove dependence on dst_vertex_st
//
// Revision 1.24  2008/05/13 12:23:33  jcs
// set FTPC calibration vertex flag: 0 = fit successful, 1 = fit unsuccessful
//
// Revision 1.23  2008/05/12 22:15:54  jcs
// cleanup comments
//
// Revision 1.22  2007/01/15 08:23:02  jcs
// replace printf, cout and gMesMgr with Logger commands
//
// Revision 1.21  2004/04/06 18:59:21  oldi
// New constructor which takes input data from StVertex added.
//
// Revision 1.20  2004/02/12 19:37:11  oldi
// *** empty log message ***
//
// Revision 1.19  2004/01/28 01:41:33  jeromel
// *** empty log message ***
//
// Revision 1.18  2003/12/05 04:10:54  perev
// Fix typo, h1h.Set(&x_hist,low,upp) to h1h.Set(&y_hist,low,uppx)
//
// Revision 1.17  2003/10/23 04:17:31  perev
// Protection aginst bad TH1::Fit added
//
// Revision 1.16  2003/09/02 17:58:17  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.15  2003/05/20 18:35:10  oldi
// Cuts for vertex estimation introduced (globDca < 1 cm, multiplicity >= 200).
//
// Revision 1.14  2003/01/13 18:09:26  perev
// remove Clear of histogram. It is TNamed method.
//
// Revision 1.13  2002/11/06 13:47:59  oldi
// IFlag and Id added as data members.
// New functionality introduced (to clean up StFtpcTrackMaker.cxx).
//
// Revision 1.12  2002/10/31 13:42:55  oldi
// Code cleanup.
//
// Revision 1.11  2002/10/11 15:45:50  oldi
// Get FTPC geometry and dimensions from database.
// No field fit activated: Returns momentum = 0 but fits a helix.
// Bug in TrackMaker fixed (events with z_vertex > outer_ftpc_radius were cut).
// QA histograms corrected (0 was supressed).
// Code cleanup (several lines of code changed due to *params -> Instance()).
// cout -> gMessMgr.
//
// Revision 1.10  2002/06/04 13:43:52  oldi
// Minor change: 'west' -> 'hemisphere' (just a naming convention)
//
// Revision 1.9  2002/04/29 15:50:29  oldi
// All tracking parameters moved to StFtpcTrackingParameters.cc/hh.
// In a future version the actual values should be moved to an .idl file (the
// interface should be kept as is in StFtpcTrackingParameters.cc/hh).
//
// Revision 1.8  2002/04/05 16:51:16  oldi
// Cleanup of MomentumFit (StFtpcMomentumFit is now part of StFtpcTrack).
// Each Track inherits from StHelix, now.
// Therefore it is possible to calculate, now:
//  - residuals
//  - vertex estimations obtained by back extrapolations of FTPC tracks
// Chi2 was fixed.
// Many additional minor (and major) changes.
//
// Revision 1.7  2001/07/12 13:05:02  oldi
// QA histogram of FTPC vertex estimation is generated.
// FTPC vertex estimation is stored as pre vertex (id = 301) in any case, now.
//
// Revision 1.6  2001/01/25 15:22:39  oldi
// Review of the complete code.
// Fix of several bugs which caused memory leaks:
//  - Tracks were not allocated properly.
//  - Tracks (especially split tracks) were not deleted properly.
//  - TClonesArray seems to have a problem (it could be that I used it in a
//    wrong way). I changed all occurences to TObjArray which makes the
//    program slightly slower but much more save (in terms of memory usage).
// Speed up of HandleSplitTracks() which is now 12.5 times faster than before.
// Cleanup.
//
// Revision 1.5  2000/11/28 14:00:53  hummler
// protect vertex finder against nan
//
// Revision 1.4  2000/11/10 18:39:25  oldi
// Changes due to replacement of StThreeVector by TVector3.
// New constructor added to find the main vertex with given point array.
//
// Revision 1.3  2000/06/13 14:35:00  oldi
// Changed cout to gMessMgr->Message().
//
// Revision 1.2  2000/05/15 14:28:15  oldi
// problem of preVertex solved: if no main vertex is found (z = NaN) StFtpcTrackMaker stops with kStWarn,
// refitting procedure completed and included in StFtpcTrackMaker (commented),
// new constructor of StFtpcVertex due to refitting procedure,
// minor cosmetic changes
//
// Revision 1.1  2000/05/10 13:39:34  oldi
// Initial version of StFtpcTrackMaker
//

//----------Author:        Holm G. H&uuml;ummler, Markus D. Oldenburg
//----------Last Modified: 24.07.2000
//----------Copyright:     &copy MDO Production 1999

#include "TH1Helper.h"
#include "StFtpcVertex.hh"
#include "StFtpcTrackingParams.hh"
#include "StFtpcPoint.hh"
#include "StMessMgr.h"

#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "tables/St_g2t_vertex_Table.h"

#include "StVertex.h"

#include "TF1.h"

////////////////////////////////////////////////////////////////////////////
//                                                                        //
// StFtpcVertex class - representation of the main vertex for the FTPC.   //
//                                                                        //
// This class contains the coordinates of the main vertex plus the usual  //
// getters and setter. It is just a wrapper of the Staf tables.           //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

ClassImp(StFtpcVertex)


StFtpcVertex::StFtpcVertex()
{
  // Default constructor.

  SetX(0.);
  SetY(0.);
  SetZ(0.);

  SetXerr(0.);
  SetYerr(0.);
  SetZerr(0.);

  SetIFlag(0);
  SetId(0);
  
  return;
}


StFtpcVertex::StFtpcVertex(TObjArray *hits, TH1F *vtx_pos)
{
  // Constructor with TObjArray of ftpc points - fits vertex from points

  Int_t numFppoints = hits->GetEntriesFast();

  Double_t *rmap = new Double_t[StFtpcTrackingParams::Instance()->NumberOfPadRows()*6*numFppoints];
  Double_t *zmap = new Double_t[StFtpcTrackingParams::Instance()->NumberOfPadRows()];
  Int_t *mapMax = new Int_t[StFtpcTrackingParams::Instance()->NumberOfPadRows()*6];
  Int_t *myhist = new Int_t[StFtpcTrackingParams::Instance()->HistoBins()];
  Double_t hratio=StFtpcTrackingParams::Instance()->HistoBins()/(StFtpcTrackingParams::Instance()->HistoMax()-StFtpcTrackingParams::Instance()->HistoMin());
  
  for(Int_t iii=0; iii<StFtpcTrackingParams::Instance()->HistoBins(); iii++) {
    myhist[iii]=0;
  }
  
  for(Int_t ii=0; ii<120; ii++) mapMax[ii]=0;
  
  for(Int_t i=0; i<numFppoints;i++) {

    StFtpcPoint *thispoint = (StFtpcPoint *)hits->At(i);

    rmap[(thispoint->GetPadRow()-1)+StFtpcTrackingParams::Instance()->NumberOfPadRows()*(thispoint->GetSector()-1)+120*mapMax[(thispoint->GetPadRow()-1)+StFtpcTrackingParams::Instance()->NumberOfPadRows()*(thispoint->GetSector()-1)]]=::sqrt(thispoint->GetX()*thispoint->GetX()+thispoint->GetY()*thispoint->GetY());
    zmap[thispoint->GetPadRow()-1]=thispoint->GetZ();
    mapMax[(thispoint->GetPadRow()-1)+StFtpcTrackingParams::Instance()->NumberOfPadRows()*(thispoint->GetSector()-1)]++;
  }

  for(Int_t secI=0; secI<6; secI++) {
    
    for(Int_t rowOut=0; rowOut<19; rowOut++) {

      for(Int_t rowIn=rowOut+1; rowIn<StFtpcTrackingParams::Instance()->NumberOfPadRows(); rowIn++) {
	
	if(rowIn<StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide() || rowOut>=StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide()) {
	  
	  for(Int_t iOut=0; iOut<mapMax[rowOut+StFtpcTrackingParams::Instance()->NumberOfPadRows()*secI]; iOut++) {
	    Double_t ri=rmap[rowOut+StFtpcTrackingParams::Instance()->NumberOfPadRows()*secI+120*iOut];	    

	    for(Int_t iIn=0; iIn<mapMax[(rowIn)+StFtpcTrackingParams::Instance()->NumberOfPadRows()*secI]; iIn++) {
	      Double_t rj=rmap[rowIn+StFtpcTrackingParams::Instance()->NumberOfPadRows()*secI+120*iIn];
			  
	      if(rj>ri) {
		Double_t intersect=(rj*zmap[rowOut]-ri*zmap[rowIn])/(rj-ri);
		
		if (vtx_pos) {
		  vtx_pos->Fill(intersect);
		}

		if(intersect>StFtpcTrackingParams::Instance()->HistoMin() && intersect<StFtpcTrackingParams::Instance()->HistoMax()) {
		  myhist[int((intersect-StFtpcTrackingParams::Instance()->HistoMin())*hratio)]++;
		}
	      }
	    }
	  }
	}
      }
    }
  }

  Int_t maxBin=StFtpcTrackingParams::Instance()->HistoBins()/2, maxHeight=0;
  
  Double_t vertex = 0.;
  Double_t sigma = 0.;

  for(Int_t hindex=1; hindex<StFtpcTrackingParams::Instance()->HistoBins()-1; hindex++) {
    
    if(myhist[hindex]>maxHeight && myhist[hindex]>=myhist[hindex-1] && myhist[hindex]>=myhist[hindex+1]) {
      maxBin=hindex;
      maxHeight=myhist[hindex];
    }  
  }

  // check if Gaussfit will fail
  if((myhist[maxBin] == 0) 
     || (myhist[maxBin+1] == 0) 
     || (myhist[maxBin-1] == 0) 
     || (myhist[maxBin] <= myhist[maxBin+1]) 
     || (myhist[maxBin] <= myhist[maxBin-1])) {
    
    // use weighted mean instead 
    vertex=(myhist[maxBin]*((maxBin+0.5)/hratio+StFtpcTrackingParams::Instance()->HistoMin())
	    + myhist[maxBin-1]*((maxBin-0.5)/hratio+StFtpcTrackingParams::Instance()->HistoMin())
	    + myhist[maxBin+1]*((maxBin+1.5)/hratio+StFtpcTrackingParams::Instance()->HistoMin()))
      / (myhist[maxBin]+myhist[maxBin-1]+myhist[maxBin+1]);
  }

  else {
      
    // do gaussfit 
    sigma = sqrt (1 / ((2 * ::log((double)myhist[maxBin])) -
		       (::log((double)myhist[maxBin+1]) + 
			::log((double)myhist[maxBin-1]))));
    vertex =  ((maxBin+0.5)/hratio+StFtpcTrackingParams::Instance()->HistoMin()) + 
      sigma*sigma/(hratio*hratio) * (::log((double)myhist[maxBin+1]) - 
				     ::log((double)myhist[maxBin-1]));
  } 
		  
  delete[] myhist;
  delete[] mapMax;
  delete[] zmap;
  delete[] rmap;
  
  SetX(0.);
  SetY(0.);
  SetXerr(0.);
  SetYerr(0.);

  if(vertex*0 != 0) {
    cerr << "vertex not found, setting to 0!" << endl;
    vertex = 0.;
    sigma = 0.;
  }

  SetZ((Double_t) vertex);
  SetZerr((Double_t) sigma);

  SetIFlag(0);
  SetId(0);
}


StFtpcVertex::StFtpcVertex(St_DataSet *const geant)
{
  // Obsolete constructor taking vertex from geant.

  if (geant) {
    St_DataSetIter geantI(geant);
    St_g2t_vertex *g2t_vertex = (St_g2t_vertex *) geantI.Find("g2t_vertex");
    
    if (g2t_vertex) {
      g2t_vertex_st   *vertex = g2t_vertex->GetTable();
      SetX((Double_t) vertex->ge_x[0]);
      SetY((Double_t) vertex->ge_x[1]);
      SetZ((Double_t) vertex->ge_x[2]);
      LOG_INFO << "Using primary vertex coordinates (Geant): " << endm;
    }
    
    else {
      Double_t dummy = 0.0;
      SetX(dummy);
      SetY(dummy);
      SetZ(dummy);
      LOG_INFO << "Using primary vertex coordinates (Default): " << endm;
    }

    SetXerr(0.);
    SetYerr(0.);
    SetZerr(0.); 

    SetIFlag(0);
    SetId(0);
  }
}


StFtpcVertex::StFtpcVertex(StVertex *vertex)
{
  // constructor from StVertex
  
  SetX(vertex->position().x());
  SetY(vertex->position().y());
  SetZ(vertex->position().z());
  SetXerr(vertex->positionError().x());
  SetYerr(vertex->positionError().y());
  SetZerr(vertex->positionError().z());  

  SetIFlag(vertex->flag());
  SetId(vertex->type());
}  


StFtpcVertex::StFtpcVertex(TObjArray *tracks, StFtpcVertex *vertex, Char_t hemisphere)
{
  // constructor from track array

  TH1F x_hist("x_hist", "x position of estimated vertex", 200, -10., 10.);
  TH1F y_hist("y_hist", "y position of estimated vertex", 200, -10., 10.);
  TH1F z_hist("z_hist", "z position of estimated vertex", 200, -75., 75.);

  TF1 gauss_x("gauss_x", "gaus", -10., 10.);
  TF1 gauss_y("gauss_y", "gaus", -10., 10.);
  TF1 gauss_z("gauss_z", "gaus", -75., 75.);
  
  StFtpcVertex v;

  if (vertex == 0) {
    // set nominal vertex to 0, 0, 0 if no nominal vertex is given
    v = StFtpcVertex(0., 0., 0., 0., 0., 0.);
  }

  else {
    v = *vertex;
  }

  TH1Helper  h1h;
  for (Int_t i = 0; i < tracks->GetEntriesFast(); i++) {

    StFtpcTrack *track = (StFtpcTrack*)tracks->At(i);

    if (track->GetHemisphere() == hemisphere && track->GetDca() < StFtpcTrackingParams::Instance()->MaxDcaVertex()) {
      z_hist.Fill(track->z(track->pathLength(v.GetX(), v.GetY())));
    }
  }
   

  // fit only 20 cm in both directions of maximum
  double low,upp;
  low = z_hist.GetXaxis()->GetBinCenter(z_hist.GetMaximumBin())-20;
  upp = z_hist.GetXaxis()->GetBinCenter(z_hist.GetMaximumBin())+20;
  h1h.Set(&z_hist,low,upp);
  if (h1h.GetNonZeros()<=2) { //Bad case, fit will fail
    SetZ(h1h.GetMean());
    SetZerr(h1h.GetRMS());
  } else {  
    z_hist.Fit(&gauss_z, "QN", "", low, upp);
    SetZ(gauss_z.GetParameter(1));
    SetZerr(gauss_z.GetParameter(2));
  }

  for (Int_t i = 0; i < tracks->GetEntriesFast(); i++) {

    StFtpcTrack *track = (StFtpcTrack*)tracks->At(i);
    
    if (track->GetHemisphere() == hemisphere && track->GetDca() < StFtpcTrackingParams::Instance()->MaxDcaVertex()) {
      StThreeVector<Double_t> rv(0, 0, GetZ());
      StThreeVector<Double_t> nv(0,0,1);
      Double_t pl = track->pathLength(rv, nv);
      x_hist.Fill(track->x(pl));
      y_hist.Fill(track->y(pl));
    }
  }

  // fit only 3 cm in both directions of maximum
  low = x_hist.GetXaxis()->GetBinCenter(x_hist.GetMaximumBin())-3;
  upp = x_hist.GetXaxis()->GetBinCenter(x_hist.GetMaximumBin())+3;
  h1h.Set(&x_hist,low,upp);
  if (h1h.GetNonZeros()<=2) { //Bad case, fit will fail
    SetX(h1h.GetMean());
    SetXerr(h1h.GetRMS());
  } else {  
    x_hist.Fit(&gauss_x, "QN", "", low,upp);
    SetX(gauss_x.GetParameter(1));
    SetXerr(gauss_x.GetParameter(2));
  }
  // fit only 3 cm in both directions of maximum
  low = y_hist.GetXaxis()->GetBinCenter(y_hist.GetMaximumBin())-3;
  upp = y_hist.GetXaxis()->GetBinCenter(y_hist.GetMaximumBin())+3;
  h1h.Set(&y_hist,low,upp);
  if (h1h.GetNonZeros()<=2) { //Bad case, fit will fail
    SetY(h1h.GetMean());
    SetYerr(h1h.GetRMS());
  } else {  
    y_hist.Fit(&gauss_y, "QN", "", low, upp);
    SetY(gauss_y.GetParameter(1));
    SetYerr(gauss_y.GetParameter(2));
  }
  if (GetX()==0 && GetY()==0 && GetZ()==0) {
    SetIFlag(1); 
  } else {
    SetIFlag(0);
  }
  SetId(0);
}


StFtpcVertex::StFtpcVertex(Double_t pos[6], Int_t iFlag, Int_t id)
{
  // constructor from Doubles
  
  SetX((Double_t) pos[0]);
  SetY((Double_t) pos[1]);
  SetZ((Double_t) pos[2]);
  SetXerr((Double_t) pos[3]);
  SetYerr((Double_t) pos[4]);
  SetZerr((Double_t) pos[5]);  

  SetIFlag(iFlag);
  SetId(id);
}  


StFtpcVertex::StFtpcVertex(Double_t pos[3], Double_t err[3], Int_t iFlag, Int_t id)
{
  // constructor from Doubles with errors
  
  SetX((Double_t) pos[0]);
  SetY((Double_t) pos[1]);
  SetZ((Double_t) pos[2]);
  SetXerr((Double_t) err[0]);
  SetYerr((Double_t) err[1]);
  SetZerr((Double_t) err[2]);

  SetIFlag(iFlag);
  SetId(id);
}  


StFtpcVertex::StFtpcVertex(Double_t x, Double_t y, Double_t z, Double_t x_err, Double_t y_err, Double_t z_err, Int_t iFlag, Int_t id)
{
  // constructor from Doubles with errors
  
  SetX(x);
  SetY(y);
  SetZ(z);
  SetXerr(x_err);
  SetYerr(y_err);
  SetZerr(z_err);

  SetIFlag(iFlag);
  SetId(id);
}  


StFtpcVertex::~StFtpcVertex() 
{
  // Destructor.
  // Does nothing except destruct.
}


StFtpcVertex::StFtpcVertex(const StFtpcVertex &vertex)
{
  // Copy constructor.

  SetX(vertex.GetX());
  SetY(vertex.GetY());
  SetZ(vertex.GetZ());
  SetXerr(vertex.GetXerr());
  SetYerr(vertex.GetYerr());
  SetZerr(vertex.GetZerr());
  
  SetIFlag(vertex.GetIFlag());
  SetId(vertex.GetId());
}


StFtpcVertex& StFtpcVertex::operator=(const StFtpcVertex &vertex)
{
  // Assignment operator.

  if (this != &vertex) {  // beware of self assignment: vertex = vertex
    SetX(vertex.GetX());
    SetY(vertex.GetY());
    SetZ(vertex.GetZ());
    SetXerr(vertex.GetXerr());
    SetYerr(vertex.GetYerr());
    SetZerr(vertex.GetZerr());

    SetIFlag(vertex.GetIFlag());
    SetId(vertex.GetId());
  }

  return *this;
}


Int_t StFtpcVertex::CheckVertex()
{
  // Perform tests to see if this vertex is usable for tracking.

  if (GetIFlag() == 1) {
    // TPC  Vertex used
    LOG_INFO << "Using primary vertex from StEvent (" << *this <<  ") for Ftpc tracking." << endm;
  }
  
  else if (GetIFlag() == 101) {
    // TPC  preVertex used
    LOG_INFO << "Using Tpc preVertex estimation (" << *this << ") for Ftpc tracking."   << endm;
  }
  
  else if (GetIFlag() == 0) {
    //  No vertex found, therefore set to (0., 0., 0.) to make FTPC tracking possible.
    LOG_WARN << "No vertex found. Use (" << *this << ")." << endm;    
  }
  
  // Check for the position of the main vertex
  if (CoordIsNan()) {
    LOG_WARN << "Error in vertex calculation - no tracking." << endm;

    // No tracking!
    return 1;
  } 
  
  // Test if vertex is within the desired range.
  
  if (GetAbsZ() > StFtpcTrackingParams::Instance()->MaxVertexPosZWarning()) {
    
    if (GetAbsZ() > StFtpcTrackingParams::Instance()->PadRowPosZ(0)) {
      LOG_ERROR <<  "Found vertex lies inside of one Ftpc. No Ftpc tracking possible." << endm;
      
      // No tracking!
      return 1;   
    }
    
    else if (GetAbsZ() > StFtpcTrackingParams::Instance()->MaxVertexPosZError()) {
      LOG_ERROR << "Found vertex is more than " 
		<< StFtpcTrackingParams::Instance()->MaxVertexPosZError() 
		<< " cm off from z = 0. Ftpc tracking makes no sense." << endm;
      // No tracking!
      return 1;
    }
    
    else {
      LOG_WARN << "Found vertex is more than " 
	       << StFtpcTrackingParams::Instance()->MaxVertexPosZWarning() 
	       << " cm off from z = 0 but Ftpc tracking is still possible." << endm;
      // Do tracking.
    }
  }
  
  if (GetRadius2() >= StFtpcTrackingParams::Instance()->InnerRadius()) {
    LOG_ERROR << "Found vertex x-z-position is greater than " 
	      << StFtpcTrackingParams::Instance()->InnerRadius()
	      << " cm (inner Ftpc radius). No Ftpc tracking possible." << endm;
    // No tracking!
    return 1;
  }

  return 0;
}


ostream& operator<< (ostream& s, const StFtpcVertex &vertex)
{
  // cout
  
  return s << "x = " << vertex.GetX() << "+-" << vertex.GetXerr() << ", " 
	   << "y = " << vertex.GetY() << "+-" << vertex.GetYerr() << ", " 
	   << "z = " << vertex.GetZ() << "+-" << vertex.GetZerr();
}
