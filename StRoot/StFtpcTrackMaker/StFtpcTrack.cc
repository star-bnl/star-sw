// $Id: StFtpcTrack.cc,v 1.9 2000/09/07 11:35:39 jcs Exp $
// $Log: StFtpcTrack.cc,v $
// Revision 1.9  2000/09/07 11:35:39  jcs
// Set flag,id_start_vertex,nrec,nmax,nfit for FTPC primary tracks
//
// Revision 1.8  2000/07/18 21:22:16  oldi
// Changes due to be able to find laser tracks.
// Cleanup: - new functions in StFtpcConfMapper, StFtpcTrack, and StFtpcPoint
//            to bundle often called functions
//          - short functions inlined
//          - formulas of StFormulary made static
//          - avoid streaming of objects of unknown size
//            (removes the bunch of CINT warnings during compile time)
//          - two or three minor bugs cured
//
// Revision 1.7  2000/07/17 14:54:22  jcs
// save results of constrained fit
//
// Revision 1.6  2000/07/12 11:58:40  jcs
// calculate and save FTPC track parameters for unconstrained fit
//
// Revision 1.5  2000/07/03 12:42:57  jcs
// save (pre)Vertex id and unconstrained fit results
//
// Revision 1.4  2000/06/07 11:48:56  oldi
// Added function GetEta().
// In SetProperties(Bool_t usage, Int_t tracknumber): calculation of
// mRowsWithPoints added.
// CalculateNMax() changed. It calculates now the (arbitrary) angle and radius
// in the inner and outer padrow.
//
// Revision 1.3  2000/05/12 12:59:15  oldi
// removed delete operator for mSegment in StFtpcConfMapper (mSegment was deleted twice),
// add two new constructors for StFtpcTracker to be able to refit already existing tracks,
// minor cosmetics
//
// Revision 1.2  2000/05/11 15:14:50  oldi
// Changed class names *Hit.* due to already existing class StFtpcHit.cxx in StEvent
//
// Revision 1.1  2000/05/10 13:39:22  oldi
// Initial version of StFtpcTrackMaker
//

//----------Author:        Holm G. H&uuml;mmler, Markus D. Oldenburg
//----------Last Modified: 18.07.2000
//----------Copyright:     &copy MDO Production 1999

#include "StFormulary.hh"
#include "StFtpcTrack.hh"
#include "StFtpcVertex.hh"
#include "StFtpcConfMapPoint.hh"
#include "StFtpcMomentumFit.hh"

////////////////////////////////////////////////////////////////////////////////////
//                                                                                //
// StFtpcTrack class - representation of one track for the FTPC trackers.         //
//                                                                                //
// This class contains all data members which are the output of the FTPC tracker. //
//                                                                                //
////////////////////////////////////////////////////////////////////////////////////


ClassImp(StFtpcTrack)


StFtpcTrack::StFtpcTrack()
{
  // Default constructor.
  // Creates a ObjArray of the hits belonging to the track.

  SetDefaults();
}


StFtpcTrack::StFtpcTrack(Int_t tracknumber)
{
  // Same as default constructor except that the track number is set.
  
  SetDefaults();
  SetTrackNumber(tracknumber);
}


StFtpcTrack::StFtpcTrack(fpt_fptrack_st *track_st, TClonesArray *hits, Int_t tracknumber)
{
  // Constructor if STAF table track is given.
  // Creates a ObjArray of the hits belonging to the track.

  mPoints = new TObjArray(0, 0);
  mPointNumbers = new MIntArray();

  SetTrackNumber(tracknumber);

  Int_t id;

  for(Int_t row=9; row>=0; row--) {
    id = track_st->hitid[row];

    if (id != -1) {
      mPointNumbers->AddLast(id - 1);
      
      if (hits) {
	mPoints->AddLast(hits->At(id - 1));
      }
    }
  }
  
  mP.setX(track_st->p[0]);
  mP.setY(track_st->p[1]);
  mP.setZ(track_st->p[2]);

  mV.setX(track_st->v[0]);
  mV.setY(track_st->v[1]);
  mV.setZ(track_st->v[2]);

  mQ = track_st->q;
  mChiSq[0] = track_st->chisq[0];
  mChiSq[1] = track_st->chisq[1];
  mTheta = track_st->theta;

  ComesFromMainVertex(track_st->flag);

  SetRadius(1./track_st->curvature);
  SetCenterX(0.);
  SetCenterY(0.);
  SetAlpha0(0.);

  SetNMax(track_st->nmax);
  SetDca(track_st->impact);
}


StFtpcTrack::~StFtpcTrack()
{
  // Destructor.

  delete mPoints;
  delete mPointNumbers;
}


void StFtpcTrack::SetDefaults()
{
  // Executes the default setup for the track.

    mPoints = new TObjArray(0, 0);
  mPointNumbers = new MIntArray();

  mTrackNumber = -1;

  SetRadius(0.);
  SetCenterX(0.);
  SetCenterY(0.);
  SetAlpha0(0.);
  SetPid(0);
  SetNMax(0);

  ComesFromMainVertex(false);

  mP.setX(0.);
  mP.setY(0.);
  mP.setZ(0.);

  mV.setX(0.);
  mV.setY(0.);
  mV.setZ(0.);
 
  mQ = 0;
  mChiSq[0] = 0.;
  mChiSq[1] = 0.;
  mTheta = 0.;
  mDca = 0.;

  return;
}


void StFtpcTrack::SetTrackNumber(Int_t number) 
{
  // Sets the tracknumber.
  // If the track has already some hits assigned the track number of the hits is also set.

  mTrackNumber = number;

  for (Int_t i = 0; i < mPoints->GetEntriesFast(); i++) {
    ((StFtpcPoint*)mPoints->At(i))->SetTrackNumber(number);    
  }

  return;
}


void StFtpcTrack::AddPoint(StFtpcPoint* point)
{
  // Adds a given point to the track.

  mPointNumbers->AddLast(point->GetHitNumber());
  mPoints->AddLast(point);
  point->SetUsage(true);
  point->SetTrackNumber(GetTrackNumber());

  return;
}


Double_t StFtpcTrack::CalcAlpha0()
{
  // Calculates the starting angle (with respect to the x-axis) of xt.
  
  Double_t asin_arg;
  Double_t alpha0;

  StFtpcConfMapPoint *trackpoint = (StFtpcConfMapPoint *)mPoints->First();
  asin_arg = StFormulary::CheckASinArg((trackpoint->GetYt() - GetCenterY())/GetRadius());

  if (trackpoint->GetXt() >= GetCenterX() && trackpoint->GetYt() > GetCenterY()) {
    alpha0 = TMath::ASin(asin_arg);
  }

  else if (trackpoint->GetXt() < GetCenterX() && trackpoint->GetYt() >= GetCenterY()) {
    alpha0 = -TMath::ASin(asin_arg) + TMath::Pi();
  }

  else if (trackpoint->GetXt() <= GetCenterX() && trackpoint->GetYt() < GetCenterY()) {
    alpha0 = TMath::ASin(-asin_arg) +  TMath::Pi();
  }

  else if (trackpoint->GetXt() > GetCenterX() && trackpoint->GetYt() <= GetCenterY()) {
    alpha0 = -TMath::ASin(-asin_arg) + 2 * TMath::Pi();
  }

  return alpha0;
}


void StFtpcTrack::SetProperties(Bool_t usage, Int_t tracknumber) 
{
  // Sets number of next hit. Counting is started from the vertex. (The tracker finds hits on tracks vice versa!)
  // Sets the usage of all points belonging to this track to the value of fUsage and
  // sets the track number of all points belonging to this track to the value of fTrackNumber.

  mRowsWithPoints = 0;
  
  for (Int_t i = 0; i < mPoints->GetEntriesFast(); i++) {    
    StFtpcConfMapPoint *p = (StFtpcConfMapPoint *)mPoints->At(i);

    if (usage == true) {
      mRowsWithPoints += (Int_t)TMath::Power(2, ((p->GetPadRow()-1)%10)+1);

      if (i != 0) {
	p->SetNextHitNumber(((StFtpcConfMapPoint *)mPoints->At(i-1))->GetHitNumber());
      }

      else {
	p->SetNextHitNumber(-1);
      }
    }    

    else {
      p->SetNextHitNumber(-1);
    }
    
    p->SetUsage(usage);
    p->SetTrackNumber(tracknumber);
  }

  return;
}


void StFtpcTrack::SetPointDependencies()
{
  // Sets number of next hit. Counting is started from the vertex. (The tracker finds hits on tracks vice versa!)

  mRowsWithPoints = 0;
  
  for (Int_t i = 0; i < mPoints->GetEntriesFast(); i++) {    
    StFtpcConfMapPoint *p = (StFtpcConfMapPoint *)mPoints->At(i);

    mRowsWithPoints += (Int_t)TMath::Power(2, ((p->GetPadRow()-1)%10)+1);
    
    if (i != 0) {
      p->SetNextHitNumber(((StFtpcConfMapPoint *)mPoints->At(i-1))->GetHitNumber());
    }
    
    else {
      p->SetNextHitNumber(-1);
    }
  }    

  return;
}


void StFtpcTrack::CalculateNMax()
{
  // Calculates the max. possible number of points on this track.
  // Up to now this is only a approximation. The calculated value would be right if:
  //   - the track would be a straight line
  //
  // In addition this funtion calculates the radius and the angle of a potential 
  // track point in the first (inner) and the last (outer) pad row.

  // This should be read from the appropriate table, of course.
  Double_t z_row[] = {162.75, 171.25, 184.05, 192.55, 205.35, 213.85, 226.65, 235.15, 247.95, 256.45};
  Short_t nmax = 0;
  
  StFtpcConfMapPoint *lastpoint  = (StFtpcConfMapPoint *)this->GetHits()->Last();
  StFtpcConfMapPoint *firstpoint = (StFtpcConfMapPoint *)this->GetHits()->First();
  
  Double_t z2 = firstpoint->GetZ();
  Double_t z1 = lastpoint->GetZ();
  Double_t x2 = firstpoint->GetX();
  Double_t x1 = lastpoint->GetX();
  Double_t r2 = TMath::Sqrt((firstpoint->GetX() * firstpoint->GetX()) + (firstpoint->GetY() * firstpoint->GetY())); 
  Double_t r1 = TMath::Sqrt((lastpoint->GetX() * lastpoint->GetX()) + (lastpoint->GetY() * lastpoint->GetY())); 

  // These values should go into an .idl file.
  Double_t outer_radius =  30.00;
  Double_t inner_radius =   8.00;
  Double_t r, x;
    
  for (Int_t i = 0; i < 10; i++) {
    r = (r2 - r1) / (z2 - z1) * (TMath::Sign(z_row[i], z1) - z1) + r1;
    x = (x2 - x1) / (z2 - z1) * (TMath::Sign(z_row[i], z1) - z1) + x1;
    
    if (i == 0) {
      mRFirst = r;
      mAlphaFirst = TMath::ACos(x/r);
    }
   
    if (i == 9) {
      mRLast = r;
      mAlphaLast = TMath::ACos(x/r);
    }
    
    if (r < outer_radius && r > inner_radius) {
      nmax++;
    }
  }
  
  mNMax = nmax;
}


void StFtpcTrack::Fit()
{
  // set up hits for calling fit class
  Int_t numHits = GetNumberOfPoints();
  StThreeVector<double> *Hit=new StThreeVector<double>[numHits];
  for(Int_t i=0; i<numHits; i++)
    {
      Hit[numHits -1 -i] = (((StFtpcConfMapPoint *)mPoints->At(i))->GetCoord());
    }

  // call fit class
  StFtpcMomentumFit *Fit = new StFtpcMomentumFit(Hit, numHits);

  mP = Fit->momentum();
  mChiSq[0] = Fit->chi2Rad();
  mChiSq[1] = Fit->chi2Lin();
  mQ = Fit->usedCharge();
  mTheta = Fit->momentum().theta();

  delete Fit;
  delete[] Hit;
}


Double_t StFtpcTrack::CalcDca(StFtpcVertex *vertex)
{
  // Calculates distance of closest approach to vertex.

  Int_t numHits = GetNumberOfPoints();
  StThreeVector<double> *Hit = new StThreeVector<double>[numHits];
  StThreeVector<double> vertexPos = vertex->GetCoord();
  
  for(Int_t i=0; i<numHits; i++) {
    Hit[numHits -1 -i] = (((StFtpcConfMapPoint *)mPoints->At(i))->GetCoord());
  }

  // call fit class
  StFtpcMomentumFit *looseFit = new StFtpcMomentumFit(Hit, numHits);

  StThreeVector<double> rv(vertexPos.x(), vertexPos.y(), vertexPos.z());
  StThreeVector<double> nv(0,0,1);
  Double_t pl = looseFit->pathLength(rv,nv);
  Double_t xvert = looseFit->x(pl) - vertexPos.x();
  Double_t yvert = looseFit->y(pl) - vertexPos.y();
  Double_t dca = TMath::Sqrt(xvert * xvert + yvert * yvert);

  delete looseFit;
  delete[] Hit;

  return dca;
}


void StFtpcTrack::Fit(StFtpcVertex *vertex, Double_t max_Dca, Int_t id_start_vertex)
{
  // Fitting.

  // set up hits for calling fit class
  Int_t numHits = GetNumberOfPoints();
  StThreeVector<double> *Hit = new StThreeVector<double>[numHits];
  StThreeVector<double> vertexPos = vertex->GetCoord();
  
  for(Int_t i=0; i<numHits; i++) {
    Hit[numHits -1 -i] = (((StFtpcConfMapPoint *)mPoints->At(i))->GetCoord());
  }

  // call fit class
  StFtpcMomentumFit *looseFit = new StFtpcMomentumFit(Hit, numHits);

  StThreeVector<double> rv(vertexPos.x(), vertexPos.y(), vertexPos.z());
  StThreeVector<double> nv(0,0,1);
  Double_t pl = looseFit->pathLength(rv,nv);
  Double_t xvert = looseFit->x(pl) - vertexPos.x();
  Double_t yvert = looseFit->y(pl) - vertexPos.y();
  mDca = TMath::Sqrt(xvert * xvert + yvert * yvert);

  StFtpcMomentumFit *Fit = new StFtpcMomentumFit(&vertexPos, Hit, numHits);

  if (id_start_vertex < 0 ) {
    mP = looseFit->momentum();
    StThreeVector<double> firstPoint(Hit[0].x(),Hit[0].y(),Hit[0].z());
    pl = looseFit->pathLength(firstPoint,nv);
    mV.setX(looseFit->x(pl));
    mV.setY(looseFit->y(pl));
    mV.setZ(looseFit->z(pl));
    mTrackLength = looseFit->pathLength(Hit[numHits-1],nv);
    mChiSq[0] = looseFit->chi2Rad();
    mChiSq[1] = looseFit->chi2Lin();
    mQ = looseFit->usedCharge();
    mRadius = 1./looseFit->curvature();
    mTheta = mP.theta();
    if (mDca > max_Dca) {
       mFromMainVertex = (Bool_t)false;
    }
    else {
       mFromMainVertex = (Bool_t)true;
    }
  }

  else {

     if (mDca > max_Dca) {
       mP = looseFit->momentum();
       StThreeVector<double> firstPoint(Hit[0].x(), Hit[0].y(), Hit[0].z());
       pl = looseFit->pathLength(firstPoint,nv);
       mV.setX(looseFit->x(pl));
       mV.setY(looseFit->y(pl));
       mV.setZ(looseFit->z(pl));
       mTrackLength = looseFit->pathLength(Hit[numHits-1], nv);
       mFromMainVertex = (Bool_t)false;
       mChiSq[0] = looseFit->chi2Rad();
       mChiSq[1] = looseFit->chi2Lin();
       mQ = looseFit->usedCharge();
       mRadius = 1./looseFit->curvature();
       mTheta = mP.theta();
     }

     else {
       mP = Fit->momentum();
       StThreeVector<double> firstPoint(vertexPos.x(), vertexPos.y(), vertexPos.z());
       pl = Fit->pathLength(firstPoint,nv);
       mV.setX(Fit->x(pl));
       mV.setY(Fit->y(pl));
       mV.setZ(Fit->z(pl));
       mTrackLength = Fit->pathLength(Hit[numHits-1], nv);
       mFromMainVertex = (Bool_t)true;
       mChiSq[0] = Fit->chi2Rad();
       mChiSq[1] = Fit->chi2Lin();
       mQ = Fit->usedCharge();
       mRadius = 1./Fit->curvature();
       mTheta = mP.theta();
     }
   }
  
  delete looseFit;
  delete Fit;
  delete[] Hit;
}


Int_t StFtpcTrack::Write(fpt_fptrack_st *trackTableEntry, Int_t id_start_vertex)
{
  // Writes track to StAF table
  
  for(Int_t k=0; k<10; k++) {
    trackTableEntry->hitid[k] = -1;
  }
  
  trackTableEntry->nrec = mPoints->GetEntriesFast();

  for(Int_t i=0; i<trackTableEntry->nrec; i++) {
    Int_t rowindex = (((StFtpcConfMapPoint *)mPoints->At(i))->GetPadRow());
    
    if(rowindex > 10) {
      rowindex -= 10;
    }  
    
    trackTableEntry->hitid[rowindex-1] = mPointNumbers->At(i)+1;
  }

  trackTableEntry->nfit = trackTableEntry->nrec;
  trackTableEntry->nmax = mNMax;

  if(mFromMainVertex) {
    trackTableEntry->flag = 1;
    trackTableEntry->id_start_vertex = id_start_vertex;
    if (id_start_vertex > 0) {
       ++trackTableEntry->nrec;
       ++trackTableEntry->nmax;
       ++trackTableEntry->nfit;
    }
  }

  else {
    trackTableEntry->flag = 0;
    trackTableEntry->id_start_vertex = 0;
  }
  
  trackTableEntry->q = mQ;
  trackTableEntry->chisq[0] = mChiSq[0];
  trackTableEntry->chisq[1] = mChiSq[1];
  trackTableEntry->p[0] = mP.x();
  trackTableEntry->p[1] = mP.y();
  trackTableEntry->p[2] = mP.z();
  trackTableEntry->v[0] = mV.x();
  trackTableEntry->v[1] = mV.y();
  trackTableEntry->v[2] = mV.z();
  trackTableEntry->length = mTrackLength;
  trackTableEntry->theta = mTheta;
  trackTableEntry->curvature = 1/mRadius;
  trackTableEntry->impact = mDca;

  return 0;
}
