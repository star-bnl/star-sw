/// \author Piotr A. Zolnierczuk, Indiana University Cyclotron Facility
/// \date   2004/01/19
// $Id: EEmcTTDisplay.cxx,v 1.13 2009/01/26 14:37:43 fisyak Exp $
// doxygen info here
/**
 * \class   EEmcTTDisplay
 * \brief   EEmc Tower and Track Display class
 *
 * This is a simple root TGeoXXXX based class. It displays EEMC tower geometry
 * (the class inherits from EEmcGeomSimple) and allows for "turning on/off" individual
 * towers as well as it displays muDST tracks (StMuTrack).
 *
 * \author  Piotr A. Zolnierczuk
 * 
 * $Date: 2009/01/26 14:37:43 $ 
 * $Revision: 1.13 $
 * 
 * \section displayexample  Short example how to use EEmcTTDisplay
 * for details consult  StRoot/StEEmcPool/macros/TTM/show.C
 * \include show.C
 *  
 * \image html snapshot.jpg "Sample snapshot"
 *
 * \section displayremarks Remarks
 * \todo [medium] private member volumeName is not best implemented
 * \todo [minor]  speed of light redefined (should be taken from StarClassLibrary)
 * \bug  the class requires an explitic set of magnetic field via SetMagneticField
 *
 */

#include <Stiostream.h>
#include <Stsstream.h>

#include "TList.h"
#include "TMath.h"
#include "TGeoVolume.h"
#include "TGeoCone.h"
#include "TGeoManager.h"
#include "TGeoMedium.h"
#include "TGeoMatrix.h"
#include "THelix.h"

#include "StarClassLibrary/StPhysicalHelixD.hh"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"

#include "EEmcTower.h"
#include "EEmcTTMatch.h"
#include "EEmcTTMMaker.h"
#include "EEmcTTDisplay.h"


static const double cLight = 3e10*centimeter/second;

// FIXME sector color assignement
static const int  kSectorColors[] = {
  kBlack,  // SEC01
  kBlack,  // SEC02
  kGreen,  // SEC03
  kBlack,  // SEC04
  kBlack,  // SEC05
  kBlue ,  // SEC06
  kBlack,  // SEC07
  kBlack,  // SEC08
  kYellow, // SEC09
  kBlack,  // SEC10
  kBlack,  // SEC11
  kRed  ,  // SEC12
  kWhite,  
  kWhite,  
};


EEmcTTDisplay::EEmcTTDisplay(const char *name) : EEmcGeomSimple()
{ 
  mEEmc=NULL;

  mTrackHits=new TList;
  mTowerHits=new TList;
  mBField   =0.5;  // in Tesla
  mShowExtrapolatedTracks = false;
  initGeometry(name);

};

EEmcTTDisplay::~EEmcTTDisplay() 
{ 
  if(mEEmc)      delete mEEmc;
  if(mTrackHits) delete mTrackHits;
  if(mTowerHits) delete mTowerHits;
}


void
EEmcTTDisplay::initGeometry(const char *topName)
{
  const double kDPhi = -15.0/180.0*M_PI;   // FIXME hard coded offset
  char rotName[256];
  if(mEEmc!=NULL) return; // already initialized;

  double eta1  = mEtaBin[0];
  double eta2  = mEtaBin[mNumEta];

  double rmin1 = mZ1/TMath::SinH(eta1);
  double rmax1 = mZ1/TMath::SinH(eta2);

  double rmin2 = mZ2/TMath::SinH(eta1);
  double rmax2 = mZ2/TMath::SinH(eta2);

  double dz    = TMath::Abs(mZ2-mZ1)/2.0;

  int numSSec  = mNumSec*mNumSSec;
  
  TGeoMedium   *medVac = new TGeoMedium  ("Vacuum",1, new TGeoMaterial("Vacuum") );
  TGeoCone     *econe  = new TGeoCone(dz,rmin1,rmax1,rmin2,rmax2);
  mEEmc                = new TGeoVolume(topName,econe,medVac);
  mEEmc->SetLineColor(kWhite);
  TGeoConeSeg  *geosector = new TGeoConeSeg(dz,rmin1,rmax1,rmin2,rmax2,0.0,360.0/mNumSec);
  TGeoConeSeg  *geosubsec = new TGeoConeSeg(dz,rmin1,rmax1,rmin2,rmax2,0.0,360.0/numSSec);
  for(unsigned s=0;s<mNumSec;s++) {
    double phi = getPhiMean(s)+kDPhi;
    sprintf(rotName,"RotSec%02d",s+1);
    TGeoRotation *rots = new TGeoRotation(rotName,phi/M_PI*180.0,0.0,0.0);
    TGeoVolume *sector = new TGeoVolume(volumeName(s),geosector,medVac);
    sector->SetVisibility(kTRUE);
    sector->SetLineColor (kSectorColors[s]); 
    mEEmc->AddNode(sector,1,rots);
    //
    for(unsigned ss=0;ss<mNumSSec;ss++) {
      double phi = getPhiMean(0,ss)-getPhiMean(0,mNumSSec-1); // FIXME: assumed counter-clockwise 
      sprintf(rotName,"Rot%02dT%1c",s+1,ss+'A');
      TGeoRotation *rotss   = new TGeoRotation(rotName,phi/M_PI*180.0,0.0,0.0);
      TGeoVolume *subsector = new TGeoVolume(volumeName(s,ss),geosubsec,medVac); 
      subsector->SetVisibility(kFALSE);
      sector->AddNode(subsector,1,rotss);
      //
      for(unsigned e=0;e<mNumEta;e++) {   
	eta1  = mEtaBin[e];
	eta2  = mEtaBin[e+1];
	rmin1 = mZ1/TMath::SinH(eta1);	
	rmax1 = mZ1/TMath::SinH(eta2);
	rmin2 = mZ2/TMath::SinH(eta1);	
	rmax2 = mZ2/TMath::SinH(eta2);

	TGeoConeSeg  *geotile = new TGeoConeSeg(dz,rmin1,rmax1,rmin2,rmax2,0.0,360.0/numSSec);
	TGeoVolume   *tile    = new TGeoVolume(volumeName(s,ss,e),geotile,medVac);
	tile->SetVisibility(kFALSE);
	subsector->AddNode(tile,1);
      }
    }
  }
}


void
EEmcTTDisplay::Draw(const Option_t* option)
{
  TIter nextTower(mTowerHits);
  TIter nextTrack(mTrackHits);
  TGeoNode *gnode;
  TGeoVolume *vol;
  THelix   *helix;

  while( (gnode=(TGeoNode *)nextTower())!=NULL ) {
    vol=gnode->GetVolume();
    vol->SetLineColor(kRed);
    vol->SetVisibility(kTRUE);
  }
  while( (helix=(THelix   *)nextTrack())!=NULL ) {
    helix->Draw();
  }
}


void
EEmcTTDisplay::Clear(const Option_t* option)
{
  TIter nextTower(mTowerHits);
  TGeoNode *gnode;
  while( (gnode=(TGeoNode *)nextTower())!=NULL ) {
    //printf("clearing node %s\n",gnode->GetVolume()->GetName());
    gnode->GetVolume()->SetLineColor(kBlack);
    gnode->GetVolume()->SetVisibility(kFALSE);
  }

  mTrackHits->Delete();
  mTowerHits->Clear();
}




Bool_t
EEmcTTDisplay::AddTower(const char *vname)
{
  const int kSecLen=2;
  const int kSubLen=4;
  const int kEtaLen=6;
  TGeoNode *node;
  TIter sector(mEEmc->GetNodes());
  while( (node=(TGeoNode *)sector())!=NULL ) {
    if(strncmp(vname,node->GetVolume()->GetName(),kSecLen)!=0) continue;
    TIter subsector(node->GetVolume()->GetNodes());
    while( (node=(TGeoNode *)subsector())!=NULL ) {
      if(strncmp(vname,node->GetVolume()->GetName(),kSubLen)!=0) continue;
      TIter tile(node->GetVolume()->GetNodes());
      while( (node=(TGeoNode *)tile())!=NULL ) {
	if(strncmp(vname,node->GetVolume()->GetName(),kEtaLen)!=0) continue;
	//printf("towerHit: node %s found\n",vname);
	mTowerHits->Add(node);
	return kTRUE;
      }
    }
  }
  return kFALSE;
}

Bool_t       
EEmcTTDisplay::AddTower(const EEmcTower& tower)
{
  return AddTower(volumeName(tower.Sec(),tower.SubSec(),tower.Eta()));
}


Bool_t       
EEmcTTDisplay::AddTrack(Double_t x,  Double_t y,    Double_t z, 
			Double_t px, Double_t py,   Double_t pz, 
			Double_t qB, Double_t zMin, Double_t zMax)
{
  THelix *helix    = new THelix(x,y,z,px,py,pz,cLight*qB);

  if(zMin<=0.0) zMin=z;
  if(zMax<=0.0) zMax=mZ2;

  helix->SetRange(zMin,zMax);
  helix->SetLineColor(102); // the beauty of root :)
  helix->SetLineWidth(2);

  if(mShowExtrapolatedTracks) {
    THelix *helixExt = new THelix(x,y,z,px,py,pz,cLight*qB);
    helixExt->SetRange(z   ,mZ2);
    helixExt->SetLineColor(20);
    helixExt->SetLineWidth(1);
    mTrackHits->Add(helixExt);
  }
  mTrackHits->Add(helix);
  return kTRUE;
}

Bool_t       
EEmcTTDisplay::AddTrack(const StMuTrack& track)
{
  StPhysicalHelixD h = track.helix();
  StThreeVectorD o   = h.origin(); 
  StThreeVectorD p   = h.momentum(mBField*tesla);
  double         q   = h.charge(mBField*tesla);
  double         zMin= track.firstPoint().z();
  double         zMax= track.lastPoint().z();
  return AddTrack(o.x(),o.y(),o.z(),
		  p.x(),p.y(),p.z(),
		  q*mBField*tesla,zMin,zMax);
}


Bool_t       
EEmcTTDisplay::AddMatch(EEmcTTMatch& tmatch)
{
  EEmcTower *tower =tmatch.Tower(); 
  StMuTrack *track =NULL;
  if(!AddTower(*tower)) return kFALSE;
  TIter nextTrack(tmatch.Tracks());
  while((track=(StMuTrack *)nextTrack())) AddTrack(*track);
  return kTRUE;
}


void         
EEmcTTDisplay::Out(ostream &out, EEmcTTMatch &tmatch)
{
  EEmcTower *tower =tmatch.Tower(); 
  StMuTrack *track =NULL;
  out << *tower;
  TIter nextTrack(tmatch.Tracks());
  while((track=(StMuTrack *)nextTrack())) 
    out << *track;
}

void         
EEmcTTDisplay::Out(TString &out, EEmcTTMatch &tmatch)
{
  const  int  bufLen=1024; // FIXME buffer length
  static char buff[bufLen];


  EEmcTower *tower =tmatch.Tower(); 
  StMuTrack *track =NULL;
  TIter nextTrack(tmatch.Tracks());
  while((track=(StMuTrack *)nextTrack())) {
    sprintf(buff,"ADC(%s)=%4.1f pT=%.2f GeV/c\n",volumeName(*tower),tower->ADC(),track->pt());
    out.Append(buff);
  }
}





// a lousy ..... lousy
// sec [0,mNumSec)   sub [0,mNumSSec)  eta [0,mNumEta)
char *
EEmcTTDisplay::volumeName (int sec, int sub, int eta)
{
  const  int  vnameLen=1024;
  static char vname[vnameLen];
  int    kCase=0x00;
  
  memset(vname,0x00,vnameLen);
  if( 0<=sec && sec<int(mNumSec ) ) kCase |=0x4; else kCase &=0x3; 
  if( 0<=sub && sub<int(mNumSSec) ) kCase |=0x2; else kCase &=0x5;
  if( 0<=eta && eta<int(mNumEta ) ) kCase |=0x1; else kCase &=0x6;

  switch(kCase) {
  case 0x07: sprintf(vname,"%02dT%1c%02d",sec+1,sub+'A',eta+1);  break;
  case 0x06: sprintf(vname,"%02dT%1c"    ,sec+1,sub+'A');        break;
  case 0x04: sprintf(vname,"%02d"        ,sec+1)              ;  break;
  default:   break;
  }
  return vname;
}

char *
EEmcTTDisplay::volumeName(const EEmcTower& tower)
{  
  return volumeName(tower.Sec(),tower.SubSec(),tower.Eta()); 
}


// $Log: EEmcTTDisplay.cxx,v $
// Revision 1.13  2009/01/26 14:37:43  fisyak
// Add missing (in ROOT 5.22) includes
//
// Revision 1.12  2007/07/12 19:27:22  fisyak
// Add includes for TMath for ROOT 5.16
//
// Revision 1.11  2005/02/05 00:53:01  perev
// Bacward compatibility to redhead7
//
// Revision 1.10  2004/05/06 16:02:48  zolnie
// more docs
//
// Revision 1.9  2004/05/05 21:37:36  zolnie
// ver 2.0 released
//
// Revision 1.8  2004/05/04 18:28:55  zolnie
// version after split
//
// Revision 1.7  2004/04/13 14:53:38  zolnie
// *** empty log message ***
//
// Revision 1.6  2004/01/27 20:38:41  zolnie
// more docs
//
// Revision 1.5  2004/01/27 16:26:14  zolnie
// polished doxygen documentation
//
// Revision 1.4  2004/01/26 22:54:14  zolnie
// after name cleanup
//
// Revision 1.3  2004/01/26 21:51:53  zolnie
// shorter names
//
// Revision 1.2  2004/01/26 21:08:31  zolnie
// working track/tower display (before big farewell cleanup)
//
// Revision 1.1  2004/01/19 22:07:49  zolnie
// toward track/tower display
//
//







