/// \author Piotr A. Zolnierczuk, Indiana University Cyclotron Facility
/// \date   2004/01/19
// $Id: EEmcTTDisplay.cxx,v 1.1 2004/01/19 22:07:49 zolnie Exp $
// doxygen info here

#include "TGeoVolume.h"
#include "TGeoCone.h"
#include "TGeoManager.h"
#include "TGeoMedium.h"

#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "EETowDisplay.h"

EETowDisplay::EETowDisplay(const char *name) : 
  EEmcGeomSimple()
{ 
  char vname[256];
  double eta1  = mEtaBin[0];
  double eta2  = mEtaBin[mNumEta];

  double rmin1 = mZ1/TMath::CosH(eta1);
  double rmax1 = mZ1/TMath::CosH(eta2);
  double rmin2 = mZ2/TMath::CosH(eta1);
  double rmax2 = mZ2/TMath::CosH(eta2);

  double dz    = TMath::Abs(mZ2-mZ1)/2.0;

  //TGeoMaterial *matVac = new TGeoMaterial("Vacuum",0,0,0);
  TGeoMedium   *medVac = new TGeoMedium  ("Vacuum",1, new TGeoMaterial("Vacuum",0,0,0) );
  TGeoCone     *econe  = new TGeoCone(dz,rmin1,rmax1,rmin2,rmax2);
  mEEmc                = new TGeoVolume(name,econe,medVac);

  TGeoConeSeg  *geosector = new TGeoConeSeg(dz,rmin1,rmax1,rmin2,rmax2,0.0,360.0/mNumSec);
  TGeoConeSeg  *geosubsec = new TGeoConeSeg(dz,rmin1,rmax1,rmin2,rmax2,0.0,360.0/mNumSec/mNumSSec);
  for(unsigned s=0;s<mNumSec;s++) {
    sprintf(vname,"SEC_%02d",s+1);  
    TGeoRotation *rots = new TGeoRotation();  
    rots ->SetAngles(getPhiMean(s)/M_PI*180.0,0.0,0.0);
    TGeoVolume *sector = new TGeoVolume(vname,geosector,medVac);
    mEEmc->AddNode(sector,1,rots);

    for(unsigned ss=0;ss<mNumSSec;ss++) {
      sprintf(vname,"SUB_%1c",'A'+ss);
      TGeoRotation *rotss  = new TGeoRotation();  
      rotss ->SetAngles(getPhiMean(0,ss)/M_PI*180.0,0.0,0.0);
      TGeoVolume *subsector = new TGeoVolume(vname,geosubsec,medVac); 
      sector->AddNode(subsector,1,rotss);
      
      for(unsigned e=0;e<mNumEta;e++) {   
	sprintf(vname,"%02dT%1c%02d",s+1,'A'+ss,e+1);
	eta1  = mEtaBin[e];
	eta2  = mEtaBin[e+1];
	rmin1 = mZ1/TMath::CosH(eta1);
	rmax1 = mZ1/TMath::CosH(eta2);
	rmin2 = mZ2/TMath::CosH(eta1);
	rmax2 = mZ2/TMath::CosH(eta2);
	TGeoConeSeg  *geotile = new TGeoConeSeg(dz,rmin1,rmax1,rmin2,rmax2,0.0,360.0/mNumSec/mNumSSec);
	TGeoVolume   *tile    = new TGeoVolume(vname,geotile,medVac);
	subsector->AddNode(tile,1);
      } 
    }
  }

#if 0
  for(unsigned e=0;e<mNumEta;e++) {   
    char vname[256];
    int   kss = mNumSSec*mNumSec;
    eta1  = mEtaBin[e];
    eta2  = mEtaBin[e+1];
    rmin1 = mZ1/TMath::CosH(eta1);
    rmax1 = mZ1/TMath::CosH(eta2);
    rmin2 = mZ2/TMath::CosH(eta1);
    rmax2 = mZ2/TMath::CosH(eta2);
    TGeoCone   *etacone   = new TGeoCone  (dz,rmin1,rmax1,rmin2,rmax2);
    sprintf(vname,"ETA%02d",e+1);
    TGeoVolume *eta       = new TGeoVolume(vname,etacone,medVac);
    mEEmc->AddNode(eta,1);

    TGeoConeSeg  *geotile = new TGeoConeSeg(dz,rmin1,rmax1,rmin2,rmax2,0.0,360.0/kss);
    for(unsigned s=0;s<mNumSec;s++) {
      for(unsigned ss=0;ss<mNumSSec;ss++) {
	sprintf(vname,"%02dT%1c%02d",s+1,'A'+ss,e+1);
	TGeoVolume   *tile = new TGeoVolume(vname,geotile,medVac); 
	TGeoRotation *rota = new TGeoRotation();  
	rota ->SetAngles(getPhiMean(s,ss)/M_PI*180.0,0.0,0.0);
	eta->AddNode(tile,1,rota);
      }
    }
  }
#endif

#if 0  

    eta1  = mEtaBin[e];
    eta2  = mEtaBin[e+1];
    rmin1 = mZ1/TMath::CosH(eta1);
    rmax1 = mZ1/TMath::CosH(eta2);
    rmin2 = mZ2/TMath::CosH(eta1);
    rmax2 = mZ2/TMath::CosH(eta2);
    char vnamE [256];
    char vnamS [256];
    sprintf(vnamE ,"ETA%02d",e+1);
    sprintf(vnamS ,"SEC%02d",e+1);
    Int_t s = mNumSec*mNumSSec;
    eta->Divide(vnamS,2,s,0.0,360.0/s);
    eta->SetLineColor(e+1);
    mEEmc->AddNode(eta,e+1);
  }
#endif
};



void
EETowDisplay::towerHit(int eta) 
{
   char vname [256];
   sprintf(vname ,"ETA%02d",eta);
   TIter next(mEEmc->GetNodes());
   TGeoNode *node;
   while( (node=(TGeoNode *)next())!=NULL ) 
     if(strncmp(vname,node->GetVolume()->GetName(),5)==0) break;
   if(node!=NULL) {
     node->GetVolume()->SetLineColor(kRed);
     printf("node %s found\n",vname);
   }
}


// $Log: EEmcTTDisplay.cxx,v $
// Revision 1.1  2004/01/19 22:07:49  zolnie
// toward track/tower display
//
//







