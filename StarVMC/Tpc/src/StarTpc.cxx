/* $Id: StarTpc.cxx,v 1.2 2004/09/02 23:23:59 potekhin Exp $ */

#include <TVirtualMC.h>

#include "StarTpc.h"
#include <iostream.h>
#include "StarMaterial.h"
#include "StarMedium.h"
#include "StarVolume.h"
#include "StarRotation.h"

#include "TPCG.h"
#include "TECW.h"
#include "TPRS.h"


ClassImp(StarTpc)


//_______________________________________________________________________
StarTpc::StarTpc() {
  cout<<"Constructing Star TPC"<<endl;
}
//_______________________________________________________________________
//_______________________________________________________________________
StarTpc::StarTpc(const char* name_, const char *title_):
  StarDetector(name_,title_) {
    cout<<"Constructing Star TPC"<<endl;
}

//_______________________________________________________________________
StarTpc::~StarTpc()
{
}
//_______________________________________________________________________
void StarTpc::CreateGeometry(void) {

  // Some useful calculated parameters:
  Float_t
    tofcLeng,tpgvLeng,tpgvz,
    tocsIR, tocsOR, tokaIR, tokaOR,
    tonxIR, tonxOR, toadIR, toadOR,
    toigIR, toigOR, toalIR, toalOR,
    tohaIR, tohaOR, tifcIR, tifcOR,
    tiadIR, tiadOR, tinxIR, tinxOR,
    tikaIR, tikaOR, tialIR, tialOR,
    tofcIR, tofcOR, tofsIR, tofsOR,
    tpgvIR,
    tpeaZ;


  tofcLeng = TPCG::length-2.0*TPCG::WheelTHK;                // gas plus endcaps
  tpgvLeng = (tofcLeng-TPCG::MembTHK-2.0*TPCG::tpeaTHK)/2.0; // active gas
  tpgvz    = (TPCG::MembTHK + tpgvLeng)/2.0;

  tpeaZ    = (tofcLeng - TPCG::tpeaTHK)/2.0;                 //  TPC endcaps 

  // calculate radii of outer finest structures
  tocsIR = TPCG::SenGasOR;   tocsOR = tocsIR + TPCG::tocsDR;
  tokaIR = tocsOR;           tokaOR = tokaIR + TPCG::tokaDR;
  tonxIR = tokaOR;           tonxOR = tonxIR + TPCG::tonxDR;
  toadIR = tonxOR;           toadOR = toadIR + TPCG::toadDR;
  toigIR = toadOR;           toigOR = toigIR + TPCG::toigDR;
  toalIR = toigOR;           toalOR = toalIR + TPCG::toalDR;
  tohaIR = toalOR;           tohaOR = tohaIR + TPCG::tohaDR;

  // calculate radii of inner finest structures
  tifcIR = TPCG::rmin; //    inner field cage inner radius
  tiadIR = tifcIR;           tiadOR = tiadIR + TPCG::tiadDR;
  tinxIR = tiadOR;           tinxOR = tinxIR + TPCG::tinxDR;
  tikaIR = tinxOR;           tikaOR = tikaIR + TPCG::tikaDR;
  tialIR = tikaOR;           tialOR = tialIR + TPCG::tialDR;

  // derive radii of larger structures
  tofcIR = tocsIR;           tofcOR = tohaOR;
  tofsIR = tocsIR;           tofsOR = toadOR;
  tifcIR = tiadIR;           tifcOR = tialOR;
  tpgvIR = tifcOR;   //      TPC gas inner radius

  // the TPC envelope: ------------------------------
    Float_t tpce[3];
  tpce[0] = TPCG::rmin;       // TPC inner radius
  tpce[1] = TPCG::rmax;       // TPC outer radius
  tpce[2] = TPCG::length/2.0; // TPC half length
  StarVolume::Volume("TPCE","TUBE","Air",tpce,3);

  // the gas volume:   ------------------------------
  Float_t tpgv[3];
  tpgv[0] = tpgvIR;
  tpgv[1] = TPCG::SenGasOR;
  tpgv[2] = tpgvLeng/2.0;
  StarVolume::Volume("TPGV","TUBE","P10",tpgv,3);

  // the inner cage:   ------------------------------
  Float_t tifc[3];
  tifc[0] = tiadIR;
  tifc[1] = tifcOR;
  tifc[2] = tofcLeng/2.0;
  StarVolume::Volume("TIFC","TUBE","Mylar",tifc,3);

  // the outer cage:   ------------------------------
  Float_t tofc[3];
  tofc[0] = tofcIR;
  tofc[1] = tofcOR;
  tofc[2] = tofcLeng/2.0;
  StarVolume::Volume("TOFC","TUBE","Air",tofc,3); // should be nitrogen

  Float_t test[3] = {tofcIR,tofcOR,tofcLeng/2.0};


  // the membrane:     ------------------------------
  Float_t tpcm[3];
  tpcm[0] = tpgvIR;
  tpcm[1] = TPCG::SenGasOR;
  tpcm[2] = TPCG::MembTHK/2.0;
  StarVolume::Volume("TPCM","TUBE","Mylar",tpcm,3);

  // the endcap:       ------------------------------
  Float_t tpea[3];
  tpea[0] = tpgvIR;
  tpea[1] = TPCG::SenGasOR;
  tpea[2] = TPCG::tpeaTHK/2.0;
  StarVolume::Volume("TPEA","TUBE","P10",tpea,3);

  // a supersector containing Al sector, PCB and MWC volume
  TString tsecName("TSE");
  TString tsecNumb(tsecName);

  Float_t tsec[4];
  tsec[0]=TECW::inwidth[0]/2.0;
  tsec[1]=TECW::ouwidth[0]/2.0;
  tsec[2]=(TECW::ppdepth[0]+TECW::asdepth[0]+TECW::ggdepth[0])/2.0,
  tsec[3]=TECW::height[0]/2.0;
  //  tsecNumb+=1;
  //  StarVolume::Volume(tsecNumb,"TRD1","Al",tsec,4);

  // the padrow: ------------------------------
    Float_t tpad[3];
    //  tpce[0] = TPCG::rmin;       // TPC inner radius
    //tpce[1] = TPCG::rmax;       // TPC outer radius
    //tpce[2] = TPCG::length/2.0; // TPC half length
    //StarVolume::Volume("TPAD","BOX","P10",tpea,3);


  Double_t posX =  0.0;  Double_t posY =  0.0;  Double_t posZ =  0.0;
  gMC->Gspos("TPCE", 1 ,"WRLD", posX, posY, posZ, 0, "ONLY");


  StarRotation::Rotation("tpgv1",90.0, 75.0, 90.0, -15.0,   0.0, 0.0);
  StarRotation::Rotation("tpgv2",90.0,105.0, 90.0, 195.0, 180.0, 0.0);

  posX =  0.0;  posY =  0.0;  posZ =  tpgvz;
  gMC->Gspos("TPGV", 1 ,"TPCE", posX, posY,  posZ, StarRotation::FindRotation("tpgv1")->GetNumber(), "ONLY");
  gMC->Gspos("TPGV", 2 ,"TPCE", posX, posY, -posZ, StarRotation::FindRotation("tpgv2")->GetNumber(), "ONLY");

  posX =  0.0;  posY =  0.0;  posZ =  0.0;
  gMC->Gspos("TIFC", 1 ,"TPCE", posX, posY,  posZ, 0, "ONLY");
  gMC->Gspos("TOFC", 1 ,"TPCE", posX, posY,  posZ, 0, "ONLY");

  posX =  0.0;  posY =  0.0;  posZ =  tpeaZ;
  gMC->Gspos("TPEA", 1 ,"TPCE", posX, posY,  posZ, StarRotation::FindRotation("tpgv1")->GetNumber(), "ONLY");
  gMC->Gspos("TPEA", 2 ,"TPCE", posX, posY, -posZ, StarRotation::FindRotation("tpgv2")->GetNumber(), "ONLY");

  // TESS is a division of endcap volume corresponding to one supersector:  gMC->Gsdvn("TESS",    "TPEA", 12,2);

  StarVolume::Division("TESS","TPEA", 12,2);

  for(int i_sec=1;i_sec<=2;i_sec++) {
    for(int i_row=1;i_row<=TPRS::nRow[i_sec];i_row++) {
      
    }
  }
  //  StarVolume::Division("TPSS","TPGV", 12,2);




//   //------------------------------ 
//   // Tracker segments
//   //------------------------------
//   // 
//   // An example of Parameterised volumes
//   // dummy values for G4Box -- modified by parameterised volume
//   // - implemented using Gsposp

//   Double_t chamber[3];
//   chamber[0] = -1;
//   chamber[1] = -1;
//   chamber[2] = -1;
//   gMC->Gsvolu("CHMB","BOX", chamberMater, chamber, 0);

//   Double_t firstPosition = -trackerSize + 0.5*fChamberWidth;
//   Double_t firstLength = fTrackerLength/10;
//   Double_t lastLength  = fTrackerLength;
//   StarChamberParametrization* chamberParam 
//     = new StarChamberParametrization(  
// 			   fNofChambers,          // NoChambers 
// 			   firstPosition,         // Z of center of first 
// 			   fChamberSpacing,        // Z spacing of centers
// 			   fChamberWidth,          // Width Chamber 
// 			   firstLength,           // lengthInitial 
// 			   lastLength);           // lengthFinal
//   for (Int_t i=0; i<fNofChambers; i++) {
//     Double_t pos[3];
//     Double_t dim[3];
//     chamberParam->ComputeTransformation(i, pos);
//     chamberParam->ComputeDimensions(i, dim);
//     gMC->Gsposp("CHMB", i ,"TRAK", pos[0], pos[1], pos[2], 0, "ONLY", dim, 3); 
//   }  

}
