// *-- Author : J.Balewski
// 
// $Id: StFgtSlowSimu_histo.cxx,v 1.1 2012/06/06 20:35:09 jeromel Exp $

#include <TVector3.h>
#include <TH2.h>
#include <TF1.h>
#include <TFile.h>
#include <TLine.h>
#include <TPolyLine.h>
#include <TCrown.h>
#include <TRandom3.h>

#include "StFgtUtil/geometry/StFgtGeom.h"

#include "StFgtSlowSimuMaker.h"

//_______________________________________________
//________________________________________________
void
StFgtSlowSimuMaker::InitHisto1( ){
  /* working arrays for slow simulator:
     quadDigitizationXY,quadDigitizationRad, quadDigitizationPhi is reset for every quart (24 times per event)
  */

  Int_t digNx=2000, digNy=digNx;
  Float_t maxR=40.;
  quadDigitizationXY=new TH2F("digXY"," 2D digitization, z=charge (a.u); quadrant local X(cm); quadrant local Y (cm)",digNx,0.,maxR,digNy,0.,maxR);  

  quadDigitizationRad=new TH1F("digRad","rad-strips ADC; radial-strip ID",kFgtNumStrips,0.,kFgtNumStrips);
  quadDigitizationPhi=new TH1F("digPhi","phi-strips ADC; phi-strip ID",kFgtNumStrips,0.,kFgtNumStrips);


#ifdef __FGT_QA_HISTO__  
  HList->Add(quadDigitizationXY);
  HList->Add(quadDigitizationRad);
  HList->Add(quadDigitizationPhi);

  // The next 4 histograms are not reset for debugging purpose WMZ 
  digXYAll=new TH2F("digXYAll"," 2D charge , sens vol; local X(cm); local Y (cm)",digNx,0.,maxR,digNy,0.,maxR);  HList->Add(digXYAll);

  digPadcAll=new TH2F("digPadcAll"," 2D ADC/bin, P-plane; local X(cm); local Y (cm)",digNx,0.,maxR,digNy,0.,maxR);  HList->Add(digPadcAll);
  digRadcAll=new TH2F("digRadcAll"," 2D ADC/bin, R-plane; local X(cm); local Y (cm)",digNx,0.,maxR,digNy,0.,maxR);  HList->Add(digRadcAll);

  digRAll=new TH1F("digRAll","rad-strips ADC, all events; radial-strip ID",kFgtNumStrips,0.,kFgtNumStrips); HList->Add(digRAll);
  digPAll=new TH1F("digPAll","phi-strips ADC, all events; phi-strip ID",kFgtNumStrips,0.,kFgtNumStrips); HList->Add(digPAll);
#endif  
 
} 

#ifdef __FGT_QA_HISTO__
//________________________________________________
//________________________________________________
void
StFgtSlowSimuMaker::InitHisto2(){
  
  // QA-histos
  hA[0]=new TH1F("ss_inDE","g2t  DE of hit; Elos (keV)",100,0.,5.);
  hA[1]=new TH1F("ss_inDS","g2t  path length of hit;  DS (cm)",60,0.,9.);
  hA[2]=new TH1F("ss_inZ","g2t  Z of hit (entrance);  Z(cm)",100,50.,250.);
  

  hA[3]=new TH1F("ss_hitStat","Where G-hits are lost, 1-10 general; x=10+5*disk+quad",45, 0.5,45.5);
  hA[4]=new TH2F("ss_inXY","Entrance X-Y , accepted G-hits, all disks",450,-45.,45.,450,-45.,45.);
  
  Float_t maxRb=45.;
  hA[5]=new TH1F("ss_cTof","g2t  TOF of hit (before cut);  TOF(ns)",1000,0.,200.);
  hA[6]=new TH1F("ss_inR","g2t  Rxy of hit, w=1.;  Rxy (cm)",10*(int)maxRb,0.,maxRb);
  hA[7]=new TH1F("ss_cPmag","g2t  P.Mag of hit (before cut);  log10(momentum/MeV)",160,-2,6.);



 // 8,9,10-free

  Int_t i;
  for(i=0;i<kFgtNumDiscs;i++) { // uses histos ID=[11,18]
    char tt1[100], tt2[500];
    sprintf(tt1,"ss_gXY%d",i+1);
    sprintf(tt2," G-hits accepted in Disk=%d; LAB X (cm) ; LAB Y (cm) ",i+1);
    hA[11+i]=new TH2F(tt1,tt2,25,-maxRb,maxRb,25,-maxRb,maxRb);
  }
 hA[20]=new TH1F("fr_pairEne", "Energy of pair per collision; Energy Loss per collision [eV]", 100, 0, 100);
 hA[21]=new TH1F("fr_nPrimPair", Form("No. of prim pairs per track, use %.1f ions/mm; # of pairs",par_pairsPerCm/10.), 35, -0.5, 34.5);
   hA[22]= new TH1F("fr_totEne", "Total energy deposit per track,F.model;  Energy Loss [keV]", 250, 0, 5.);

   hA[23]= new TH1F("fr_nTotPair", "Total No. of  pairs per track; # of pairs", 250, -0.5, 249.5);
   hA[24]= new TH1F("fr_pathL", "Total path length in gas per track; path (mm)",50, 0., 10.);
   hA[25]= new TH1F("fr_avrPath", "Average path length , w=nAnyEle; path (mm)",50, 0., 10.);
   hA[26]= new TH1F("fr_avrTPath", "Average transverse path length , w=nAnyEle; path (mm)",250, 0., 10.);
   hA[27]=new TH1F("fr_Zdrf","Zdrift of prim ele ;  Z(mm)",100,0.,5.);
   hA[28]=new TH2F("fr_Rdiff","Transverse diffusion; relative X(um); relative Y(um)",50,-250,250,50,-250,250);


   hA[29]=new TH1F("dg_Radc","R-plane ADC sum per quad per eve; ADC sum ",200,0,3000);
   hA[30]=new TH1F("dg_Padc","P-plane ADC sum per quad per eve; ADC sum ",200,0,3000);
   hA[31]=new TH2F("dg_PRadc","ADC sum per quad per eve; P-plane sum; R-plane sum", 100,0,2000,100,0,2000);


   //..............add gadgets to histos
   TList *Lx;  TCrown *cr; //TLine *ln; 

   //....inXY....
   Lx=hA[4]->GetListOfFunctions();    //jjassert(Lx);
   Int_t iq=0;
 
   for(iq=0;iq<kFgtNumQuads;iq++) {
     Float_t phi1=StFgtGeom::phiQuadXaxis(iq)/3.1416*180.;
     cr=new TCrown(0.,0.,StFgtGeom::rIn(),StFgtGeom::rOut(),phi1,phi1+90);
     cr->SetLineColor(kRed); Lx->Add(cr);
   }
   cr=new TCrown(0.,0.,StFgtGeom::rMid(),StFgtGeom::rMid(),0,360);
   Lx->Add(cr);  

  for(i=0;i<mxH;i++) 
    if(hA[i]) HList->Add(hA[i]);
  

}
#endif


//_______________________________________________
//________________________________________________
void
StFgtSlowSimuMaker::CloseHisto(){
  
    //..............add gadgets to histos
   TList *Lx;  TCrown *cr; //TLine *ln; 

   for(Int_t ih=0;ih<3;ih++) {

     if( ih==0)  Lx=quadDigitizationXY->GetListOfFunctions();   
     if( ih==1)  Lx=digXYAll->GetListOfFunctions();   
     if( ih==2)  Lx=digPadcAll->GetListOfFunctions();   
     //jjassert(Lx);

     Int_t iqq=0;
     for(iqq=0;iqq<2;iqq++) {
       Float_t phi1=0;
       if(iqq<1) 
	 cr=new TCrown(0.,0.,StFgtGeom::rMid(),StFgtGeom::rOut(),phi1,phi1+90);
       else
	 cr=new TCrown(0.,0.,StFgtGeom::rIn(),StFgtGeom::rMid(),phi1,phi1+90);
       cr->SetLineStyle(2);
       Lx->Add(cr);
     }
     cr=new TCrown(0.,0.,StFgtGeom::rMid(),StFgtGeom::rMid(),0,90);
     Lx->Add(cr);

     TLine *ln=new TLine(0,0,30,30);
     ln->SetLineStyle(2);       Lx->Add(ln);
          
   }
}



/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

// $Log: StFgtSlowSimu_histo.cxx,v $
// Revision 1.1  2012/06/06 20:35:09  jeromel
// Code  review closed (requested Anselm/Jan; reviewed Jonathan/Jason)
//
// Revision 1.16  2012/05/08 16:40:26  avossen
// prepare for review
//
// Revision 1.15  2012/03/17 01:08:31  balewski
// works with Anselm's cluster finder
//
// Revision 1.14  2012/02/09 16:48:06  rfatemi
// Updated Rin, Rmin, Rout etc to conform with new names in StFgtGeom class
//
// Revision 1.13  2012/01/27 14:09:34  balewski
// switch to new consts
//
// Revision 1.12  2012/01/26 18:41:43  balewski
// fixing , new constants
//
// Revision 1.11  2012/01/04 16:33:10  balewski
// adjusted lables
//
// Revision 1.10  2011/12/01 00:58:01  avossen
// changed the use of the naive maker to use of StFgtDb, replaced geom-> with StFgtGeom::
//
// Revision 1.9  2011/11/08 21:43:25  balewski
// testing P-strips
//
// Revision 1.8  2011/11/02 20:53:07  balewski
// slow simu works for 6 discs, onlt R-plane
//
// Revision 1.7  2011/10/12 21:15:02  balewski
// half way
//
// Revision 1.6  2011/10/12 18:20:38  balewski
// after testing of R-strip ID mapping
//
// Revision 1.5  2011/10/07 19:45:33  balewski
// testing strip ID
//
// Revision 1.3  2011/10/05 18:04:33  balewski
// storing of FGT in StEvent is almost working
//
// Revision 1.2  2011/09/29 21:36:17  balewski
// now 2D distribution of charge & fiducial cuts are workimng properly
//
// Revision 1.1  2011/09/28 20:57:37  balewski
// merging private code
//
// Revision 1.1  2011/04/07 19:31:22  balewski
// start
//


 


