//*-- Author : Jan Balewski
//  
// $Id: histoLPproject.cxx,v 1.1.1.2 2001/04/21 00:43:14 fisyak Exp $
// $Log: histoLPproject.cxx,v $
// Revision 1.1.1.2  2001/04/21 00:43:14  fisyak
// *** empty log message ***
//
// Revision 1.4  2001/04/19 21:30:37  balewski
// add I/O to ppDst
//
// Revision 1.3  2001/04/12 15:19:09  balewski
// *** empty log message ***
//
// Revision 1.2  2001/02/28 19:06:12  balewski
// some reorganizations
//
// Revision 1.1.1.1  2001/01/31 14:00:07  balewski
// First release
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                    
//   histograms initialization for this class
//                                                                    
//////////////////////////////////////////////////////////////////////////

#include <math.h>
#include <stdio.h>
#include "StppLPprojectMaker.h"

#include "TH2.h"

//_______________________________________________________________
//_______________________________________________________________
//_______________________________________________________________
void StppLPprojectMaker::init_histo()
{
  float pt0=0., pt1=15.;
  int npt=(int)pt1;
  int nphi=12;

  printf("init histo ::%s\n", GetName());

  hst[0]=new TH1F("rpt0","all recon events vs. LP rPt (GeV/c)",npt,pt0,pt1);
  hst[1]=new TH1F("rpt1","after LP tuned cuts=AC vs. LP rPt (GeV/c)",npt,pt0,pt1);
  hst[2]=new TH1F("sID","valid spinID, AC",5,-0.5,4.5);

  hst[3] =(TH1F*)new TH2F("hvt","No. TCL hits/K vs. No. PrimTrack, AC",50,0.,50.,50,0.,10.);

  hst[4]=new TH1F("ph1","rLP vs. #phi(deg), all AC",nphi*10,0.,360.);

  // spin dependent histos
  hpol[0]=new TH1F("p0","rLP vs. #phi(deg) for VOID Pol",nphi,0.,360.);
  hpol[1]  =new TH1F("p1","rLP vs. #phi(deg) for +Pol",nphi,0.,360.);
  hpol[2]=new TH1F("p2","rLP vs. #phi(deg) for -Pol",nphi,0.,360.);
  hpol[3]  =new TH1F("p3","rLP vs. #phi(deg) for 0-Pol",nphi,0.,360.);


#if 0 //tmp
  hm[1]=new TH1F("m1","nTclPts rPT12 bad",30,0.,60.);
  hm[2]=new TH1F("m2","nTclPts rPT12  good",30,0.,60.);

  hm[3]=new TH1F("m3","chi2/free rPT12  bad",25,0.,.5);
  hm[4]=new TH1F("m4","chi2/free rPT12 good",25,0.,.5);

  hm[5]=new TH1F("m5","nVertTR rPT12  bad",31,-0.5,30.5);
  hm[6]=new TH1F("m6","nVertTR  rPT12 god",31,-0.5,30.5);

  hm[7]=new TH1F("m7","zV-zrLP (cm)  bad",30,-1.5,1.5);
  hm[8]=new TH1F("m8","zV-zrLP (cm)  good",30,-1.5,1.5);

  hm[9]=new TH1F("m9","delRxy V-rLP (cm)  bad",50,0.,.1);
  hm[10]=new TH1F("m10","delRxy V-rLP (cm)  good",50,.0,.1);

  hm[11]=new TH1F("m11","LP Rxy  (cm)  bad",30,0.,1.5);
  hm[12]=new TH1F("m12","LP Rxy  (cm)  good",30,0.,1.5);

  hm[13]=new TH1F("m13","rPT-gPT (GeV/c)  bad",50,-2.5,2.5);
  hm[14]=new TH1F("m14","rPT-gPT (GeV/c)  good",50,-2.5,2.5);

  hm[15]=new TH1F("m15","|#Delta#phi| rec-gen  bad",30,0.,200.);
  hm[16]=new TH1F("m16","|#Delta#phi| rec-gen  good",30,0.,200.);

  hst[0]=new TH1F("gpta","all input events vs. gPt (GeV/c)",npt,pt0,pt1);
  hst[1]=new TH1F("gptb","all accepted events vs. gPt (GeV/c)",npt,pt0,pt1);


  hst[3]=new TH1F("stA","Stat pT=[1,2]",25,-0.5,24.5);
  hst[4]=new TH1F("stB","Stat pT=[3,4]",25,-0.5,24.5);
  hst[5]=new TH1F("stC","Stat pT=[5,6]",25,-0.5,24.5);




  hpol[4+voidPol]=new TH1F("vpol2","rLP vs. #phi(deg) for void Pol",4*nphi,0.,360.);
  hpol[4+upPol]  =new TH1F("+pol2","rLP vs. #phi(deg) for up Pol",4*nphi,0.,360.);
  hpol[4+downPol]=new TH1F("-pol2","rLP vs. #phi(deg) for down Pol",4*nphi,0.,360.);
  hpol[4+noPol]  =new TH1F("0pol2","rLP vs. #phi(deg) for no Pol",4*nphi,0.,360.);


  hpol[8+voidPol]=new TH1F("vpol3","rLP vs. #phi(deg) for void Pol",16*nphi,0.,360.);
  hpol[8+upPol]  =new TH1F("+pol3","rLP vs. #phi(deg) for up Pol",16*nphi,0.,360.);
  hpol[8+downPol]=new TH1F("-pol3","rLP vs. #phi(deg) for down Pol",16*nphi,0.,360.);
  hpol[8+noPol]  =new TH1F("0pol3","rLP vs. #phi(deg) for no Pol",16*nphi,0.,360.);
#endif

}

