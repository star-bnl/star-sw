//*-- Author : Jan Balewski
//  
// $Id: histoLPfind.cxx,v 1.5 2001/04/27 20:50:45 balewski Exp $
// $Log: histoLPfind.cxx,v $
// Revision 1.5  2001/04/27 20:50:45  balewski
// *** empty log message ***
//
// Revision 1.4  2001/04/13 20:15:14  balewski
// *** empty log message ***
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
#include "StppLPfindMaker.h"

#include "TH2.h"

//_______________________________________________________________
//_______________________________________________________________
//_______________________________________________________________
void StppLPfindMaker::init_histo()
{
  float pt0=0., pt1=15.;
  int npt=(int)pt1;

  printf("init histo\n"); 
  hv[0]=new TH1F("R vZ","Recon Zvertex/cm ",100, -20., 20.);
  hv[1]=new TH1F("R dZ","Recon #DeltaZ vertex-LP /cm ",100, -5., 5.);
  hv[2]=new TH1F("R dRxy","Recon #Delta#sqrt{X^2+Y^2} vertex-LP /cm ",200, 0., .1);
  hv[3]=new TH1F("R nPri","No. of primary tracks",100,0.,200.);
  hv[4]=new TH1F("R pt","Recon LP pT  gPt (GeV/c)",npt,pt0,pt1);
  hv[5]=new TH1F("R nTPT","No. of all TPT tracks",100,0.,1000.);
  hv[6]=new TH1F("R eff","Counts Input,rec events",10,-0.5,9.5);
  hv[7] =(TH1F*)new TH2F("hvt","No. TCL hits/K vs. No. PrimTrack, AC",50,0.,50.,50,0.,10.);

  printf("init histo .. done\n");

}

