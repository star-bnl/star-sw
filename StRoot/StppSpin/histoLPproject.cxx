//*-- Author : Jan Balewski
//  
// $Id: histoLPproject.cxx,v 1.5 2001/04/27 20:50:45 balewski Exp $
// $Log: histoLPproject.cxx,v $
// Revision 1.5  2001/04/27 20:50:45  balewski
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
  int nphi=24;

  printf("init histo ::%s\n", GetName());

  hst[0]=new TH1F("rpt0","all recon events vs. LP rPt (GeV/c)",npt,pt0,pt1);
  hst[1]=new TH1F("rpt1","after LP tuned cuts=AC vs. LP rPt (GeV/c)",npt,pt0,pt1);
  hst[2]=new TH1F("sID","valid spinID, AC",5,-0.5,4.5);
  // [3] not used

  hst[4]=new TH1F("ph1","rLP vs. #phi(deg), all AC",nphi*10,0.,360.);

  // spin dependent histos
  hpol[0]=new TH1F("p0","rLP vs. #phi(deg) for VOID Pol",nphi,0.,360.);
  hpol[1]  =new TH1F("p1","rLP vs. #phi(deg) for +Pol",nphi,0.,360.);
  hpol[2]=new TH1F("p2","rLP vs. #phi(deg) for -Pol",nphi,0.,360.);
  hpol[3]  =new TH1F("p3","rLP vs. #phi(deg) for 0-Pol",nphi,0.,360.);
  
}

