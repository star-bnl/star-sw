//*-- Author : Jan Balewski
//  
// $Id: LeadPartAnal.h,v 1.1.1.2 2001/04/21 00:43:13 fisyak Exp $
// $Log: LeadPartAnal.h,v $
// Revision 1.1.1.2  2001/04/21 00:43:13  fisyak
// *** empty log message ***
//
// Revision 1.3  2001/04/12 15:19:08  balewski
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
//                                                                      //
//  here all counters & histogram for evaluation of the Leading         //
// Particle analysis are defined                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////



#define MX_GLPT 15 //  LP pT range bins

struct RecStep
{
  int n;
  TH1D *h;
};

struct LeadPartAnal
{
  struct  RecStep Rec,  dPT, dPsi; // keep them alike
};

static  char *tt1[]={"Rec", "dPT", "dPsi"};



