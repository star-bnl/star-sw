//*-- Author : Jan Balewski
//  
// $Id: LeadPartAnal.h,v 1.2 2001/02/28 19:06:12 balewski Exp $
// $Log: LeadPartAnal.h,v $
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



#define MX_LPA 2 // I consider the leading & next to leading charged particle
#define MX_GLPT 15 //  LP pT range bins

struct RecStep
{
  int n;
  TH1D *h;
};

struct LeadPartAnal
{
  struct  RecStep Inp, Trig, Acc, dPT, dPsi; // keep them alike
};
//, Vert, Det, Rec,Prim, Lead, DrPt

static  char *tt1[]={"Inp","Trig","Acc", "dPT", "dPsi"};

