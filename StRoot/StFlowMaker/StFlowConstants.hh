//////////////////////////////////////////////////////////////////////
//
// $Id: StFlowConstants.hh,v 1.1 1999/12/15 22:01:20 posk Exp $
//
// Author: Art Poskanzer and Raimond Snellings 
//////////////////////////////////////////////////////////////////////
//
// Description: constants for the flow makers
//////////////////////////////////////////////////////////////////////
//
// $Log: StFlowConstants.hh,v $
// Revision 1.1  1999/12/15 22:01:20  posk
// Added StFlowConstants.hh
//
// Revision 1.0  posk
//
//////////////////////////////////////////////////////////////////////
#ifndef StFlowConstants_hh
#define StFlowConstants_hh

struct Flow{

  enum { nHars    =  6, 
	 nSels    =  2,
	 nSubs    =  2,
	 nPhiBins = 60 };

  typedef Double_t PhiWgt_t[nSels][nHars][nPhiBins];

};

#endif
