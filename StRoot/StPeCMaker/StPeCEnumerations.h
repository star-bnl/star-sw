//////////////////////////////////////////////////////////////////////
//
// $Id: StPeCEnumerations.h,v 1.4 2001/04/23 21:44:29 meissner Exp $
// $Log: StPeCEnumerations.h,v $
// Revision 1.4  2001/04/23 21:44:29  meissner
// add dEdx z variable to tree, setFormat(1) for tree, use private BetheBloch (temp solution)
//
// Revision 1.3  2001/02/12 21:15:40  yepes
// New version of StPeCMaker, lots of changes
//
// Revision 1.2  2000/04/21 19:11:47  nystrand
// Add muons, electrons
//
// Revision 1.1  2000/03/24 22:38:11  nystrand
// First version
//
// Revision 1.0  2000/03/20 23:28:50  nystrand
//
//////////////////////////////////////////////////////////////////////
#ifndef StPeCEnumerations_hh
#define StPeCEnumerations_hh
const Int_t nSpecies = 5 ;
enum StPeCSpecies {pion, kaon, proton, electron, muon};


#define mElectron 0.000510999
#define mMuon     0.105658389
#define mPion     0.139567
#define mKaon     0.493677
#define mProton   0.93827231


// Maximmum number of tracka
#define StPeCnMaxTracks 10


#endif
