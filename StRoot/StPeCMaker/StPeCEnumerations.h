//////////////////////////////////////////////////////////////////////
//
// $Id: StPeCEnumerations.h,v 1.8 2015/02/25 01:23:07 ramdebbe Exp $
// $Log: StPeCEnumerations.h,v $
// Revision 1.8  2015/02/25 01:23:07  ramdebbe
// fixed StPeCnMaxTracks =6
//
// Revision 1.7  2012/06/13 15:46:53  ramdebbe
// Control the number of tracks for UPC selection
//
// Revision 1.6  2003/11/25 01:54:25  meissner
// correct several bugs: eta cut for tracks, charge sorting, add counting of FTPC and TPC primary tracks, Add bbc information
//
// Revision 1.5  2001/04/27 19:32:07  akio
// fixing a #define conflict bug
//
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

const double mMassElectron =0.000510999;
const double mMassMuon     =0.105658389;
const double mMassPion     =0.139567;
const double mMassKaon     =0.493677;
const double mMassProton   =0.93827231;

// Maximmum number of tracka
const int  StPeCnMaxTracks =6;
// maximu number of snapShots
const int snapLimit = 0;
#endif
