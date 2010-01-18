// $Id: WanaConst.h,v 1.2 2010/01/18 03:26:15 balewski Exp $
//
//*-- Author : Jan Balewski, MIT


#ifndef WANA_CONST_HH
#define WANA_CONST_HH
// bsmd indexes needed for Jan's calibration
enum {mxBStrips=18000, mxBSmd=2, kBSE=0, kBSP=1}; 
enum {mxBtow=4800, mxBTile=2, kBTow=0, kBPrs=1} ; // tower, preshower indexes
//enum {mxBprsCrate=4};
enum {mxBTphiBin=120}; // # of phi bins for towers, preshower
enum {mxBTetaBin=40}; // # of eta bins for towers, preshower
enum {mxBetaStrMod=150}; // # of Eta strip in module
enum {mxBMod2Pi=60}; // # of barrel modules over 2pi strip in module
//enum {mxBphiStrBand=900}; // # of Phi strip in Barrel at fixed eta
//enum {mxBXcell=20}; // dimension of 2D eta-phi array of cells,?? mxBYcell=60

enum {mxEtow=720,mxEtowSec=12,mxEtowSub=5,mxEtowEta=12}; 
enum {mxEtowPhiBin=mxEtowSec*mxEtowSub};                                  

enum {mxTpcSec=24};
#endif


// $Log: WanaConst.h,v $
// Revision 1.2  2010/01/18 03:26:15  balewski
// expanded TPC track filtering, not finished
//
// Revision 1.1  2009/11/23 23:00:18  balewski
// code moved spin-pool
//
