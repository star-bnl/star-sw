// Hey Emacs this is -*-c++-*-
#ifndef STAR_EEmcTT2NTMaker
#define STAR_EEmcTT2NTMaker
// $Id: EEmcTT2NTMaker.h,v 1.1 2004/05/05 22:04:15 zolnie Exp $

/*!
 *                                                                     
 * \class  EEmcTT2NTMaker
 * \author Piotr A. Zolnierczuk
 * \date   2004/05/04
 *
 * \brief  FIXME
 *
 * 
 * 
 */                                                                      
const int       kNTupleTTM_MaxTracks  =  128;
const int       kNTupleTTM_MaxTrigger =   32;

class EEmcTT2NTMaker : public StMaker {

public:
  /// structure to hold the results from EEmcTTMMaker
  struct NTupleTTM_t {
    Int_t    numtracks;                      /**<- number of tracks */
    Int_t    sector  [kNTupleTTM_MaxTracks]; /**<- sector */
    Int_t    subsec  [kNTupleTTM_MaxTracks]; /**<- subsector */
    Int_t    etabin  [kNTupleTTM_MaxTracks]; /**<- tower# a.k.a eta bin */
    Float_t  adc     [kNTupleTTM_MaxTracks]; /**<- pedestal subtracted adc = adc - pedestal */
    Float_t  edep    [kNTupleTTM_MaxTracks]; /**<- energy deposited : (adc-pedestal)/gain */
    Int_t    ntrack  [kNTupleTTM_MaxTracks]; /**<- number of tracks in an event that hit this tower */
    //
    Float_t  pt      [kNTupleTTM_MaxTracks]; /**<- track transverse momentum */
    Float_t  ptot    [kNTupleTTM_MaxTracks]; /**<- track total momentum  */
    Int_t    nhits   [kNTupleTTM_MaxTracks]; /**<- number of hits/track */
    Float_t  length  [kNTupleTTM_MaxTracks]; /**<- track length */
    Float_t  dedx    [kNTupleTTM_MaxTracks]; /**<- track energy loss (dE/dx) */
    Float_t  etatrk  [kNTupleTTM_MaxTracks]; /**<- track pseudorapidity  */
    
    //
    Float_t  detasmd [kNTupleTTM_MaxTracks]; /**<- distance in eta between track hit and the tower center */
    Float_t  dphismd [kNTupleTTM_MaxTracks]; /**<- distance in phi between track hit and the tower center */
    
    // for Trigger Info 
    Int_t    numtrig;                        /**<- number of trigger present */
    Int_t    trigid[kNTupleTTM_MaxTrigger];  /**<- a list of trigger id's    */
    Int_t    daqbits;                        /**<- DAQ trigger bits          */
    Int_t    ctbsum;                         /**<- CTB sum                   */

    /// zeroes the structure
    inline void Clear() { memset(this,0x00,sizeof(*this)); }
  };
  
  



}



#endif
