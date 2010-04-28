/***************************************************************************
 *
 * $Id: StMcHitC.cxx,v 2.2 2010/04/28 20:15:45 fine Exp $
 * $Log: StMcHitC.cxx,v $
 * Revision 2.2  2010/04/28 20:15:45  fine
 * Implementation if the new OO for Mc hits
 *
 * Revision 2.1  2010/04/28 18:10:10  fine
 * New OO model for Mc event components
 *
 * Revision 2.10  2005/11/22 21:44:52  fisyak
 * Add compress Print for McEvent, add Ssd collections
  *
 *
 **************************************************************************/
#include "StMcHitC.hh"

//_____________________________________________________________________________
//
//       Ctf  hits
//_____________________________________________________________________________
void StMcCtfHitC::get_slat_tray(unsigned int & slat, unsigned int & tray) const
{
   // From StMcEvent/StMcCtbHit.cc
    long i1 ;
    unsigned int volume = volumeId();
    int i_phi = static_cast<int>(fmod(volume,100.)) ;
    int i_eta = 999;
    slat = 999;
    tray = 999;
    
    i1     = int(volume/100) ;
    if ( i1 < 20 ) {
	i_phi = 14 - i_phi ;
	if ( i_phi < 1 ) i_phi = i_phi + 60 ;
	if ( i1 == 11 ) i_eta = 3 ;
	else 
	    if ( i1 == 12 ) i_eta = 4 ;
    }
    else if ( i1 > 20 ) {
	i_phi = i_phi - 42 ;
	if ( i_phi < 1 ) i_phi = i_phi + 60 ;
	if ( i1 == 21 ) i_eta = 2 ;
	else 
	    if ( i1 == 22 ) i_eta = 1 ;
    }

    i_phi--;
    i_eta--;
    
    if(i_eta == 0){
        slat = 1;
        tray = i_phi + 102;
        if(tray > 119)
	    tray-= 60;
    }
    if(i_eta == 1){
        slat = 0;
        tray = i_phi + 102;
        if(tray > 119)
	    tray-= 60;
    }
    if(i_eta == 2){
        slat = 0;
        tray = 12 - i_phi;
        if(tray < 0)
	    tray += 60;
    }
    if(i_eta == 3){
        slat = 1;
        tray = 12 - i_phi;
        if(tray < 0)
	    tray += 60;
    }
}
//_____________________________________________________________________________
//
//       Fgt hits
//_____________________________________________________________________________
unsigned long StMcFgtHitC::layer() const
{
  unsigned long iLayer; // layer = disk in StFgtGeom

  // volumeId encoded in UPGR16  
  int numbv1 = volumeId()/1000000;
  int numbv2 = (volumeId()/10000)%100;
  if(numbv2 != 0) iLayer = (unsigned long) numbv1 - 1;
  else iLayer = 8;
  return iLayer; 
}
//_____________________________________________________________________________
unsigned long StMcFgtHitC::quad() const
{
  unsigned long iQuad;

  // volumeId encoded in UPGR16  
  int numbv1 = volumeId()/1000000;
  int numbv2 = (volumeId()/10000)%100;
  if(numbv2 != 0) iQuad = (unsigned long) (numbv2 - 1);
  else iQuad = (unsigned long) (numbv1 - 1); 
  
  return iQuad;
}

//_____________________________________________________________________________
//
//       Ftpc hits
//_____________________________________________________________________________
unsigned long StMcFtpcHitC::plane() const
{
    // 1-20, where 1-10 = West and 11-20 = East
   int vid = volumeId();
    if (vid <1000) return (vid /100 - 1)*10 + (vid)%100; // for backward compatibility

    //new encoding from Maria: 1st (west) or 2nd (east) FTPC * 1000 + plane * 100 + sector
    //volume_id = 101? to 110? are the first FTPC (1-10 in StEvent), last digit is sector (below)
    //volume_id = 201? to 210? are the second FTPC (11-20 in StEvent), last digit is sector (below)
    return (vid /1000 - 1)*10 + (vid/10)%100;
}

//_____________________________________________________________________________
unsigned long StMcFtpcHitC::sector() const 
{  // 1-6
   int mVolumeId = volumeId();
   if (mVolumeId < 1000) return 99999; // for backward compatibility
    //volume_id = 1??1 to 1??6 are the sectors in the first FTPC (1-6 in StEvent)
    //volume_id = 2??1 to 2??6 are the sectors in the second FTPC (1-6 in StEvent)
    return mVolumeId%10;
}

//_____________________________________________________________________________
//
//       Ist hits
//_____________________________________________________________________________
unsigned long StMcIstHitC::layer() const
{
  //Only one ist layer now (WL, 03/13/08)
  /*
  unsigned long iLayer = mVolumeId/1000000;
  unsigned long layer;
  if(iLayer<4) layer=iLayer-1;
  else 
    {
      cout << "StMcIstHit::layer() -E- volumeId not known!" << endl;
      layer = 10000; 
    }
  
  return layer;
  */
  return 1;
}
//_____________________________________________________________________________
unsigned long StMcIstHitC::ladder() const
{      
  //unsigned long iModule = (mVolumeId%1000000)/10000;
  unsigned long iLadder=volumeId()/1000000-1;
  /*
  unsigned long iLadder = 0;
  if(iModule<=24) iLadder=iModule;
  else 
    {
      cout << "StMcIstHit::ladder() -E- volumeId not known!" << endl;
      iLadder = 10000; 
    }
  */
  return iLadder;
}
//_____________________________________________________________________________
unsigned long StMcIstHitC::wafer() const
{
    // Willie: Added function wafer() to return wafer number (1-12)
  unsigned long iWafer=(volumeId()%1000000)/10000;
  return iWafer;
}


//_____________________________________________________________________________
//
//       Pixel hits
//_____________________________________________________________________________
unsigned long StMcPixelHitC::layer() const
{
   // 1-2
  unsigned long iLayer=volumeId()/1000000;
  return iLayer;
}

//_____________________________________________________________________________
unsigned long StMcPixelHitC::ladder() const
{
    // 1-6, 1-18
  unsigned long iLadder = (volumeId()%1000000)/10000;
  return iLadder;
}

StMcHitC a(0);

