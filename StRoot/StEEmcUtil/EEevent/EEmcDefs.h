// Hey Emacs this is really -*-c++-*- ! 
// \author Piotr A. Zolnierczuk    Aug 26, 2002
#ifndef EEmcDefs_h
#define EEmcDefs_h
/*********************************************************************
 * $Id: EEmcDefs.h,v 1.1 2003/01/28 23:16:07 balewski Exp $
 *********************************************************************
 * Descripion:
 * STAR Endcap Electromagnetic Calorimeter Definitions (temp file)
 *********************************************************************
 * $Log: EEmcDefs.h,v $
 * Revision 1.1  2003/01/28 23:16:07  balewski
 * start
 *
 * Revision 1.7  2002/11/12 20:08:30  balewski
 * some cleanup
 *
 * Revision 1.6  2002/11/11 21:22:48  balewski
 * EEMC added to StEvent
 *
 * Revision 1.5  2002/10/03 15:52:25  zolnie
 * updates reflecting changes in *.g files
 *
 * Revision 1.4  2002/09/30 21:58:26  zolnie
 * do we understand Oleg? (the depth problem)
 *
 * Revision 1.3  2002/09/30 20:15:55  zolnie
 * Oleg's geometry updates
 *
 * Revision 1.2  2002/09/24 22:47:34  zolnie
 * major rewrite: SMD incorporated, use constants rather hard coded numbers
 * 	introducing exceptions (rather assert)
 *
 * Revision 1.1.1.1  2002/09/19 18:58:54  zolnie
 * Imported sources
 *
 * Revision 1.1.1.1  2002/08/29 19:32:01  zolnie
 * imported sources
 *
 * Revision 1.2  2002/08/28 01:43:15  zolnie
 * version alpha
 *
 * Revision 1.1  2002/08/26 19:50:45  zolnie
 * Initial revision
 *
 *********************************************************************/

enum EEmcVolId {
  // for Tower
  kEEmcTowerHalfId =  100000,
  kEEmcTowerPhiId  =    1000,
  kEEmcTowerEtaId  =      10,
  kEEmcTowerDepId  =       1,
  
  // for SMDs
  kEEmcSmdHalfId   = 1000000,
  kEEmcSmdPhiId    =   10000,
  kEEmcSmdPlaneId  =    1000,
  kEEmcSmdStripId  =        1
};




const int kEEmcNumDepths     =  5;
const int kEEmcNumSectors    = 12;
const int kEEmcNumSubSectors =  5;
const int kEEmcNumEtas       = 12;
const int kEEmcNumStrips     =288;



#endif
