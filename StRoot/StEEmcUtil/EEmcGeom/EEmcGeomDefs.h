// Hey Emacs this is really -*-c++-*- ! 
// \author Piotr A. Zolnierczuk    Aug 26, 2002
#ifndef EEmcGeom_EEmcDefs_h
#define EEmcGeom_EEmcDefs_h
/*********************************************************************
 * $Id: EEmcGeomDefs.h,v 1.2 2003/03/06 18:54:20 zolnie Exp $
 *********************************************************************
 * Descripion:
 * STAR Endcap Electromagnetic Calorimeter Definitions (temp file)
 *********************************************************************
 * $Log: EEmcGeomDefs.h,v $
 * Revision 1.2  2003/03/06 18:54:20  zolnie
 * improvements for track/tower matching
 *
 * Revision 1.1  2003/02/20 21:26:58  zolnie
 * added simple geometry class
 *
 * Revision 1.1  2003/02/20 21:15:16  zolnie
 * *** empty log message ***
 *
 *********************************************************************/

const int kEEmcNumDepths     =  5;
const int kEEmcNumSectors    = 12;
const int kEEmcNumSubSectors =  5;
const int kEEmcNumEtas       = 12;
const int kEEmcNumStrips     =288;


const Float_t kEEmcZPRE1=270.190; // [cm] z preshower1
const Float_t kEEmcZPRE2=271.695; // [cm] z preshower2
const Float_t kEEmcZSMD =279.542; // [cm] z location of the EEMC SMD layer 
const Float_t kEEmcZPOST=306.158; // [cm] z postshower  


#endif
/* ------------------------------------------
   MEGA 1    270.19        -   preshower 1
   MEGA 2    271.695       -   preshower 2
   MEGA 3    273.15 
   MEGA 4    274.555
   MEGA 5    275.96
   MEGA 6    277.365
   ------------------
   SMD  1    278.327
   SMD  2    279.542
   SMD  3    280.757
   ------------------
   MEGA 7    282.3630
   MEGA 8    283.7680
   MEGA 9    285.1730
   MEGA 10   286.5780
   MEGA 11   287.9830
   MEGA 12   289.3880
   MEGA 13   290.7930
   MEGA 14   292.1980
   MEGA 15   293.6030
   MEGA 16   295.0080
   MEGA 17   296.4130
   MEGA 18   297.8180
   MEGA 19   299.2230
   MEGA 20   300.6280
   MEGA 21   302.0330
   MEGA 22   303.4380
   MEGA 23   304.8430
   MEGA 24   306.1580          - postshower 
   ------------------------------------------*/
