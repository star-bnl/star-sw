// $Id: GeometryConfiguration.h,v 1.12 2009/01/21 16:22:37 fisyak Exp $
// $Log: GeometryConfiguration.h,v $
// Revision 1.12  2009/01/21 16:22:37  fisyak
// Split SSD ladders into sectors
//
// Revision 1.11  2009/01/16 16:01:14  fisyak
// Adjust SVT geometry topology for alignment
//
// Revision 1.10  2009/01/14 16:31:52  fisyak
// Freeze conversion from mortran to Cint for SVT
//
// Revision 1.9  2008/12/30 21:34:16  fisyak
// Prepare for geometries clean up
//
// Revision 1.8  2008/12/30 21:10:32  fisyak
// Prepare for geometries clean up
//
// Revision 1.7  2008/12/30 00:25:08  fisyak
// Remove complete and dev2005 tags
//
// Revision 1.6  2008/11/17 14:18:30  fisyak
// *** empty log message ***
//
// Revision 1.5  2008/09/08 14:42:10  fisyak
// import geomertry annotations from geometry.g
//
// Revision 1.4  2008/09/03 20:44:46  fisyak
// replace tpc geometry with translate one from mortran, clean ups
//
// Revision 1.3  2008/08/27 21:48:11  fisyak
//
// the STAR geometry configuration
/*                                                          base geometry (by time tag)
  Run 0 : CY99/FY99    => 06/07/99 - 08/16/99               year2000
  Run 1 : CY00/FY00    => 09/07/00 - 09/19/00 AuAu          year2000
  Run 2 :   01/02      => 11/28/01 - 01/25/02 AuAu          year2001
  Run 3 : CY02-03/FY03 => 03/26/03 - 05/30/03 pp, dAu       y2003c => new SVT
  Run 4 : CY03-04/FY04 => 04/02/04 - 05/15/04 AuAu, pp      y2004d => new SVT
  Run 5 : CY04-05/FY05 => 05/31/05 - 06/03/05 pp, CuCu      y2005g
  Run 6 : CY05-06/FY06 => 07/10/06 - 07/10/06 pp            y2006g
  Run 7 : CY06-07/FY07 => 06/06/07            AuAu          y2007g
  Run 8 : CY07-08/FY08 => 03/11/08            dAu           y2008

  Geometry used in Production so far (12/30/08)
___________________________________________________________________________________________________________________
MC:                                    | rawData:             |Not used                      |Keep
___________________________________________________________________________________________________________________
2000: y1a,y1b,y1h,y2000,year_1a,year_1b|y1h, y2000a (?), y2000|                              | year2000
2001: y2001                            |y2001                 |                              | year2001
2002: y2a,y2b,y2x,y2y,year_2a          |                      |y2002                         | year_2a ?
2003: y2003,y2003x                     |y2003                 |y2003a,y2003b                 | year2003c
2004: y2004a, y2004c,y2004,y2004y      |y2004                 |y2004b,y2004x                 | y2004d
2005: y2005,y2005x,y2005g              |y2005b,y2005f         |y2005c,y2005d,y2005e          | y2005g
2006: y2006c,y2006,y2006g              |y2006,y2006g          |y2006a,y2006b                 | y2006g
2007: y2007g,y2007                     |y2007,y2007g          |y2007a                        | y2007g
2008: y2008                            |y2008                 |                              | y2008
___________________________________________________________________________________________________________________
      tpcOnly,dev2005,upg21            |                      |				     | tpcOnly
upgr: 01,05,06,07,08,09,10,11,13       |                      |02,03,04,14,15,20,21	     | 01,05,06,07,08,09,10,11,13
                                       |                      |complete,dev2005,ist1,pix1    |
_____________________________________________________________________________________________|______________________
*/											     
#ifdef tpcOnly /* for tests only */							     
/*************************************************************************************************************
  on TPC_ONLY   { Minimal geometry - only TPC;
                  {PIPE,SVTT,ftpc,BTOF,VPDD,CALB,ECAL,MAGP,UPST,ZCAL,PHMD,FPDM,BBCM,SISD,FTRO}=off; }
*************************************************************************************************************/
  #define caveConfig   1 /* tpcOnly */
  #define hallConfig   1 /* tpcOnly */
  #define pipeConfig   1 /* tpcOnly */
  #define tpcConfig    3 /* tpcOnly */
#endif
#ifdef svtOnly /* for tests only */							     
/*************************************************************************************************************
  on TPC_ONLY   { Minimal geometry - only TPC;
                  {PIPE,SVTT,ftpc,BTOF,VPDD,CALB,ECAL,MAGP,UPST,ZCAL,PHMD,FPDM,BBCM,SISD,FTRO}=off; }
*************************************************************************************************************/
  #define caveConfig   1 /* tpcOnly */
  #define hallConfig   1 /* tpcOnly */
  #define pipeConfig   1 /* tpcOnly */
  #define svtConfig   13 /* svtOnly */
#endif
#ifdef year_2b
/************************************************************************************************************* 
  on YEAR_2B    { old 2001 geometry first guess - TPC+CTB+FTPC+RICH+CaloPatch+SVT;
                  BtofConfig=4;
                  {RICH,ems}=on;  nmod={24,0}; shift={21,0};  
                  nonf={0,2,2};  Itof=2;  Rv=2;                        nSi=6; }
*************************************************************************************************************/
  #define btofConfig   2 /* year_2b */
  #define calbConfig   2 /* year_2b */
  #define caveConfig   1 /* year_2b */
  #define ftpcConfig   1 /* year_2b */
  #define hallConfig   1 /* year_2b */
  #define ibemConfig   1 /* year_2b */
  #define magpConfig   1 /* year_2b */
  #define pipeConfig   1 /* year_2b */
  #define richConfig   1 /* year_2b */
  #define sconConfig   1 /* year_2b */
  #define supportConfig   1 /* year_2b */
  #define svtConfig   13 /* 2 => 13 year_2b */
  #define tpcConfig   1 /* year_2b */
  #define upstreamConfig   1 /* year_2b */
  #define vpdConfig   1 /* year_2b */
  #define zcalConfig   1 /* year_2b */
#endif /* year_2b */
#ifdef year_2a
/*************************************************************************************************************
  on YEAR_2A    { old asymptotic STAR;    Itof=1; mwx=1;  BBCM=on;            }
*************************************************************************************************************/
  #define bbcConfig   1 /* year_2a */
  #define calbConfig   3 /* year_2a */
  #define caveConfig   1 /* year_2a */
  #define ftpcConfig   1 /* year_2a */
  #define hallConfig   1 /* year_2a */
  #define ibemConfig   1 /* year_2a */
  #define magpConfig   1 /* year_2a */
  #define pipeConfig   1 /* year_2a */
  #define sconConfig   1 /* year_2a */
//  #define ssdConfig    1 /* remove it at all year_2a */
  #define supportConfig   1 /* year_2a */
  #define svtConfig   13 /* 4 => 13 year_2a */
  #define tpcConfig   1 /* year_2a */
  #define upstreamConfig   1 /* year_2a */
  #define vpdConfig   1 /* year_2a */
  #define zcalConfig   1 /* year_2a */
#endif /* year_2a */
#ifdef year2000
/*************************************************************************************************************
* corrected: MWC readout, RICH reconstructed position, no TOF 
  on YEAR2000   { actual 2000:  TPC+CTB+RICH+caloPatch+svtLadder; 
                  {VPDD,ECAL,FTPC,svtw}=off; {RICH,ems}=on; Field=2.5; 
                  nmod={12,0}; shift={87,0}; Rp=2; Rv=2; Wfr=7; Mf=3;  nSi=-3;}
*************************************************************************************************************/
  #define btofConfig   1 /* year2000 */
  #define calbConfig   1 /* year2000 */
  #define caveConfig   1 /* year2000 */
  #define hallConfig   1 /* year2000 */
  #define magpConfig   1 /* year2000 */
  #define pipeConfig   1 /* year2000 */
  #define richConfig   1 /* year2000 */
  #define sconConfig   1 /* year2000 */
  #define svtConfig    1 /* single ladder  year2000 */
  #define tpcConfig    1 /* year2000 */
  #define upstreamConfig   1 /* year2000 */
  #define zcalConfig   1 /* year2000 */
#endif /* year2000 */
#ifdef year2001
/*************************************************************************************************************
  on YEAR2001   { 2001 geometry - TPC+CTB+FTPC+RICH+CaloPatch+SVT+FPD;

* 02/09/2004  Jerome signed off on changing, retroactively, the
* position of the wafers in year2001, which was incorrectly offset
* by 250 um insterad of 150 um.

*                    -- Obsoleted CorrNum = 1;
                     SvttConfig = 1; "SVTT version"
                     SupoConfig = 1; "FTPC Support"

                  BtofConfig=4;
                  {RICH,ems}=on;

* a newer way to steer ECAL:
                  ecal_config=1   " one ECAL patch, west "

* this was put here in recent versions (as of 1.50) and I believe this is wrong as
* it destroys compatibility with earlier code: --max--
*    ECAL=off;  
                  nmod={24,0}; shift={21,0}; Itof=2; Rv=2; Mf=3;       nSi=6; }  
*************************************************************************************************************/
  #define btofConfig   2 /* year2002 */
  #define calbConfig   2 /* year2002 */
  #define caveConfig   1 /* year2002 */
  #define ecalConfig   1 /* year2002 */
  #define ftpcConfig   1 /* year2002 */
  #define hallConfig   1 /* year2002 */
  #define ibemConfig   1 /* year2002 */
  #define magpConfig   1 /* year2002 */
  #define pipeConfig   1 /* year2002 */
  #define richConfig   1 /* year2002 */
  #define sconConfig   1 /* year2002 */
  #define supportConfig   1 /* year2002 */
  #define svtConfig   13 /* 3 => 13 year2002 */
  #define tpcConfig   1 /* year2002 */
  #define upstreamConfig   1 /* year2002 */
  #define vpdConfig   1 /* year2002 */
  #define zcalConfig   1 /* year2002 */
#endif /* year2001 */
#ifdef year2002
/*************************************************************************************************************
  on YEAR2002   { january 2002 geometry - TPC+CTB+FTPC+CaloPatch2+Rich+SVT3+BBC+FPD;
                  "svt: 3 layers ";
                     nSi=6        " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0        " numbring is in the code   ";
                     Wdm=0        " width is in the code      ";
                  "tpc: standard, i.e.  "
                     mwc=on       " Wultiwire chambers are read-out ";
                     pse=on       " inner sector has pseudo padrows ";
                  "RICH"
                     RICH=on      " have RICH ";
                     Rv=2;        " save additional (fake) hits "; 
                  "ctb: central trigger barrer ";
                     Itof=2       " call btofgeo2 ";
                     BtofConfig=4;
                  "CALB: barrel calorimeter "
                     ems=on       " sector version "
                     nmod={24,0}  " 24 sectors ";
                     shift={21,0} " starting from 21         "; 
                  "ECAL "
                     ECAL=off
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=2;
                  "field version "
                     Mf=4;      " tabulated field, with correction ";
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* year2002 */
  #define btofConfig   2 /* year2002 */
  #define calbConfig   2 /* year2002 */
  #define caveConfig   1 /* year2002 */
  #define fpdConfig   1 /* year2002 */
  #define ftpcConfig   1 /* year2002 */
  #define hallConfig   1 /* year2002 */
  #define ibemConfig   1 /* year2002 */
  #define magpConfig   1 /* year2002 */
  #define pipeConfig   1 /* year2002 */
  #define richConfig   1 /* year2002 */
  #define sconConfig   1 /* year2002 */
  #define supportConfig   1 /* year2002 */
  #define svtConfig   13 /* 2 => 13 year2002 */
  #define tpcConfig   1 /* year2002 */
  #define upstreamConfig   1 /* year2002 */
  #define vpdConfig   1 /* year2002 */
  #define zcalConfig   1 /* year2002 */
#endif /* year2002 */
#ifdef year2003
/*************************************************************************************************************
  on YEAR2003   { draft 2003 geometry - TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL;
                  "svt: 3 layers ";
                     nSi=6  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 "  btofgeo2  ";
                     BtofConfig=5;
                  "CALB" 
                     ems=on   "endcap "
                     nmod={60,0}; shift={0,0}; " 60 sectors "
                  "ECAL" 
                     ecal_config=1   "one ECAL patch, west "
                     ecal_fill=1     " sectors 2-5 filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=3;
                  "field version "
                     Mf=4;      "tabulated field, with correction "
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* year2003 */
  #define btofConfig   3 /* year2003 */
  #define calbConfig   4 /* year2003 */
  #define caveConfig   1 /* year2003 */
  #define ecalConfig   2 /* year2003 */
  #define fpdConfig   1 /* year2003 */
  #define ftpcConfig   1 /* year2003 */
  #define hallConfig   1 /* year2003 */
  #define ibemConfig   1 /* year2003 */
  #define magpConfig   1 /* year2003 */
  #define pipeConfig   1 /* year2003 */
  #define sconConfig   1 /* year2003 */
  #define supportConfig   1 /* year2003 */
  #define svtConfig   13  /* 2 => 13  year2003 */
  #define tpcConfig   1 /* year2003 */
  #define upstreamConfig   1 /* year2003 */
  #define vpdConfig   1 /* year2003 */
  #define zcalConfig   1 /* year2003 */
#endif /* year2003 */
#ifdef y2003a
/*************************************************************************************************************
* In y2003a:
*    removed serious bugs from SUPOGEO (incorrect positioning inside the SVT,
*    where is doesn't belong)
*    corrected CALB -- the shift variable (was 0,0 so the barrel wasn't tilted right)
*    corrected SVT  -- the layer radii (from 250 to 150 microns, see the svt code)
****************************************************************************************
  on Y2003A    { correction 1 in 2003 geometry - TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL;
                  "svt: 3 layers ";
                     nSi=6  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "CALB" 
                     ems=on
                     nmod={60,0}; shift={75,0}; " 60 sectors " 
                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=1     " sectors 2-5 filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=3;
                  "field version "
                     Mf=4;      "tabulated field, with correction "
*                    -- Obsoleted CorrNum = 1;
                     SvttConfig = 1; "SVTT version"
                     SupoConfig = 1; "FTPC Support"
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* year2003a */
  #define btofConfig   3 /* year2003a */
  #define calbConfig   5 /* year2003a */
  #define caveConfig   1 /* year2003a */
  #define ecalConfig   2 /* year2003a */
  #define fpdConfig   1 /* year2003a */
  #define ftpcConfig   1 /* year2003a */
  #define hallConfig   1 /* year2003a */
  #define ibemConfig   1 /* year2003a */
  #define magpConfig   1 /* year2003a */
  #define pipeConfig   1 /* year2003a */
  #define sconConfig   1 /* year2003a */
  #define supportConfig   1 /* year2003a */
  #define svtConfig   13  /* 3 => 13  year2003a */
  #define tpcConfig   1 /* year2003a */
  #define upstreamConfig   1 /* year2003a */
  #define vpdConfig   1 /* year2003a */
  #define zcalConfig   1 /* year2003a */
#endif /* y2003a */
#ifdef y2003b
/*************************************************************************************************************
* y2003b is y2003a, but with the extra material in the SVT
* This is actually an important case (i.e. the "most precise" geometry
* approximation for the early 2003 run) which we were lacking so far.
* This is achieved by setting CorrNum to 2.
* The endcap EMC has one third of one wheel, as before
* For more info on the extra material in SVT -- see web page
****************************************************************************************
  on Y2003B    { correction 2 in 2003 geometry: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL;
                  "svt: 3 layers ";
                     nSi=6  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "CALB" 
                     ems=on
                     nmod={60,0}; shift={75,0}; " 60 sectors " 
                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=1     " sectors 2-5 filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=3;
                  "field version "
                     Mf=4;      "tabulated field, with correction "
*                    -- Obsoleted CorrNum = 2;
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 2; "SVTT version"
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* y2003b */
  #define btofConfig   3 /* y2003b */
  #define calbConfig   5 /* y2003b */
  #define caveConfig   1 /* y2003b */
  #define ecalConfig   2 /* y2003b */
  #define fpdConfig   1 /* y2003b */
  #define ftpcConfig   1 /* y2003b */
  #define hallConfig   1 /* y2003b */
  #define ibemConfig   1 /* y2003b */
  #define magpConfig   1 /* y2003b */
  #define pipeConfig   1 /* y2003b */
  #define sconConfig   1 /* y2003b */
  #define supportConfig   1 /* y2003b */
  #define svtConfig   13  /* 5 => 13  y2003b */
  #define tpcConfig   1 /* y2003b */
  #define upstreamConfig   1 /* y2003b */
  #define vpdConfig   1 /* y2003b */
  #define zcalConfig   1 /* y2003b */
#endif /* y2003b */
#ifdef y2003c
/*************************************************************************************************************
  on Y2003C    { Better SVT model on top of 2003B: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL;
                  "svt: 3 layers ";
                     nSi=6  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "CALB" 
                     ems=on
                     nmod={60,0}; shift={75,0}; " 60 sectors " 
                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=1     " sectors 2-5 filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=3;
                  "field version "
                     Mf=4;      "tabulated field, with correction "
*                    -- Obsoleted CorrNum = 2;
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 6; "SVTT version"
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* y2003c */
  #define btofConfig   3 /* y2003c */
  #define calbConfig   5 /* y2003c */
  #define caveConfig   1 /* y2003c */
  #define ecalConfig   2 /* y2003c */
  #define fpdConfig   1 /* y2003c */
  #define ftpcConfig   1 /* y2003c */
  #define hallConfig   1 /* y2003c */
  #define ibemConfig   1 /* y2003c */
  #define magpConfig   1 /* y2003c */
  #define pipeConfig   1 /* y2003c */
  #define sconConfig   2 /* y2003c */
  #define supportConfig   1 /* y2003c */
  #define svtConfig   13  /* 6 => 13  y2003c */
  #define tpcConfig   1 /* y2003c */
  #define upstreamConfig   1 /* y2003c */
  #define vpdConfig   1 /* y2003c */
  #define zcalConfig   1 /* y2003c */
#endif /* y2003c */
#ifdef y2003x
/*************************************************************************************************************
  on Y2003X    { same as y2003b but with full calorimeters and PHMD
                  "svt: 3 layers ";
                     nSi=6  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "CALB" 
                     ems=on ;
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides" 
                  "ECAL" 
                     ecal_config=3   "both wheels"
                     ecal_fill=3     "all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=3;
                  "field version "
                     Mf=4;      "tabulated field, with correction "
*                    -- Obsoleted CorrNum = 2;
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 2; "SVTT version"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* y2003x */
  #define btofConfig   3 /* y2003x */
  #define calbConfig   3 /* y2003x */
  #define caveConfig   1 /* y2003x */
  #define ecalConfig   1 /* y2003x */
  #define fpdConfig   1 /* y2003x */
  #define ftpcConfig   1 /* y2003x */
  #define hallConfig   1 /* y2003x */
  #define ibemConfig   1 /* y2003x */
  #define magpConfig   1 /* y2003x */
  #define phmdConfig   1 /* y2003x */
  #define pipeConfig   1 /* y2003x */
  #define sconConfig   1 /* y2003x */
  #define supportConfig   1 /* y2003x */
  #define svtConfig   13  /* 5 => 13  y2003x */
  #define tpcConfig   1 /* y2003x */
  #define upstreamConfig   1 /* y2003x */
  #define vpdConfig   1 /* y2003x */
  #define zcalConfig   1 /* y2003x */
#endif /* y2003x */
#ifdef y2004x
/****************************************************************************************
* NOTE:  this geometry, y2004x, should logically follow the baseline y2004, as a block of code,
* however the parser we used isn't too good and it grabs the y2004 and then
* stumbles, hence the order of tags is sometimes important and we have to list
* the y2004x prior to y2004

  on Y2004X    { hypothetical 2004 geometry: full barrel. Standard cuts in PHMD.;
                  "svt: 3 layers ";
                     nSi=6  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
* note the upgrade with respect to previous years:
                     BtofConfig=7;

* note the full barrel same as in y2003x:
                  "CALB" 
                     ems=on ;
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides" 

                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

*                    -- Obsoleted CorrNum = 3;
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 3; "SVTT version"
                     DensConfig = 1; "gas density correction"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 2;
                }
****************************************************************************************/
  #define bbcConfig    1 /* y2004x */
  #define btofConfig   4 /* y2004x */
  #define calbConfig   3 /* y2004x */
  #define caveConfig   1 /* y2004x */
  #define ecalConfig   1 /* y2004x */
  #define fpdConfig    2 /* y2004x */
  #define ftpcConfig   1 /* y2004x */
  #define hallConfig   1 /* y2004x */
  #define ibemConfig   1 /* y2004x */
  #define magpConfig   1 /* y2004x */
  #define phmdConfig   1 /* y2004x */
  #define pipeConfig   1 /* y2004x */
  #define sconConfig   2 /* y2004x */
  #define ssdConfig    3 /* 2 => 3 y2004x */
  #define supportConfig   1 /* y2004x */
  #define svtConfig   13  /* 7 => 13  y2004x */
  #define tpcConfig   1 /* y2004x */
  #define upstreamConfig   1 /* y2004x */
  #define vpdConfig   1 /* y2004x */
  #define zcalConfig   1 /* y2004x */
#endif /* y2004x */
#ifdef y2004y
/*************************************************************************************************************
  on Y2004Y    { same as Y2004X but with the SVT chip correction+cone+better SSD+TPC backplane+FTRO
                  "svt: 3 layers ";
                     nSi=6  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
* note the upgrade with respect to previous years:
                     BtofConfig=7;

* note the full barrel same as in y2003x:
                  "CALB" 
                     ems=on ;
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides" 

                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 4; "SVTT version"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "FTPC configuration"
* Above:  Ar+C02 in ftpc

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 22;

                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 2;

                }

*************************************************************************************************************/
  #define bbcConfig   1 /* y2004y */
  #define btofConfig   4 /* y2004y */
  #define calbConfig   3 /* y2004y */
  #define caveConfig   1 /* y2004y */
  #define ecalConfig   1 /* y2004y */
  #define fpdConfig   2 /* y2004y */
  #define ftroConfig   1 /* y2004y */
  #define ftpcConfig   2 /* y2004y */
  #define hallConfig   1 /* y2004y */
  #define ibemConfig   1 /* y2004y */
  #define magpConfig   1 /* y2004y */
  #define phmdConfig   1 /* y2004y */
  #define pipeConfig   1 /* y2004y */
  #define sconConfig   3 /* y2004y */
  #define ssdConfig    3 /* y2004y */
  #define supportConfig   1 /* y2004y */
  #define svtConfig   13  /* 8 => 13  y2004y */
  #define tpcConfig   2 /* y2004y */
  #define upstreamConfig   1 /* y2004y */
  #define vpdConfig   1 /* y2004y */
  #define zcalConfig   1 /* y2004y */
#endif /* y2004y */
#ifdef y2004a
/*************************************************************************************************************
  on Y2004A    { baseline 2004 geometry: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL+PHMD with standard GSTPAR in PHMD; 
                  "svt: 3 layers ";
                     nSi=6  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
* note the upgrade with respect to previous years:
                     BtofConfig=7;

                  "CALB" 
                     ems=on
                     CalbConfig = 1
* remember that with this config, the following parameters have
* a different meaning because we have to (unfortunately) switch
* from divisions to copies and introduce a map, which DOES
* control the configuration
                     nmod={60,60}; shift={75,105}; " 60 sectors West plus 30 East split between 2 halves"

                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

*                    -- Obsoleted CorrNum = 3;
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 3; "SVTT version"
                     DensConfig = 1; "gas density correction"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 2;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* y2004a */
  #define btofConfig   4 /* y2004a */
  #define calbConfig   6 /* y2004a */
  #define caveConfig   1 /* y2004a */
  #define ecalConfig   1 /* y2004a */
  #define fpdConfig   2 /* y2004a */
  #define ftpcConfig   1 /* y2004a */
  #define hallConfig   1 /* y2004a */
  #define ibemConfig   1 /* y2004a */
  #define magpConfig   1 /* y2004a */
  #define phmdConfig   1 /* y2004a */
  #define pipeConfig   1 /* y2004a */
  #define sconConfig   2 /* y2004a */
  #define ssdConfig    3 /* 2 => 3 y2004a */
  #define supportConfig   1 /* y2004a */
  #define svtConfig   13  /* 7 => 13  y2004a */
  #define tpcConfig   1 /* y2004a */
  #define upstreamConfig   1 /* y2004a */
  #define vpdConfig   1 /* y2004a */
  #define zcalConfig   1 /* y2004a */
#endif /* y2004a */
#ifdef y2004b
/*************************************************************************************************************
  on Y2004B    { corrected 2004 geometry: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL+PHMD with standard GSTPAR in PHMD;
                  "svt: 3 layers ";
                     nSi=6  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
* note the upgrade with respect to previous years:
                     BtofConfig=7;

                  "CALB" 
                     CalbConfig = 1  " Please see note in Y2004A"
                     nmod={60,60}; shift={75,105}; " 60 sectors West plus 30 East split between 2 halves"

                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

*                    -- Obsoleted CorrNum = 3;
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 3; "SVTT version"
                     DensConfig = 1; "gas density correction"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 12;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* y2004b */
  #define btofConfig   4 /* y2004b */
  #define calbConfig   6 /* y2004b */
  #define caveConfig   1 /* y2004b */
  #define ecalConfig   1 /* y2004b */
  #define fpdConfig   2 /* y2004b */
  #define ftpcConfig   1 /* y2004b */
  #define hallConfig   1 /* y2004b */
  #define ibemConfig   1 /* y2004b */
  #define magpConfig   1 /* y2004b */
  #define phmdConfig   1 /* y2004b */
  #define pipeConfig   1 /* y2004b */
  #define sconConfig   2 /* y2004b */
  #define ssdConfig    3 /* 4 => 3 y2004b */
  #define supportConfig   1 /* y2004b */
  #define svtConfig   13  /* 7 => 13  y2004b */
  #define tpcConfig   1 /* y2004b */
  #define upstreamConfig   1 /* y2004b */
  #define vpdConfig   1 /* y2004b */
  #define zcalConfig   1 /* y2004b */
#endif /* y2004b */
#ifdef y2004c
/*************************************************************************************************************
  on Y2004C    { same as Y2004B but with the SVT chip correction+cone+better SSD+TPC backplane+FTRO
                  "svt: 3 layers ";
                     nSi=6  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
* note the upgrade with respect to previous years:
                     BtofConfig=7;

                  "CALB" 
                     CalbConfig = 1  " Please see note in Y2004A"
                     nmod={60,60}; shift={75,105}; " 60 sectors West plus 30 East split between 2 halves"

                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "


                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 4; "SVTT version"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"
* Above:  Ar+C02 in ftpc


                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

* second file version, 10 ladders
                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 22;

                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 2;
                }

*************************************************************************************************************/
  #define bbcConfig   1 /* y2004c */
  #define btofConfig   4 /* y2004c */
  #define calbConfig   6 /* y2004c */
  #define caveConfig   1 /* y2004c */
  #define ecalConfig   1 /* y2004c */
  #define fpdConfig   2 /* y2004c */
  #define ftroConfig   1 /* y2004c */
  #define ftpcConfig   2 /* y2004c */
  #define hallConfig   1 /* y2004c */
  #define ibemConfig   1 /* y2004c */
  #define magpConfig   1 /* y2004c */
  #define phmdConfig   1 /* y2004c */
  #define pipeConfig   1 /* y2004c */
  #define sconConfig   3 /* y2004c */
  #define ssdConfig    3 /* y2004c */
  #define supportConfig   1 /* y2004c */
  #define svtConfig   13  /* 8 => 13  y2004c */
  #define tpcConfig   2 /* y2004c */
  #define upstreamConfig   1 /* y2004c */
  #define vpdConfig   1 /* y2004c */
  #define zcalConfig   1 /* y2004c */
#endif /* y2004c */
#ifdef y2004d
/*************************************************************************************************************
  on Y2004D    { Better SVT on top of Y2004B
                  "svt: 3 layers ";
                     nSi=6  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
* note the upgrade with respect to previous years:
                     BtofConfig=7;

                  "CALB" 
                     CalbConfig = 1  " Please see note in Y2004A"
                     nmod={60,60}; shift={75,105}; " 60 sectors West plus 30 East split between 2 halves"

                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "


                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 6; "SVTT version"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"
* Above:  Ar+C02 in ftpc


                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

* second file version, 10 ladders
                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 22;

                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 2;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* y2004d */
  #define btofConfig   4 /* y2004d */
  #define calbConfig   6 /* y2004d */
  #define caveConfig   1 /* y2004d */
  #define ecalConfig   1 /* y2004d */
  #define fpdConfig   2 /* y2004d */
  #define ftroConfig   1 /* y2004d */
  #define ftpcConfig   2 /* y2004d */
  #define hallConfig   1 /* y2004d */
  #define ibemConfig   1 /* y2004d */
  #define magpConfig   1 /* y2004d */
  #define phmdConfig   1 /* y2004d */
  #define pipeConfig   1 /* y2004d */
  #define sconConfig   3 /* y2004d */
  #define ssdConfig    3 /* y2004d */
  #define supportConfig   1 /* y2004d */
  #define svtConfig   13  /* 6 => 13  y2004d */
  #define tpcConfig   2 /* y2004d */
  #define upstreamConfig   1 /* y2004d */
  #define vpdConfig   1 /* y2004d */
  #define zcalConfig   1 /* y2004d */
#endif /* y2004d */
#ifdef y2004
/*************************************************************************************************************
* Corrections and enhancements in y2004:
*    added the Photon Multiplicity Detector (PHMD)
*    The endcap EMC has one complete wheel in the west
*    To be done: 3/4 of the second half of the barrel!
*
*                >>>THIS IS THE MASTER GEOMETRY FOR THE SPRING'04<<<
*
****************************************************************************************
  on Y2004     { baseline 2004 geometry: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL+PHMD with low cuts GSTPAR in PHMD; ; 
                  "svt: 3 layers ";
                     nSi=6  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
* note the upgrade with respect to previous years:
                     BtofConfig=7;

                  "CALB" 
                     ems=on
                     CalbConfig = 1
* remember that with this config, the following parameters have
* a different meaning because we have to (unfortunately) switch
* from divisions to copies and introduce a map, which DOES
* control the configuration
                     nmod={60,60}; shift={75,105}; " 60 sectors West plus 30 East split between 2 halves"


                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

*                    -- Obsoleted CorrNum = 3;
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 3; "SVTT version"
                     DensConfig = 1; "gas density correction"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;

                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 2;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* y2004 */
  #define btofConfig   4 /* y2004 */
  #define calbConfig   6 /* y2004 */
  #define caveConfig   1 /* y2004 */
  #define ecalConfig   1 /* y2004 */
  #define fpdConfig   2 /* y2004 */
  #define ftpcConfig   1 /* y2004 */
  #define hallConfig   1 /* y2004 */
  #define ibemConfig   1 /* y2004 */
  #define magpConfig   1 /* y2004 */
  #define phmdConfig   1 /* y2004 */
  #define pipeConfig   1 /* y2004 */
  #define sconConfig   2 /* y2004 */
  #define ssdConfig    3 /* 2 => 3 y2004 */
  #define supportConfig   1 /* y2004 */
  #define svtConfig   13  /* 7 => 13  y2004 */
  #define tpcConfig   1 /* y2004 */
  #define upstreamConfig   1 /* y2004 */
  #define vpdConfig   1 /* y2004 */
  #define zcalConfig   1 /* y2004 */
#endif /* y2004 */
#ifdef y2005x
/*************************************************************************************************************
  on Y2005X    { first cut of full CALB 2005 geometry: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL+PHMD_FTRO;
                  "svt: 3 layers ";
                     nSi=6  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
* note the upgrade with respect to previous years:
                     BtofConfig=7;

* note the full barrel same as in y2003x:
                  "CALB" 
                     ems=on ;
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides" 

                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

*                    -- Obsoleted CorrNum = 3;
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 3; "SVTT version"
                     DensConfig = 1; "gas density correction"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 23; "second version, full barrel"


                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

                }
*************************************************************************************************************/
  #define bbcConfig   1 /* y2005x */
  #define btofConfig   4 /* y2005x */
  #define calbConfig   3 /* y2005x */
  #define caveConfig   1 /* y2005x */
  #define ecalConfig   1 /* y2005x */
  #define fpdConfig   2 /* y2005x */
  #define ftroConfig   1 /* y2005x */
  #define ftpcConfig   1 /* y2005x */
  #define hallConfig   1 /* y2005x */
  #define ibemConfig   1 /* y2005x */
  #define magpConfig   1 /* y2005x */
  #define phmdConfig   1 /* y2005x */
  #define pipeConfig   1 /* y2005x */
  #define sconConfig   3 /* y2005x */
  #define ssdConfig    8 /* 5 => 8 y2005x */
  #define supportConfig   1 /* y2005x */
  #define svtConfig   13  /* 7 => 13  y2005x */
  #define tpcConfig   1 /* y2005x */
  #define upstreamConfig   1 /* y2005x */
  #define vpdConfig   1 /* y2005x */
  #define zcalConfig   1 /* y2005x */
#endif /* y2005x */
#ifdef y2005b
/*************************************************************************************************************
  on Y2005B    { TPC,FTPC,SVT and SSD correction of 2005 geometry
                  "svt: 3 layers ";
                     nSi=6  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
* note the upgrade with respect to previous years:
                     BtofConfig=7;

                  "CALB" 
                     ems=on
                     CalbConfig = 1
* remember that with this config, the following parameters have
* a different meaning because we have to (unfortunately) switch
* from divisions to copies and introduce a map, which DOES
* control the configuration
                     nmod={60,60}; shift={75,105}; " 60 sectors West plus 30 East split between 2 halves"

                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 4; "SVTT version"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 24; "second version, full barrel with corrected radii"


                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 2;

                }
*************************************************************************************************************/
  #define bbcConfig   1 /* y2005b */
  #define btofConfig   4 /* y2005b */
  #define calbConfig   6 /* y2005b */
  #define caveConfig   1 /* y2005b */
  #define ecalConfig   1 /* y2005b */
  #define fpdConfig   2 /* y2005b */
  #define ftroConfig   1 /* y2005b */
  #define ftpcConfig   2 /* y2005b */
  #define hallConfig   1 /* y2005b */
  #define ibemConfig   1 /* y2005b */
  #define magpConfig   1 /* y2005b */
  #define phmdConfig   1 /* y2005b */
  #define pipeConfig   1 /* y2005b */
  #define sconConfig   3 /* y2005b */
  #define ssdConfig    8 /* 6 => 8 y2005b */
  #define supportConfig   1 /* y2005b */
  #define svtConfig   13  /* 8 => 13  y2005b */
  #define tpcConfig   2 /* y2005b */
  #define upstreamConfig   1 /* y2005b */
  #define vpdConfig   1 /* y2005b */
  #define zcalConfig   1 /* y2005b */
#endif /* y2005b */
#ifdef y2005c
/*************************************************************************************************************
  on Y2005C    { TPC,FTPC,SVT and SSD correction of 2005 geometry
                  "svt: 3 layers ";
                     nSi=6  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=4 " call btofgeo4 ";
* NEW CONFIG!
                     BtofConfig=8;

                  "CALB" 
                     ems=on
                     CalbConfig = 1
* remember that with this config, the following parameters have
* a different meaning because we have to (unfortunately) switch
* from divisions to copies and introduce a map, which DOES
* control the configuration
                     nmod={60,60}; shift={75,105}; " 60 sectors West plus 30 East split between 2 halves"

                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 4; "SVTT version"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 24; "second version, full barrel with corrected radii"


                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 2;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* y2005c */
  #define btofConfig   5 /* y2005c */
  #define calbConfig   6 /* y2005c */
  #define caveConfig   1 /* y2005c */
  #define ecalConfig   1 /* y2005c */
  #define fpdConfig   2 /* y2005c */
  #define ftroConfig   1 /* y2005c */
  #define ftpcConfig   2 /* y2005c */
  #define hallConfig   1 /* y2005c */
  #define ibemConfig   1 /* y2005c */
  #define magpConfig   1 /* y2005c */
  #define phmdConfig   1 /* y2005c */
  #define pipeConfig   1 /* y2005c */
  #define sconConfig   3 /* y2005c */
  #define ssdConfig    8 /* 6 => 8 y2005c */
  #define supportConfig   1 /* y2005c */
  #define svtConfig   13  /* 8 => 13  y2005c */
  #define tpcConfig   2 /* y2005c */
  #define upstreamConfig   1 /* y2005c */
  #define vpdConfig   1 /* y2005c */
  #define zcalConfig   1 /* y2005c */
#endif /* y2005c */
#ifdef y2005d
/*************************************************************************************************************
  on Y2005D    { Better SVT on top of Y2005C
                  "svt: 3 layers ";
                     nSi=6  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=4 " call btofgeo4 ";
* NEW CONFIG!
                     BtofConfig=8;

                  "CALB" 
                     ems=on
                     CalbConfig = 1
* remember that with this config, the following parameters have
* a different meaning because we have to (unfortunately) switch
* from divisions to copies and introduce a map, which DOES
* control the configuration
                     nmod={60,60}; shift={75,105}; " 60 sectors West plus 30 East split between 2 halves"

                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 6; "SVTT version"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 24; "second version, full barrel with corrected radii"


                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 2;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* y2005d */
  #define btofConfig   5 /* y2005d */
  #define calbConfig   6 /* y2005d */
  #define caveConfig   1 /* y2005d */
  #define ecalConfig   1 /* y2005d */
  #define fpdConfig   2 /* y2005d */
  #define ftroConfig   1 /* y2005d */
  #define ftpcConfig   2 /* y2005d */
  #define hallConfig   1 /* y2005d */
  #define ibemConfig   1 /* y2005d */
  #define magpConfig   1 /* y2005d */
  #define phmdConfig   1 /* y2005d */
  #define pipeConfig   1 /* y2005d */
  #define sconConfig   3 /* y2005d */
  #define ssdConfig    8 /* 6 => 8 y2005d */
  #define supportConfig   1 /* y2005d */
  #define svtConfig   13  /* 6 => 13  y2005d */
  #define tpcConfig   2 /* y2005d */
  #define upstreamConfig   1 /* y2005d */
  #define vpdConfig   1 /* y2005d */
  #define zcalConfig   1 /* y2005d */
#endif /* y2005d */
#ifdef y2005e
/*************************************************************************************************************
  on Y2005E    { Better SVT, bigger shield and SSD on top of Y2005C, and full barrel calorimeter
                  "svt: 3 layers ";
                     nSi=6  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=4 " call btofgeo4 ";
* NEW CONFIG!
                     BtofConfig=8;

* note the full barrel same as in y2003x:
                  "CALB" 
                     ems=on ;
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides" 

                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 6; "SVTT version"
                     SvshConfig = 2; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 35; "second version, full barrel with corrected radii"


                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 2;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* y2005e */
  #define btofConfig   5 /* y2005e */
  #define calbConfig   3 /* y2005e */
  #define caveConfig   1 /* y2005e */
  #define ecalConfig   1 /* y2005e */
  #define fpdConfig   2 /* y2005e */
  #define ftroConfig   1 /* y2005e */
  #define ftpcConfig   2 /* y2005e */
  #define hallConfig   1 /* y2005e */
  #define ibemConfig   1 /* y2005e */
  #define magpConfig   1 /* y2005e */
  #define phmdConfig   1 /* y2005e */
  #define pipeConfig   1 /* y2005e */
  #define sconConfig   3 /* y2005e */
  #define ssdConfig    8 /* 7 => 8 y2005e */
  #define supportConfig   1 /* y2005e */
  #define svtConfig   13  /* 9 => 13  y2005e */
  #define tpcConfig   2 /* y2005e */
  #define upstreamConfig   1 /* y2005e */
  #define vpdConfig   1 /* y2005e */
  #define zcalConfig   1 /* y2005e */
#endif /* y2005e */
#ifdef y2005f
/*************************************************************************************************************
  on Y2005F    { Y2005E + corrected SSD with gaps and dead area
                  "svt: 3 layers ";
                     nSi=6  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=4 " call btofgeo4 ";
* NEW CONFIG!
                     BtofConfig=8;

* note the full barrel same as in y2003x:
                  "CALB" 
                     ems=on ;
                     CalbConfig = 2
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides" 

                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 6; "SVTT version"
                     SvshConfig = 2; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 55; "fifth version, full barrel with corrected radii and dead area"


                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 2;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* y2005f */
  #define btofConfig   5 /* y2005f */
  #define calbConfig   7 /* y2005f */
  #define caveConfig   1 /* y2005f */
  #define ecalConfig   1 /* y2005f */
  #define fpdConfig   2 /* y2005f */
  #define ftroConfig   1 /* y2005f */
  #define ftpcConfig   2 /* y2005f */
  #define hallConfig   1 /* y2005f */
  #define ibemConfig   1 /* y2005f */
  #define magpConfig   1 /* y2005f */
  #define phmdConfig   1 /* y2005f */
  #define pipeConfig   1 /* y2005f */
  #define sconConfig   3 /* y2005f */
  #define ssdConfig    8 /* y2005f */
  #define supportConfig   1 /* y2005f */
  #define svtConfig   13  /* 9 => 13  y2005f */
  #define tpcConfig   2 /* y2005f */
  #define upstreamConfig   1 /* y2005f */
  #define vpdConfig   1 /* y2005f */
  #define zcalConfig   1 /* y2005f */
#endif /* y2005f */
#ifdef y2005g
/*************************************************************************************************************
  on Y2005G    { Y2005F + corrected corrected SVT dead volumes from Rene
                  "svt: 3 layers ";
                     nSi=6  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=4 " call btofgeo4 ";
* NEW CONFIG!
                     BtofConfig=8;

* note the full barrel same as in y2003x:
                  "CALB" 
                     ems=on ;
                     CalbConfig = 2
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides" 

                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig =11; "SVTT version"
                     SvshConfig = 2; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 55; "fifth version, full barrel with corrected radii and dead area"


                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 2;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* y2005g */
  #define btofConfig   5 /* y2005g */
  #define calbConfig   7 /* y2005g */
  #define caveConfig   1 /* y2005g */
  #define ecalConfig   1 /* y2005g */
  #define fpdConfig   2 /* y2005g */
  #define ftroConfig   1 /* y2005g */
  #define ftpcConfig   2 /* y2005g */
  #define hallConfig   1 /* y2005g */
  #define ibemConfig   1 /* y2005g */
  #define magpConfig   1 /* y2005g */
  #define phmdConfig   1 /* y2005g */
  #define pipeConfig   1 /* y2005g */
  #define sconConfig   3 /* y2005g */
  #define ssdConfig    8 /* y2005g */
  #define supportConfig   1 /* y2005g */
  #define svtConfig   13  /* 10 => 13 with adding SSSH = "the separation shield cylinder" y2005g */
  #define tpcConfig   2 /* y2005g */
  #define upstreamConfig   1 /* y2005g */
  #define vpdConfig   1 /* y2005g */
  #define zcalConfig   1 /* y2005g */
#endif /* y2005g */
#ifdef y2005
/*************************************************************************************************************
  on Y2005    { first cut of 2005 geometry: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL+PHMD_FTRO;
                  "svt: 3 layers ";
                     nSi=6  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
* note the upgrade with respect to previous years:
                     BtofConfig=7;

                  "CALB" 
                     ems=on
                     CalbConfig = 1
* remember that with this config, the following parameters have
* a different meaning because we have to (unfortunately) switch
* from divisions to copies and introduce a map, which DOES
* control the configuration
                     nmod={60,60}; shift={75,105}; " 60 sectors West plus 30 East split between 2 halves"

                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

*                    -- Obsoleted CorrNum = 3;
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 3; "SVTT version"
                     DensConfig = 1; "gas density correction"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 23; "second version, full barrel"


                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* y2005 */
  #define btofConfig   4 /* y2005 */
  #define calbConfig   6 /* y2005 */
  #define caveConfig   1 /* y2005 */
  #define ecalConfig   1 /* y2005 */
  #define fpdConfig   2 /* y2005 */
  #define ftroConfig   1 /* y2005 */
  #define ftpcConfig   1 /* y2005 */
  #define hallConfig   1 /* y2005 */
  #define ibemConfig   1 /* y2005 */
  #define magpConfig   1 /* y2005 */
  #define phmdConfig   1 /* y2005 */
  #define pipeConfig   1 /* y2005 */
  #define sconConfig   3 /* y2005 */
  #define ssdConfig    8 /* 5 => 8 y2005 */
  #define supportConfig   1 /* y2005 */
  #define svtConfig   13  /* 7 => 13  y2005 */
  #define tpcConfig   1 /* y2005 */
  #define upstreamConfig   1 /* y2005 */
  #define vpdConfig   1 /* y2005 */
  #define zcalConfig   1 /* y2005 */
#endif /* y2005 */
#ifdef y2006c
/*************************************************************************************************************
  on Y2006C   { Y2006B without the PHMD
                  "svt: 3 layers ";
                     nSi=6  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=4 " call btofgeo4 ";
                     BtofConfig=8;
* Full barrel in 2006
                  "CALB" 
                     ems=on ;
                     CalbConfig = 2
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides" 


                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 2 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 6; "SVTT version"
                     SvshConfig = 2; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     PHMD=off;
                     PhmdConfig = 0;

                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 55; "fifth version, full barrel newly corrected radii and dead area"


                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 3;
                  "Muon Trigger System"
                     MUTD = on;
                     MutdConfig = 1;
                  "We need a bigger Cave"
                     CaveConfig = 3;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* y2006c */
  #define btofConfig   5 /* y2006c */
  #define calbConfig   7 /* y2006c */
  #define caveConfig   2 /* y2006c */
  #define ecalConfig   1 /* y2006c */
  #define fpdConfig   3 /* y2006c */
  #define ftroConfig   1 /* y2006c */
  #define ftpcConfig   2 /* y2006c */
  #define hallConfig   2 /* y2006c */
  #define ibemConfig   1 /* y2006c */
  #define magpConfig   1 /* y2006c */
  #define mutdConfig   1 /* y2006c */
  #define pipeConfig   1 /* y2006c */
  #define sconConfig   3 /* y2006c */
  #define ssdConfig    8 /* y2006c */
  #define supportConfig   1 /* y2006c */
  #define svtConfig   13  /* 9 => 13  y2006c */
  #define tpcConfig   3 /* y2006c */
  #define upstreamConfig   1 /* y2006c */
  #define vpdConfig   1 /* y2006c */
  #define zcalConfig   1 /* y2006c */
#endif /* y2006c */
#ifdef y2006g
/*************************************************************************************************************
  on Y2006G   { Y2006C new SVT dead material
                  "svt: 3 layers ";
                     nSi=6  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=4 " call btofgeo4 ";
                     BtofConfig=8;
* Full barrel in 2006
                  "CALB" 
                     ems=on ;
                     CalbConfig = 2
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides" 


                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 2 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig =11; "SVTT version"
                     SvshConfig = 2; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     PHMD=off;
                     PhmdConfig = 0;

                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 55; "fifth version, full barrel newly corrected radii and dead area"


                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 3;
                  "Muon Trigger System"
                     MUTD = on;
                     MutdConfig = 1;
                  "We need a bigger Cave"
                     CaveConfig = 3;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* y2006g */
  #define btofConfig   5 /* y2006g */
  #define calbConfig   7 /* y2006g */
  #define caveConfig   2 /* y2006g */
  #define ecalConfig   1 /* y2006g */
  #define fpdConfig   3 /* y2006g */
  #define ftroConfig   1 /* y2006g */
  #define ftpcConfig   2 /* y2006g */
  #define hallConfig   2 /* y2006g */
  #define ibemConfig   1 /* y2006g */
  #define magpConfig   1 /* y2006g */
  #define mutdConfig   1 /* y2006g */
  #define pipeConfig   1 /* y2006g */
  #define sconConfig   3 /* y2006g */
  #define ssdConfig    8 /* y2006g */
  #define supportConfig   1 /* y2006g */
  #define svtConfig   13  /* 10 => 13 with adding SSSH = "the separation shield cylinder" y2005g */
  #define tpcConfig   3 /* y2006g */
  #define upstreamConfig   1 /* y2006g */
  #define vpdConfig   1 /* y2006g */
  #define zcalConfig   1 /* y2006g */
#endif /* y2006g */
#ifdef y2006b
/*************************************************************************************************************
  on Y2006B   { Y2006A + improved SSD with dead area + improved CALB
                  "svt: 3 layers ";
                     nSi=6  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=4 " call btofgeo4 ";
* NEW CONFIG!
                     BtofConfig=8;

* Full barrel in 2006
                  "CALB" 
                     ems=on ;
                     CalbConfig = 2
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides" 


                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 2 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 6; "SVTT version"
                     SvshConfig = 2; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 55; "fifth version, full barrel newly corrected radii and dead area"


                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 3;
                  "Muon Trigger System"
                     MUTD = on;
                     MutdConfig = 1;
                  "We need a bigger Cave"
                     CaveConfig = 3;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* y2006b */
  #define btofConfig   5 /* y2006b */
  #define calbConfig   7 /* y2006b */
  #define caveConfig   2 /* y2006b */
  #define ecalConfig   1 /* y2006b */
  #define fpdConfig   3 /* y2006b */
  #define ftroConfig   1 /* y2006b */
  #define ftpcConfig   2 /* y2006b */
  #define hallConfig   2 /* y2006b */
  #define ibemConfig   1 /* y2006b */
  #define magpConfig   1 /* y2006b */
  #define mutdConfig   1 /* y2006b */
  #define phmdConfig   1 /* y2006b */
  #define pipeConfig   1 /* y2006b */
  #define sconConfig   3 /* y2006b */
  #define ssdConfig    8 /* y2006b */
  #define supportConfig   1 /* y2006b */
  #define svtConfig   13  /* 9 => 13  y2006b */
  #define tpcConfig   3 /* y2006b */
  #define upstreamConfig   1 /* y2006b */
  #define vpdConfig   1 /* y2006b */
  #define zcalConfig   1
#endif /* y2006b */
#ifdef y2006a
/*************************************************************************************************************
  on Y2006A   { Year 2006 baseline which is Y2005D+fixed TPC backplane+New SSD
                  "svt: 3 layers ";
                     nSi=6  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=4 " call btofgeo4 ";
* NEW CONFIG!
                     BtofConfig=8;

* Full barrel in 2006
                  "CALB" 
                     ems=on ;
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides" 


                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 2 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 6; "SVTT version"
                     SvshConfig = 2; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 35; "third version, full barrel newly corrected radii"


                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 3;
                  "Muon Trigger System"
                     MUTD = on;
                     MutdConfig = 1;
                  "We need a bigger Cave"
                     CaveConfig = 3;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* y2006a */
  #define btofConfig   5 /* y2006a */
  #define calbConfig   3 /* y2006a */
  #define caveConfig   2 /* y2006a */
  #define ecalConfig   1 /* y2006a */
  #define fpdConfig   3 /* y2006a */
  #define ftroConfig   1 /* y2006a */
  #define ftpcConfig   2 /* y2006a */
  #define hallConfig   2 /* y2006a */
  #define ibemConfig   1 /* y2006a */
  #define magpConfig   1 /* y2006a */
  #define mutdConfig   1 /* y2006a */
  #define phmdConfig   1 /* y2006a */
  #define pipeConfig   1 /* y2006a */
  #define sconConfig   3 /* y2006a */
  #define ssdConfig    8 /* 7 => 8 y2006a */
  #define supportConfig   1 /* y2006a */
  #define svtConfig   13  /* 9 => 13  y2006a */
  #define tpcConfig   3 /* y2006a */
  #define upstreamConfig   1 /* y2006a */
  #define vpdConfig   1 /* y2006a */
  #define zcalConfig   1 /* y2006a */
#endif /* y2006a */
#ifdef y2006
/*************************************************************************************************************
  on Y2006    { Year 2006 baseline which is Y2005D+fixed TPC backplane+New SSD
                  "svt: 3 layers ";
                     nSi=6  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=4 " call btofgeo4 ";
* NEW CONFIG!
                     BtofConfig=8;

* Full barrel in 2006
                  "CALB" 
                     ems=on ;
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides" 


                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 6; "SVTT version"
                     SvshConfig = 2; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 35; "third version, full barrel newly corrected radii"


                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 3;
                  "Muon Trigger System"
                     MUTD = on;
                     MutdConfig = 1;
                  "We need a bigger Cave"
                     CaveConfig = 3;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* y2006 */
  #define btofConfig   5 /* y2006 */
  #define calbConfig   3 /* y2006 */
  #define caveConfig   2 /* y2006 */
  #define ecalConfig   1 /* y2006 */
  #define fpdConfig   2 /* y2006 */
  #define ftroConfig   1 /* y2006 */
  #define ftpcConfig   2 /* y2006 */
  #define hallConfig   2 /* y2006 */
  #define ibemConfig   1 /* y2006 */
  #define magpConfig   1 /* y2006 */
  #define mutdConfig   1 /* y2006 */
  #define phmdConfig   1 /* y2006 */
  #define pipeConfig   1 /* y2006 */
  #define sconConfig   3 /* y2006 */
  #define ssdConfig    8 /* 7 => 8 y2006 */
  #define supportConfig   1 /* y2006 */
  #define svtConfig   13  /* 9 => 13  y2006 */
  #define tpcConfig   3 /* y2006 */
  #define upstreamConfig   1 /* y2006 */
  #define vpdConfig   1 /* y2006 */
  #define zcalConfig   1 /* y2006 */
#endif /* y2006 */
#ifdef y2007a
/*************************************************************************************************************
  on Y2007A    { Year 2007 (see below) but with corrected SVT (carbon instead of Be water channels)
                  "svt: 3 layers ";
                     nSi=7  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=5 " call btofgeo5 ";
* NEW CONFIG!
                     BtofConfig=10;

* Full barrel in 2007
                  "CALB" 
                     ems=on ;
* important:
                     CalbConfig = 2
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides" 
                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 3 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=7;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1;  "FTPC Support"
                     SvttConfig = 10; "SVTT version"
                     SvshConfig = 2;  "SVT shield"
                     DensConfig = 1;  "gas density correction"
                     FtpcConfig = 1;  "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 55; "fifth version, corrected radii, gaps, dead material"


                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 3;
                  "Muon Trigger System"
                     MUTD = on;
                     MutdConfig = 1;
                  "We need an even bigger Cave"
                     CaveConfig = 4;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* y2007a */
  #define btofConfig   6 /* y2007a */
  #define calbConfig   7 /* y2007a */
  #define caveConfig   3 /* y2007a */
  #define ecalConfig   1 /* y2007a */
  #define fpdConfig   4 /* y2007a */
  #define ftroConfig   1 /* y2007a */
  #define ftpcConfig   2 /* y2007a */
  #define hallConfig   3 /* y2007a */
  #define ibemConfig   2 /* was 3  y2007a */
  #define magpConfig   1 /* y2007a */
  #define mutdConfig   1 /* y2007a */
  #define phmdConfig   1 /* y2007a */
  #define pipeConfig   1 /* y2007a */
  #define sconConfig   3 /* y2007a */
  #define ssdConfig    8 /* y2007a */
  #define supportConfig   1 /* y2007a */
  #define svtConfig   13 /*12 => 13 y2007a */
  #define tpcConfig   3 /* y2007a */
  #define upstreamConfig   1 /* y2007a */
  #define vpdConfig   2 /* y2007a */
  #define zcalConfig   1 /* y2007a */
#endif /* y2007a */
#ifdef y2007g
/*************************************************************************************************************
  on Y2007G    { Year 2007A + dead material from Rene
                  "svt: 3 layers ";
                     nSi=7  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=5 " call btofgeo5 ";
* NEW CONFIG!
                     BtofConfig=10;

* Full barrel in 2007
                  "CALB" 
                     ems=on ;
* important:
                     CalbConfig = 2
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides" 
                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 3 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=7;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1;  "FTPC Support"
                     SvttConfig = 11; "SVTT version"
                     SvshConfig = 2;  "SVT shield"
                     DensConfig = 1;  "gas density correction"
                     FtpcConfig = 1;  "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 55; "fifth version, corrected radii, gaps, dead material"


                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 3;
                  "Muon Trigger System"
                     MUTD = on;
                     MutdConfig = 1;
                  "We need an even bigger Cave"
                     CaveConfig = 4;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* y2007g */
  #define btofConfig   6 /* y2007g */
  #define calbConfig   7 /* y2007g */
  #define caveConfig   3 /* y2007g */
  #define ecalConfig   1 /* y2007g */
  #define fpdConfig   4 /* y2007g */
  #define ftroConfig   1 /* y2007g */
  #define ftpcConfig   2 /* y2007g */
  #define hallConfig   3 /* y2007g */
  #define ibemConfig   2 /* was 3  y2007g */
  #define magpConfig   1 /* y2007g */
  #define mutdConfig   1 /* y2007g */
  #define phmdConfig   1 /* y2007g */
  #define pipeConfig   1 /* y2007g */
  #define sconConfig   3 /* y2007g */
  #define ssdConfig    8 /* y2007g */
  #define supportConfig   1 /* y2007g */
  #define svtConfig   13 /* y2007g */
  #define tpcConfig   3 /* y2007g */
  #define upstreamConfig   1 /* y2007g */
  #define vpdConfig   2 /* y2007g */
  #define zcalConfig   1 /* y2007g */
#endif /* y2007g */
#ifdef y2007
/*************************************************************************************************************
  on Y2007    { Year 2006 baseline which is Y2006+FMS
                  "svt: 3 layers ";
                     nSi=7  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=5 " call btofgeo5 ";
* NEW CONFIG!
                     BtofConfig=10;

* Full barrel in 2007
                  "CALB" 
                     ems=on ;
* important:
                     CalbConfig = 2
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides" 
                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 3 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=7;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 6; "SVTT version"
                     SvshConfig = 2; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 55; "fifth version, corrected radii, gaps, dead material"


                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 3;
                  "Muon Trigger System"
                     MUTD = on;
                     MutdConfig = 1;
                  "We need an even bigger Cave"
                     CaveConfig = 4;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* y2007 */
  #define btofConfig   6 /* y2007 */
  #define calbConfig   7 /* y2007 */
  #define caveConfig   3 /* y2007 */
  #define ecalConfig   1 /* y2007 */
  #define fpdConfig   4 /* y2007 */
  #define ftroConfig   1 /* y2007 */
  #define ftpcConfig   2 /* y2007 */
  #define hallConfig   3 /* y2007 */
  #define ibemConfig   2 /* was 3  y2007 */
  #define magpConfig   1 /* y2007 */
  #define mutdConfig   1 /* y2007 */
  #define phmdConfig   1 /* y2007 */
  #define pipeConfig   1 /* y2007 */
  #define sconConfig   3 /* y2007 */
  #define ssdConfig    8 /* y2007 */
  #define supportConfig   1 /* y2007 */
  #define svtConfig   13 /*11 => 13 y2007 */
  #define tpcConfig   3 /* y2007 */
  #define upstreamConfig   1 /* y2007 */
  #define vpdConfig   2 /* y2007 */
  #define zcalConfig   1 /* y2007 */
#endif /* y2007 */
#ifdef y2008
/*************************************************************************************************************
  on Y2008    { Year 2008 baseline: no SVT,  cones,beam support,FTPC in CAVE now
                  

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
* X.Dong
                     Itof=6 " call btofgeo6 ";
* NEW CONFIG!
                     tofX0= 0.00;
                     tofZ0=-0.50;
                     BtofConfig=11;

* Full barrel in 2007
                  "CALB" 
                     ems=on ;
* important:
                     CalbConfig = 2
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides" 
                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 3 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=7;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; 		"FTPC Support"
                     SvttConfig = -1; SVTT=off; "SVTT version"
                     SvshConfig = 2; 		"SVT shield"
                     DensConfig = 1; 		"gas density correction"
                     FtpcConfig = 1; 		"ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     SISD=off;
                     SisdConfig = -1; "fifth version, corrected radii, gaps, dead material"


                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 3;
                  "Muon Trigger System"
                     MUTD = on;
                     MutdConfig = 3;
                  "We need an even bigger Cave"
                     CaveConfig = 4;
                   PipeFlag = 1;  "pipe wrap only"
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* y2008 */
  #define btofConfig   7 /* y2008 */
  #define calbConfig   7 /* y2008 */
  #define caveConfig   3 /* y2008 */
  #define ecalConfig   1 /* y2008 */
  #define fpdConfig   4 /* y2008 */
  #define ftroConfig   1 /* y2008 */
  #define ftpcConfig   3 /* y2008 */
  #define hallConfig   3 /* y2008 */
  #define ibemConfig   1 /* was 3 */ /* y2008 */
  #define magpConfig   1 /* y2008 */
  #define mutdConfig   2 /* y2008 */
  #define phmdConfig   1 /* y2008 */
  #define pipeConfig   1 /* was 2 */ /* y2008 */
  #define supportConfig   1 /* y2008 */
  #define tpcConfig   3 /* y2008 */
  #define upstreamConfig   1 /* y2008 */
  #define vpdConfig   2 /* y2008 */
  #define zcalConfig   1 /* y2008 */
#endif /* y2008 */ 
#ifdef upgr01
/*************************************************************************************************************
  on UPGR01   { R and D geometry: TPC+SSD+HFT-SVT

                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "CALB" 
                     ems=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     ecal_config=1   " west wheel "
                     ecal_fill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 45;
* careful! Achtung!
                   PipeConfig=4;   " provisional"
                   pipeFlag=0; ! No wrap no svt shield
                   PIXL=on;        " put the pixel detector in"
                   PixlConfig=4;   " newest version by Andrew Rose"
* HPD, IST off
* Forward STAR tracker disk
                   FSTD=on;  "new pixel based forward tracker"
                   FstdConfig=2;
* Forward STAR tracker disk
                   FGTD=off;  "GEM forward tracker"
                   FgtdConfig=0;
* Forward GEM disks in this tag
                   IGTD=on;
* prototype of the Inner Tracker SuPport structure
                   ITSP=on;
                }
*************************************************************************************************************/
  #define bbcConfig        1 /* upgr01 */
  #define btofConfig   	   3 /* upgr01 */
  #define calbConfig   	   3 /* upgr01 */
  #define caveConfig   	   1 /* upgr01 */
  #define ecalConfig   	   1 /* upgr01 */
  #define fpdConfig    	   1 /* upgr01 */
  #define fstdConfig   	   1 /* upgr01 */
  //  #define ftroConfig   	   2 /* upgr01 */
  #define hallConfig   	   1 /* upgr01 */
  #define ibemConfig   	   1 /* upgr01 */
  #define igtdConfig   	   1 /* upgr01 */
  #define magpConfig   	   1 /* upgr01 */
  #define phmdConfig   	   1 /* upgr01 */
  #define pipeConfig   	   3 /* upgr01 */
  #define pixelConfig  	   2 /* upgr01 */
  #define itspConfig   	   1 /* upgr01 */
  #define ssdConfig    	   8 /* 9 => 8 upgr01 */
  #define tpcConfig    	   1 /* upgr01 */
  #define upstreamConfig   1 /* upgr01 */
  #define vpdConfig        1 /* upgr01 */
  #define zcalConfig       1 /* upgr01 */
#endif /* upgr01 */
#ifdef upgr02
/*************************************************************************************************************
  on UPGR02    { R and D geometry: TPC+IST+HFT-SVT
                  "svt: 3 layers ";
                     nSi=6  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=4 " call btofgeo4 ";
* NEW CONFIG!
                     BtofConfig=8;

                  "CALB" 
                     ems=on
                     CalbConfig = 1
* remember that with this config, the following parameters have
* a different meaning because we have to (unfortunately) switch
* from divisions to copies and introduce a map, which DOES
* control the configuration
                     nmod={60,60}; shift={75,105}; " 60 sectors West plus 30 East split between 2 halves"

                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 1 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=4;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 5; "SVTT version"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     SISD=off;
                     SisdConfig = 0; "no ssd here"


                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

* Inner STAR tracker barrel
                   ISTB=on;  "new pixel based inner tracker"
                   IstbConfig=1;

* careful! Achtung!
                   PipeConfig=5;   " thinner pipe "
                   pipeFlag=0; ! No wrap no svt shield
                   PIXL=on;        " activate "
                   PixlConfig=3;   " source version "

                  "New version of the TPC backplane "
                     TpceConfig = 3;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* upgr02 */
  #define btofConfig   5 /* upgr02 */
  #define calbConfig   6 /* upgr02 */
  #define caveConfig   1 /* upgr02 */
  #define ecalConfig   1 /* upgr02 */
  #define fpdConfig   2 /* upgr02 */
  #define ftroConfig   1 /* upgr02 */
  #define ftpcConfig   2 /* upgr02 */
  #define hallConfig   1 /* upgr02 */
  #define ibemConfig   1 /* upgr02 */
  #define istConfig   1 /* upgr02 */
  #define magpConfig   1 /* upgr02 */
  #define phmdConfig   1 /* upgr02 */
  #define pipeConfig   4 /* upgr02 */
  #define pixelConfig   3 /* upgr02 */
  #define sconConfig   5 /* upgr02 */
  #define supportConfig   1 /* upgr02 */
  #define svtConfig   16 /* upgr02 */
  #define tpcConfig   3 /* upgr02 */
  #define upstreamConfig   1 /* upgr02 */
  #define vpdConfig   1 /* upgr02 */
  #define zcalConfig   1 /* upgr02 */
#endif /* upgr02 */
#ifdef upgr03
/*************************************************************************************************************
  on UPGR03   { New Tracking: IST+IGT+HFT-SVT

                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "CALB" 
                     ems=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     ecal_config=1   " west wheel "
                     ecal_fill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "
*                    -- Obsoleted: CorrNum = 4;
                     SvshConfig = 1; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 4;

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 23;
* careful! Achtung!
                   PipeConfig=4;   " provisional"
                   pipeFlag=0; ! No wrap no svt shield
                   PIXL=on;        " put the pixel detector in"
                   PixlConfig=2;   " newer version decoupled from SVT"
* Inner STAR tracker barrel
                   ISTB=on;  "new pixel based inner tracker"
                   IstbConfig=1;
* Inner STAR GEM barrel
                   GEMB=off;  
                   GembConfig=0;
* Forward STAR tracker disk
                   FSTD=on;  "new pixel based forward tracker"
                   FstdConfig=1;
* Forward STAR tracker disk
                   FGTD=off;  "GEM forward tracker"
                   FgtdConfig=0;
* the forward GEM disks
                   IGTD=on;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* upgr03 */
  #define btofConfig   3 /* upgr03 */
  #define calbConfig   3 /* upgr03 */
  #define caveConfig   1 /* upgr03 */
  #define ecalConfig   1 /* upgr03 */
  #define fpdConfig   1 /* upgr03 */
  #define fstdConfig   1 /* upgr03 */
  //  #define ftroConfig   3 /* upgr03 */
  #define hallConfig   1 /* upgr03 */
  #define ibemConfig   1 /* upgr03 */
  #define igtdConfig   1 /* upgr03 */
  #define istConfig   1 /* upgr03 */
  #define magpConfig   1 /* upgr03 */
  #define phmdConfig   1 /* upgr03 */
  #define pipeConfig   3 /* upgr03 */
  #define pixelConfig   4 /* upgr03 */
  #define ssdConfig    8 /* 10 => 8 upgr03 */
  #define tpcConfig   1 /* upgr03 */
  #define upstreamConfig   1 /* upgr03 */
  #define vpdConfig   1 /* upgr03 */
  #define zcalConfig   1 /* upgr03 */
#endif /* upgr03 */
#ifdef upgr04
/*************************************************************************************************************
  on UPGR04   { New Tracking: HPD

                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "CALB" 
                     ems=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     ecal_config=1   " west wheel "
                     ecal_fill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "
*                    -- Obsoleted: CorrNum = 4;
                     SvshConfig = 1; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 4;

                  "Photon Multiplicity Detector Version "
                     PHMD=off;
                     PhmdConfig = 0;
                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 35;
* careful! Achtung!
                   PipeConfig=4;   " provisional"
                   pipeFlag=0; ! No wrap no svt shield
                   HPDT=on;        " put the detector in"
                   HpdtConfig=1;   " base version"
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* upgr04 */
  #define btofConfig   3 /* upgr04 */
  #define calbConfig   3 /* upgr04 */
  #define caveConfig   1 /* upgr04 */
  #define ecalConfig   1 /* upgr04 */
  #define fpdConfig   1 /* upgr04 */
  #define hallConfig   1 /* upgr04 */
  #define hpdtConfig   1 /* upgr04 */
  #define ibemConfig   1 /* upgr04 */
  #define magpConfig   1 /* upgr04 */
  #define pipeConfig   3 /* upgr04 */
  #define ssdConfig    8 /* 11 => 8 upgr04 */
  #define tpcConfig   1 /* upgr04 */
  #define upstreamConfig   1 /* upgr04 */
  #define vpdConfig   1 /* upgr04 */
  #define zcalConfig   1 /* upgr04 */
#endif /* upgr04 */
#ifdef upgr05
/*************************************************************************************************************
  on UPGR05   { New Tracking: HFT+HPD+IST+TPC-SVT

                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "CALB" 
                     ems=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     ecal_config=1   " west wheel "
                     ecal_fill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 45;
* careful! Achtung!
                   PipeConfig=4;   " provisional"
                   pipeFlag=0; ! No wrap no svt shield
                   PIXL=on;        " put the pixel detector in"
                   PixlConfig=4;   " newest version by Andrew Rose"

                   HPDT=on;        " put the Hybrid Pixel detector in"
                   HpdtConfig=1;   " base version"
* Inner STAR tracker barrel
                   ISTB=on;  "new pixel based inner tracker"
                   IstbConfig=3;
* Inner STAR GEM barrel
                   GEMB=off;  
                   GembConfig=0;
* Forward STAR tracker disk
                   FSTD=on;  "new pixel based forward tracker"
                   FstdConfig=2;
* Forward STAR tracker disk
                   FGTD=off;  "GEM forward tracker"
                   FgtdConfig=0;
* Forward GEM disks in this tag
                   IGTD=on;
* prototype of the Inner Tracker SuPport structure
                   ITSP=on;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* upgr05 */
  #define btofConfig   3 /* upgr05 */
  #define calbConfig   3 /* upgr05 */
  #define caveConfig   1 /* upgr05 */
  #define ecalConfig   1 /* upgr05 */
  #define fpdConfig   1 /* upgr05 */
  //  #define fstdConfig   1 /* upgr05 */
  //  #define ftroConfig   2 /* upgr05 */
  #define hallConfig   1 /* upgr05 */
  #define hpdtConfig   1 /* upgr05 */
  #define ibemConfig   1 /* upgr05 */
  #define igtdConfig   1 /* upgr05 */
  #define istConfig   2 /* upgr05 */
  #define magpConfig   1 /* upgr05 */
  #define phmdConfig   1 /* upgr05 */
  #define pipeConfig   3 /* upgr05 */
  #define pixelConfig   2 /* upgr05 */
  #define itspConfig   	   1 /* upgr05 */
  #define ssdConfig    8 /* 9 => 8  upgr05 */
  #define tpcConfig   1 /* upgr05 */
  #define upstreamConfig   1 /* upgr05 */
  #define vpdConfig   1 /* upgr05 */
  #define zcalConfig   1 /* upgr05 */
#endif /* upgr05 */
#ifdef upgr06
/*************************************************************************************************************
  on UPGR06   { New Tracking: HFT+HPD+SSD

                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "CALB" 
                     ems=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     ecal_config=1   " west wheel "
                     ecal_fill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 45;
* careful! Achtung!
                   PipeConfig=4;   " provisional"
                   pipeFlag=0; ! No wrap no svt shield
                   PIXL=on;        " put the pixel detector in"
                   PixlConfig=4;   " newest version by Andrew Rose"

                   HPDT=on;        " put the Hybrid Pixel detector in"
                   HpdtConfig=1;   " base version"
* Forward STAR tracker disk
                   FSTD=on;  "new pixel based forward tracker"
                   FstdConfig=2;
* Forward STAR tracker disk
                   FGTD=off;  "GEM forward tracker"
                   FgtdConfig=0;
* Forward GEM disks in this tag
                   IGTD=on;
* prototype of the Inner Tracker SuPport structure
                   ITSP=on;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* upgr06 */
  #define btofConfig   3 /* upgr06 */
  #define calbConfig   3 /* upgr06 */
  #define caveConfig   1 /* upgr06 */
  #define ecalConfig   1 /* upgr06 */
  #define fpdConfig   1 /* upgr06 */
  #define fstdConfig   1 /* upgr06 */
  //  #define ftroConfig   2 /* upgr06 */
  #define hallConfig   1 /* upgr06 */
  #define hpdtConfig   1 /* upgr06 */
  #define ibemConfig   1 /* upgr06 */
  #define igtdConfig   1 /* upgr06 */
  #define magpConfig   1 /* upgr06 */
  #define phmdConfig   1 /* upgr06 */
  #define pipeConfig   3 /* upgr06 */
  #define pixelConfig   2 /* upgr06 */
  #define itspConfig   	   1 /* upgr06 */
  #define ssdConfig    8 /* 9 => 8  upgr06 */
  #define tpcConfig   1 /* upgr06 */
  #define upstreamConfig   1 /* upgr06 */
  #define vpdConfig   1 /* upgr06 */
  #define zcalConfig   1 /* upgr06 */
#endif /* upgr06 */
#ifdef upgr07
/*************************************************************************************************************
  on UPGR07   { New Tracking: HFT+IST+TPC+SSD-SVT

                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "CALB" 
                     ems=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     ecal_config=1   " west wheel "
                     ecal_fill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 45;
* careful! Achtung!
                   PipeConfig=4;   " provisional"
                   pipeFlag=0; ! No wrap no svt shield
                   PIXL=on;        " put the pixel detector in"
                   PixlConfig=4;   " newest version by Andrew Rose"

                   ISTB=on;  "IST barrel"
                   IstbConfig=3;

                   FSTD=on;  "new pixel based forward tracker disk"
                   FstdConfig=2;

                   IGTD=on; "Forward GEM disks in this tag"
                   ITSP=on; "prototype of the Inner Tracker SuPport structure"
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* upgr07 */
  #define btofConfig   3 /* upgr07 */
  #define calbConfig   3 /* upgr07 */
  #define caveConfig   1 /* upgr07 */
  #define ecalConfig   1 /* upgr07 */
  #define fpdConfig   1 /* upgr07 */
  #define fstdConfig   1 /* upgr07 */
  //  #define ftroConfig   2 /* upgr07 */
  #define hallConfig   1 /* upgr07 */
  #define ibemConfig   1 /* upgr07 */
  #define igtdConfig   1 /* upgr07 */
  #define istConfig   2 /* upgr07 */
  #define magpConfig   1 /* upgr07 */
  #define phmdConfig   1 /* upgr07 */
  #define pipeConfig   3 /* upgr07 */
  #define pixelConfig   2 /* upgr07 */
  #define itspConfig   	   1 /* upgr07 */
  #define ssdConfig    8 /* 9 => 8  upgr07 */
  #define tpcConfig   1 /* upgr07 */
  #define upstreamConfig   1 /* upgr07 */
  #define vpdConfig   1 /* upgr07 */
  #define zcalConfig   1 /* upgr07 */
#endif /* upgr07 */
#ifdef upgr08
/*************************************************************************************************************
  on UPGR08   { New Tracking: HFT+HPD+IST+TPC-SVT-SSD

                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "CALB" 
                     ems=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     ecal_config=1   " west wheel "
                     ecal_fill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     SISD=off;
                     SisdConfig = 0;
* careful! Achtung!
                   PipeConfig=4;   " provisional"
                   pipeFlag=0; ! No wrap no svt shield
                   PIXL=on;        " put the pixel detector in"
                   PixlConfig=4;   " newest version by Andrew Rose"

                   HPDT=on;        " put the Hybrid Pixel detector in"
                   HpdtConfig=1;   " base version"
* Inner STAR tracker barrel
                   ISTB=on;  "new pixel based inner tracker"
                   IstbConfig=3;
* Inner STAR GEM barrel
                   GEMB=off;  
                   GembConfig=0;
* Forward STAR tracker disk
                   FSTD=on;  "new pixel based forward tracker"
                   FstdConfig=2;
* Forward STAR tracker disk
                   FGTD=off;  "GEM forward tracker"
                   FgtdConfig=0;
* Forward GEM disks in this tag
                   IGTD=on;
* prototype of the Inner Tracker SuPport structure
                   ITSP=on;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* upgr08 */
  #define btofConfig   3 /* upgr08 */
  #define calbConfig   3 /* upgr08 */
  #define caveConfig   1 /* upgr08 */
  #define ecalConfig   1 /* upgr08 */
  #define fpdConfig   1 /* upgr08 */
  #define fstdConfig   1 /* upgr08 */
  //  #define ftroConfig   2 /* upgr08 */
  #define hallConfig   1 /* upgr08 */
  #define hpdtConfig   1 /* upgr08 */
  #define ibemConfig   1 /* upgr08 */
  #define igtdConfig   1 /* upgr08 */
  #define istConfig   2 /* upgr08 */
  #define magpConfig   1 /* upgr08 */
  #define phmdConfig   1 /* upgr08 */
  #define pipeConfig   3 /* upgr08 */
  #define pixelConfig   2 /* upgr08 */
  #define itspConfig   	   1 /* upgr08 */
  #define tpcConfig   1 /* upgr08 */
  #define upstreamConfig   1 /* upgr08 */
  #define vpdConfig   1 /* upgr08 */
  #define zcalConfig   1 /* upgr08 */
#endif /* upgr08 */
#ifdef upgr09
/*************************************************************************************************************
  on UPGR09   { New Tracking: HFT+HPD+IST*outer+TPC-SVT-SSD

                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "CALB" 
                     ems=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     ecal_config=1   " west wheel "
                     ecal_fill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     SISD=off;
                     SisdConfig = 0;
* careful! Achtung!
                   PipeConfig=4;   " provisional"
                   PIXL=on;        " put the pixel detector in"
                   pipeFlag=0; ! No wrap no svt shield
                   PixlConfig=4;   " newest version by Andrew Rose"

                   HPDT=on;        " put the Hybrid Pixel detector in"
                   HpdtConfig=1;   " base version"
* Inner STAR tracker barrel
                   ISTB=on;  "new pixel based inner tracker"
                   IstbConfig=4;
* Inner STAR GEM barrel
                   GEMB=off;  
                   GembConfig=0;
* Forward STAR tracker disk
                   FSTD=on;  "new pixel based forward tracker"
                   FstdConfig=2;
* Forward STAR tracker disk
                   FGTD=off;  "GEM forward tracker"
                   FgtdConfig=0;
* Forward GEM disks in this tag
                   IGTD=on;
* prototype of the Inner Tracker SuPport structure
                   ITSP=on;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* upgr09 */
  #define btofConfig   3 /* upgr09 */
  #define calbConfig   3 /* upgr09 */
  #define caveConfig   1 /* upgr09 */
  #define ecalConfig   1 /* upgr09 */
  #define fpdConfig   1 /* upgr09 */
  #define fstdConfig   1 /* upgr09 */
  //  #define ftroConfig   2 /* upgr09 */
  #define hallConfig   1 /* upgr09 */
  #define hpdtConfig   1 /* upgr09 */
  #define ibemConfig   1 /* upgr09 */
  #define igtdConfig   1 /* upgr09 */
  #define istConfig   3 /* upgr09 */
  #define magpConfig   1 /* upgr09 */
  #define phmdConfig   1 /* upgr09 */
  #define pipeConfig   3 /* upgr09 */
  #define pixelConfig   2 /* upgr09 */
  #define itspConfig   	   1 /* upgr09 */
  #define tpcConfig   1 /* upgr09 */
  #define upstreamConfig   1 /* upgr09 */
  #define vpdConfig   1 /* upgr09 */
  #define zcalConfig   1 /* upgr09 */
#endif /* upgr09 */
#ifdef upgr10
/*************************************************************************************************************
  on UPGR10   { New Tracking: HFT+innerLayerIST+TPC-SVT+SSD

                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "CALB" 
                     ems=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     ecal_config=1   " west wheel "
                     ecal_fill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     SISD=off;
                     SisdConfig = 0;
* careful! Achtung!
                   PipeConfig=4;   " provisional"
                   pipeFlag=0; ! No wrap no svt shield
                   PIXL=on;        " put the pixel detector in"
                   PixlConfig=4;   " newest version by Andrew Rose"

                  "Silicon Strip Detector Version "
                   SISD=on;
                   SisdConfig = 45;
* Inner STAR tracker barrel
                   ISTB=on;  "new pixel based inner tracker"
                   IstbConfig=5;
* Inner STAR GEM barrel
                   GEMB=off;  
                   GembConfig=0;
* Forward STAR tracker disk
                   FSTD=on;  "new pixel based forward tracker"
                   FstdConfig=2;
* Forward STAR tracker disk
                   FGTD=off;  "GEM forward tracker"
                   FgtdConfig=0;
* Forward GEM disks in this tag
                   IGTD=on;
* prototype of the Inner Tracker SuPport structure
                   ITSP=on;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* upgr10 */
  #define btofConfig   3 /* upgr10 */
  #define calbConfig   3 /* upgr10 */
  #define caveConfig   1 /* upgr10 */
  #define ecalConfig   1 /* upgr10 */
  #define fpdConfig   1 /* upgr10 */
  #define fstdConfig   1 /* upgr10 */
  //  #define ftroConfig   2 /* upgr10 */
  #define hallConfig   1 /* upgr10 */
  #define ibemConfig   1 /* upgr10 */
  #define igtdConfig   1 /* upgr10 */
  #define istConfig   4 /* upgr10 */
  #define magpConfig   1 /* upgr10 */
  #define phmdConfig   1 /* upgr10 */
  #define pipeConfig   3 /* upgr10 */
  #define pixelConfig   2 /* upgr10 */
  #define itspConfig   	   1 /* upgr10 */
  #define ssdConfig    8 /* 9 => 8  upgr10 */
  #define tpcConfig   1 /* upgr10 */
  #define upstreamConfig   1 /* upgr10 */
  #define vpdConfig   1 /* upgr10 */
  #define zcalConfig   1 /* upgr10 */
#endif /* upgr10 */
#ifdef upgr11
/*************************************************************************************************************
  on UPGR11   { New Tracking: HFT+2LayerIST+TPC-SVT+SSD

                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "CALB" 
                     ems=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     ecal_config=1   " west wheel "
                     ecal_fill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;
* careful! Achtung!
                   PipeConfig=4;   " provisional"
                   pipeFlag=0; ! No wrap no svt shield
                   PIXL=on;        " put the pixel detector in"
                   PixlConfig=4;   " newest version by Andrew Rose"

                  "Silicon Strip Detector Version "
                   SISD=on;
                   SisdConfig = 45;
* Inner STAR tracker barrel
                   ISTB=on;  "new pixel based inner tracker"
                   IstbConfig=6;
* Inner STAR GEM barrel
                   GEMB=off;  
                   GembConfig=0;
* Forward STAR tracker disk
                   FSTD=on;  "new pixel based forward tracker"
                   FstdConfig=2;
* Forward STAR tracker disk
                   FGTD=off;  "GEM forward tracker"
                   FgtdConfig=0;
* Forward GEM disks in this tag
                   IGTD=on;
* prototype of the Inner Tracker SuPport structure
                   ITSP=on;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* upgr11 */
  #define btofConfig   3 /* upgr11 */
  #define calbConfig   3 /* upgr11 */
  #define caveConfig   1 /* upgr11 */
  #define ecalConfig   1 /* upgr11 */
  #define fpdConfig   1 /* upgr11 */
  #define fstdConfig   1 /* upgr11 */
  //  #define ftroConfig   2 /* upgr11 */
  #define hallConfig   1 /* upgr11 */
  #define ibemConfig   1 /* upgr11 */
  #define igtdConfig   1 /* upgr11 */
  #define istConfig   5 /* upgr11 */
  #define magpConfig   1 /* upgr11 */
  #define phmdConfig   1 /* upgr11 */
  #define pipeConfig   3 /* upgr11 */
  #define pixelConfig   2 /* upgr11 */
  #define itspConfig   	   1 /* upgr11 */
  #define ssdConfig    8 /* 9 => 8  upgr11 */
  #define tpcConfig   1 /* upgr11 */
  #define upstreamConfig   1 /* upgr11 */
  #define vpdConfig   1 /* upgr11 */
  #define zcalConfig   1 /* upgr11 */
#endif /* upgr11 */
#ifdef upgr12
/*************************************************************************************************************
  on UPGR12   { New Tracking: HFT+HPD+IST+TPC+IGT*newRadii

                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "CALB" 
                     ems=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     ecal_config=1   " west wheel "
                     ecal_fill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 45;
* careful! Achtung!
                   PipeConfig=4;   " provisional"
                   pipeFlag=0; ! No wrap no svt shield
                   PIXL=on;        " put the pixel detector in"
                   PixlConfig=4;   " newest version by Andrew Rose"

                   HPDT=on;        " put the Hybrid Pixel detector in"
                   HpdtConfig=1;   " base version"
* Inner STAR tracker barrel
                   ISTB=on;  "new pixel based inner tracker"
                   IstbConfig=3;
* Inner STAR GEM barrel
                   GEMB=off;  
                   GembConfig=0;
* Forward STAR tracker disk
                   FSTD=on;  "new pixel based forward tracker"
                   FstdConfig=2;
* Forward STAR tracker disk
                   FGTD=off;  "GEM forward tracker"
                   FgtdConfig=0;
* Forward GEM disks in this tag
                   IGTD=on;
                   IgtdConfig=2;
* prototype of the Inner Tracker SuPport structure
                   ITSP=on;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* upgr12 */
  #define btofConfig   3 /* upgr12 */
  #define calbConfig   3 /* upgr12 */
  #define caveConfig   1 /* upgr12 */
  #define ecalConfig   1 /* upgr12 */
  #define fpdConfig   1 /* upgr12 */
  #define fstdConfig   1 /* upgr12 */
  //  #define ftroConfig   2 /* upgr12 */
  #define hallConfig   1 /* upgr12 */
  #define hpdtConfig   1 /* upgr12 */
  #define ibemConfig   1 /* upgr12 */
  #define igtdConfig   2 /* upgr12 */
  #define istConfig   2 /* upgr12 */
  #define magpConfig   1 /* upgr12 */
  #define phmdConfig   1 /* upgr12 */
  #define pipeConfig   3 /* upgr12 */
  #define pixelConfig   2 /* upgr12 */
  #define itspConfig   	   1 /* upgr12 */
  #define ssdConfig    8 /* 9 => 8  upgr12 */
  #define tpcConfig   1 /* upgr12 */
  #define upstreamConfig   1 /* upgr12 */
  #define vpdConfig   1 /* upgr12 */
  #define zcalConfig   1 /* upgr12 */
#endif /* upgr12 */
#ifdef upgr13
/*************************************************************************************************************
  on UPGR13   { New Tracking: HFT+IST+TPC+SSD-SVT
                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "tpc: standard, i.e.  "
                     mwc=on " Multiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "CALB" 
                     ems=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     ecal_config=1   " west wheel "
                     ecal_fill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 65;
* careful! Achtung!
                   PipeConfig=4;   " provisional"
                   pipeFlag=0; ! No wrap no svt shield
                   PIXL=on;        " put the pixel detector in"
                   PixlConfig=5;   " newest version, thicker active Si"

                   ISTB=on;  "IST barrel"
                   IstbConfig=7;

                   FSTD=off;  "no pixel based forward tracker in this tag"
                   FstdConfig=0;

* Forward STAR tracker disk
                   FGTD=on;  "GEM forward tracker"
                   FgtdConfig=2;
* On Gerrit's request, we disable the cone:
                   ITSP=off; "prototype of the Inner Tracker SuPport structure"
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* upgr13 */
  #define btofConfig   3 /* upgr13 */
  #define calbConfig   3 /* upgr13 */
  #define caveConfig   1 /* upgr13 */
  #define ecalConfig   1 /* upgr13 */
  #define fgtdConfig   1 /* upgr13 */
  #define fpdConfig   1 /* upgr13 */
  #define hallConfig   1 /* upgr13 */
  #define ibemConfig   1 /* upgr13 */
  #define istConfig   6 /* upgr13 */
  #define magpConfig   1 /* upgr13 */
  #define phmdConfig   1 /* upgr13 */
  #define pipeConfig   3 /* upgr13 */
  #define pixelConfig   5 /* upgr13 */
  #define ssdConfig   8 /* 12 => 8 upgr13 */
  #define tpcConfig   1 /* upgr13 */
  #define upstreamConfig   1 /* upgr13 */
  #define vpdConfig   1 /* upgr13 */
  #define zcalConfig   1 /* upgr13 */
#endif /* upgr13 */
#ifdef upgr14
/*************************************************************************************************************
  on UPGR14   { TUP sans IST: HFT+TPC+SSD-SVT
                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "tpc: standard, i.e.  "
                     mwc=on " Multiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "CALB" 
                     ems=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     ecal_config=1   " west wheel "
                     ecal_fill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 65;
* careful! Achtung!
                   PipeConfig=4;   " provisional"
                   pipeFlag=0; ! No wrap no svt shield
                   PIXL=on;        " put the pixel detector in"
                   PixlConfig=5;   " newest version, thicker active Si"

                   ISTB=off;       "no IST barrel"
                   IstbConfig=0;   "ditto"

                   FSTD=off;  "no pixel based forward tracker in this tag"
                   FstdConfig=0;

* Forward STAR tracker disk
                   FGTD=on;  "GEM forward tracker"
                   FgtdConfig=2;
* On Gerrit's request, we disable the cone:
                   ITSP=off; "prototype of the Inner Tracker SuPport structure"
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* upgr14 */
  #define btofConfig   3 /* upgr14 */
  #define calbConfig   3 /* upgr14 */
  #define caveConfig   1 /* upgr14 */
  #define ecalConfig   1 /* upgr14 */
  #define fgtdConfig   1 /* upgr14 */
  #define fpdConfig   1 /* upgr14 */
  #define hallConfig   1 /* upgr14 */
  #define ibemConfig   1 /* upgr14 */
  #define magpConfig   1 /* upgr14 */
  #define phmdConfig   1 /* upgr14 */
  #define pipeConfig   3 /* upgr14 */
  #define pixelConfig   6 /* upgr14 */
  #define ssdConfig    8 /* 12 => 8  upgr14 */
  #define tpcConfig   1 /* upgr14 */
  #define upstreamConfig   1 /* upgr14 */
  #define vpdConfig   1 /* upgr14 */
  #define zcalConfig   1 /* upgr14 */
#endif /* upgr14 */
#ifdef upgr15
/*************************************************************************************************************
*The reason for naming these three file xxxxgeo00.g is to indicate that these
*are the most simple geometries that can be made. As such they don't fit in
*with the existing naming scheme where the increasing numbers indicate a
*next step of evolution, better understanding of material, better design, etc.
*I explicitly wanted to break with this scheme to make it clear that these
*designs are just for testing purposes. Gerrit

  on UPGR15   { New Tracking: HFT+IST+TPC+SSD-SVT
                     SVTT=off; "no SVT  at all in this configuration"
                     ftpc=off; "no FTPC at all in this configuration"
                  "tpc: standard, i.e.  "
                     mwc=on " Multiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";
                  "ctb: central trigger barrer             ";
                     Itof=2 " call btofgeo2 ";
                     BtofConfig=5;
                  "CALB" 
                     ems=on
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides"
                  "ECAL"
                     ecal_config=1   " west wheel "
                     ecal_fill=3     " all sectors filled "
                  "beam-beam counter "
                     BBCM=on
                  "forward pion detector "
                     FPDM=on
                  "field version "
                     Mf=4;      "tabulated field, with correction "

                     SvshConfig = 0; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 0;

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 1;
                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 65;
* careful! Achtung!
                   PipeConfig=-1;   " Simplest.Gerrit"
                   pipeFlag=-1; !   " Simplest.Gerrit"
                   PIXL=on;        " put the pixel detector in"
                   PixlConfig=-1;   " Simplest.Gerrit"

                   ISTB=on;  "IST barrel"
                   IstbConfig=-1;

                   FSTD=off;  "no pixel based forward tracker in this tag"
                   FstdConfig=0;

* Forward STAR tracker disk
                   FGTD=on;  "GEM forward tracker"
                   FgtdConfig=2;
* On Gerrit's request, we disable the cone:
                   ITSP=off; "prototype of the Inner Tracker SuPport structure"
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* upgr15 */
  #define btofConfig   3 /* upgr15 */
  #define calbConfig   3 /* upgr15 */
  #define caveConfig   1 /* upgr15 */
  #define ecalConfig   1 /* upgr15 */
  #define fgtdConfig   1 /* upgr15 */
  #define fpdConfig   1 /* upgr15 */
  #define hallConfig   1 /* upgr15 */
  #define ibemConfig   1 /* upgr15 */
  #define istConfig   7 /* upgr15 */
  #define magpConfig   1 /* upgr15 */
  #define phmdConfig   1 /* upgr15 */
  #define pipeConfig   5 /* upgr15 */
  #define pixelConfig   7 /* upgr15 */
  #define ssdConfig    8 /* 12 => 8  upgr15 */
  #define tpcConfig   1 /* upgr15 */
  #define upstreamConfig   1 /* upgr15 */
  #define vpdConfig   1 /* upgr15 */
  #define zcalConfig   1 /* upgr15 */
#endif /* upgr15 */
#ifdef upgr21
/*************************************************************************************************************
  on UPGR21    { Year UPGR20 + full tof;
                     nSi=6  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=5 " call btofgeo5 ";
* NEW CONFIG!
                     BtofConfig=6;

* Full barrel in 2007
                  "CALB" 
                     ems=on ;
* important:
                     CalbConfig = 2
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides" 
                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 3 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=7;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 6; "SVTT version"
                     SvshConfig = 2; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 55; "fifth version, corrected radii, gaps, dead material"


                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 3;
                  "Muon Trigger System"
                     MUTD = on;
                     MutdConfig = 2;
                  "We need an even bigger Cave"
                     CaveConfig = 4;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* upgr21 */
  #define btofConfig   8 /* upgr21 */
  #define calbConfig   7 /* upgr21 */
  #define caveConfig   3 /* upgr21 */
  #define ecalConfig   1 /* upgr21 */
  #define fpdConfig   4 /* upgr21 */
  #define ftroConfig   1 /* upgr21 */
  #define ftpcConfig   2 /* upgr21 */
  #define hallConfig   3 /* upgr21 */
  #define ibemConfig   2 /* upgr21 */
  #define magpConfig   1 /* upgr21 */
  #define mutdConfig   3 /* upgr21 */
  #define phmdConfig   1 /* upgr21 */
  #define pipeConfig   1 /* upgr21 */
  #define sconConfig   3 /* upgr21 */
  #define ssdConfig    8 /* upgr21 */
  #define supportConfig   1 /* upgr21 */
  #define svtConfig   13  /* 9 => 13  upgr21 */
  #define tpcConfig   3 /* upgr21 */
  #define upstreamConfig   1 /* upgr21 */
  #define vpdConfig   2 /* upgr21 */
  #define zcalConfig   1 /* upgr21 */
#endif /* upgr21 */
#ifdef upgr20
/*************************************************************************************************************
  on UPGR20    { Year 2007 + one TOF
                  "svt: 3 layers ";
                     nSi=6  " 3 bi-plane layers, nSi<=7 ";
                     Wfr=0  " numbering is in the code   ";
                     Wdm=0  " width is in the code      ";

                     ConeConfig=2 " new cable weight estimate ";

                  "tpc: standard, i.e.  "
                     mwc=on " Wultiwire chambers are read-out ";
                     pse=on " inner sector has pseudo padrows ";

                  "ctb: central trigger barrer             ";
                     Itof=5 " call btofgeo5 ";
* NEW CONFIG!
                     BtofConfig=10;

* Full barrel in 2007
                  "CALB" 
                     ems=on ;
* important:
                     CalbConfig = 2
                     nmod={60,60}; shift={75,105}; " 60 sectors on both sides" 
                  "ECAL"
                     ecal_config=1   " one ECAL patch, west "
                     ecal_fill=3     " all sectors filled "

                  "beam-beam counter "
                     BBCM=on

                  "forward pion detector "
                     FPDM=on
                     FpdmConfig  = 3 "switch to a different lead glass source code"

                  "pseudo Vertex Position Detector"
                     VPDD=on;
                     VpddConfig=7;

                  "field version "
                     Mf=4;      "tabulated field, with correction "

* important: (1) new SVT version (2) FTPC gas correction tp Ar+C02 mix (3) SSD ladders raddi correction

                     SupoConfig = 1; "FTPC Support"
                     SvttConfig = 6; "SVTT version"
                     SvshConfig = 2; "SVT shield"
                     DensConfig = 1; "gas density correction"
                     FtpcConfig = 1; "ftpc configuration"

                  "Photon Multiplicity Detector Version "
                     PHMD=on;
                     PhmdConfig = 2;

                  "Silicon Strip Detector Version "
                     SISD=on;
                     SisdConfig = 55; "fifth version, corrected radii, gaps, dead material"


                  "FTPC Readout barrel "
                     FTRO=on;
                     FtroConfig = 1;

                  "New version of the TPC backplane "
                     TpceConfig = 3;
                  "Muon Trigger System"
                     MUTD = on;
                     MutdConfig = 2;
                  "We need an even bigger Cave"
                     CaveConfig = 4;
                }
*************************************************************************************************************/
  #define bbcConfig   1 /* upgr20 */
  #define btofConfig   6 /* upgr20 */
  #define calbConfig   7 /* upgr20 */
  #define caveConfig   3 /* upgr20 */
  #define ecalConfig   1 /* upgr20 */
  #define fpdConfig   4 /* upgr20 */
  #define ftroConfig   1 /* upgr20 */
  #define ftpcConfig   2 /* upgr20 */
  #define hallConfig   3 /* upgr20 */
  #define ibemConfig   2 /* upgr20 */
  #define magpConfig   1 /* upgr20 */
  #define mutdConfig   3 /* upgr20 */
  #define phmdConfig   1 /* upgr20 */
  #define pipeConfig   1 /* upgr20 */
  #define sconConfig   3 /* upgr20 */
  #define ssdConfig    8 /* upgr20 */
  #define supportConfig   1 /* upgr20 */
  #define svtConfig   13  /* 9 => 13  upgr20 */
  #define tpcConfig   3 /* upgr20 */
  #define upstreamConfig   1 /* upgr20 */
  #define vpdConfig   2 /* upgr20 */
  #define zcalConfig   1 /* upgr20 */
#endif /* upgr20 */
