#ifndef StDetectorDefinitions_hh
#define StDetectorDefinitions_hh

/* Numbering scheme for detectors
** TPC             = 1
** SVT             = 2
** RICH            = 3
** FTPC west       = 4
** FTPC east       = 5
** TOF             = 6
** CTB             = 7
** SSD             = 8
** barrel EMC tower= 9
** barrel EMC pre-shower = 10
** barrel SMD eta strip  = 11
** barrel SMD phi strip  = 12
** endcap EMC tower      = 13
** endcap EMC pre-shower = 14
** endcap SMD eta strip  = 15
** endcap SMD phi strip  = 16
** Zero Degree Calo west = 17
** Zero Degree Calo east = 18
** MWPC west       = 19
** MWPC east       = 20
** TPC+SSD         = 21
** TPC+SVT         = 22
** TPC+SSD+SVT     = 23
** SSD+SVT         = 24
** CPV(PMD)        = 25
** PMD             = 26
** Pixel Layer (HFT) PXL = 27
** Strip Layer (HFT) IST = 28
** Forward Tracker   FGT = 29
** FPD West        = 30
** FPD East        = 31
** FMS             = 32
** MTD             = 34
** ETR             = 35
** SST             = 36
** GMT             = 37
** FTS             = 38
** iTPC            = 39
** ETof            = 40
*/




#define kUnknownIdentifier             0
#define kTpcIdentifier                 1
#define kSvtIdentifier                 2
#define kRichIdentifier                3
#define kFtpcWestIdentifier            4
#define kFtpcEastIdentifier            5
#define kTofIdentifier                 6
#define kCtbIdentifier                 7
#define kSsdIdentifier                 8
#define kBarrelEmcTowerIdentifier      9
#define kBarrelEmcPreShowerIdentifier 10
#define kBarrelSmdEtaStripIdentifier  11
#define kBarrelSmdPhiStripIdentifier  12
#define kEndcapEmcTowerIdentifier     13
#define kEndcapEmcPreShowerIdentifier 14
#define kEndcapSmdUStripIdentifier    15
#define kEndcapSmdVStripIdentifier    16
#define kZdcWestIdentifier            17
#define kZdcEastIdentifier            18
#define kMwpcWestIdentifier           19
#define kMwpcEastIdentifier           20
#define kTpcSsdIdentifier             21
#define kTpcSvtIdentifier             22
#define kTpcSsdSvtIdentifier          23
#define kSsdSvtIdentifier             24
#define kPhmdCpvIdentifier            25
#define kPhmdIdentifier               26


/*
**  The following are for the inner and forward
**  tracking upgrades. tu (Oct 11, 2007)
*/
#define kPxlIdentifier                27
#define kIstIdentifier                28
#define kFgtIdentifier                29

/*
**  The following are for the forward
**  spectrometers (tu April 6, 2009)
*/
#define kFpdWestIdentifier            30
#define kFpdEastIdentifier            31 
#define kFmsIdentifier                32

/*
 **  The following are for the Roman Pot
 **  silicon detectors (pp2pp) (tu November 6, 2009)
 */
#define kRpsIdentifier                33

/*
 **  The following are for the Muon Telescope Detector
 */
#define kMtdIdentifier                34

/*
 **  The following are for the Endcap TRD in ETTIE detector
 */
#define kEtrIdentifier                35

/*
 **  The following are for the SST (Beware: not same as SSD)
 */
#define kSstIdentifier                36
/*
**  The following is the addition of the GEM chambers
**  to improve TPC tracking and alignment. RW (Mar 27, 2013)
*/
#define kGmtIdentifier                37

/*
** Add the FTS tracker
*/
#define kFtsIdentifier                38 

/*
 ** iTPC
 */
#define kiTpcIdentifier               39

/*
 ** eTOF
 */
#define kETofIdentifier               40


/*
**  The following are more or less virtual detectors.
**  Depending on funding or policy this stuff might
**  happen or not. (OBSOLETE)
*/
/*
#define kHftIdentifier                27
#define kIstIdentifier                28
#define kIgtIdentifier                29
#define kFstIdentifier                30
#define kFgtIdentifier                31
#define kHpdIdentifier                32
*/

#endif /*STDETECTORDEFINITIONS*/

/* $Id: StDetectorDefinitions.h,v 2.15 2019/02/11 18:49:59 ullrich Exp $
**
** $Log: StDetectorDefinitions.h,v $
** Revision 2.15  2019/02/11 18:49:59  ullrich
** Added EToF.
**
** Revision 2.14  2018/03/27 02:40:12  genevb
** Introduce kiTpcId
**
** Revision 2.13  2017/05/04 00:52:53  perev
** Fts added
**
** Revision 2.12  2015/12/24 00:14:44  fisyak
** Add GMT and SST Id and new dE/dx method
**
** Revision 2.11  2015/05/13 17:06:13  ullrich
** Added hooks and interfaces to Sst detector (part of HFT).
**
** Revision 2.10  2012/01/24 02:58:21  perev
** Etr detector added
**
** Revision 2.9  2011/04/25 21:25:09  ullrich
** Modifications to hold MTD data.
**
** Revision 2.8  2009/11/23 22:22:25  ullrich
** Minor cleanup performed and hooks for RPS added.
**
** Revision 2.7  2009/04/06 19:23:53  ullrich
** Add detector Ids for FPD East/West and FMS.
**
** Revision 2.6  2007/10/11 21:50:19  ullrich
** Added new enums for PXL and IST detectors.
**
** Revision 2.5  2006/08/15 14:34:02  ullrich
** Added kHpdIdentifier.
**
** Revision 2.4  2006/01/20 15:11:59  jeromel
** ... meant needs to be C style, not C++
**
** Revision 2.3  2006/01/20 15:11:26  jeromel
** Comments need to be FORtran style
**
** Revision 2.2  2006/01/19 21:51:26  ullrich
** Added new RnD detectors.
**
** Revision 2.1  2004/04/26 16:35:19  fisyak
** Move enumerations from pams/global/inc => StEvent
**
** Revision 1.10  2002/12/19 21:52:38  lbarnby
** Corrected CVS tags
**
*/
