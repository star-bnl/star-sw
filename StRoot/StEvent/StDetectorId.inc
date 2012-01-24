      INTEGER kUnknownId,kTpcId,kSvtId,kRichId,kFtpcWestId,kFtpcEastId
      INTEGER kTofId,kCtbId,kSsdId,kBarrelEmcTowerId
      INTEGER kBarrelEmcPreShowerId,kBarrelSmdEtaStripId
      INTEGER kEndcapEmcTowerId,kEndcapEmcPreShowerId
      INTEGER kBarrelSmdPhiStripId,kZdcWestId,kZdcEastId
      INTEGER kEndcapSmdUStripId,kEndcapSmdVStripId,kMwpcWestId,kMwpcEastId
      INTEGER kTpcSvtId,kTpcSsdId,kTpcSsdsvtId,kSsdSvtId,kPhmdCpvId,kPhmdId
      INTEGER kPxlId,kIstId,kFgtId,kFpdWestId,kFpdEastId,kFmsId,kEtrId

#include "StDetectorDefinitions.h"
      PARAMETER (kUnknownId = kUnknownIdentifier)
      PARAMETER (kTpcId = kTpcIdentifier)
      PARAMETER (kSvtId = kSvtIdentifier)
      PARAMETER (kRichId = kRichIdentifier)
      PARAMETER (kFtpcWestId = kFtpcWestIdentifier)
      PARAMETER (kFtpcEastId = kFtpcEastIdentifier)
      PARAMETER (kTofId = kTofIdentifier)
      PARAMETER (kCtbId = kCtbIdentifier)
      PARAMETER (kSsdId = kSsdIdentifier)
      PARAMETER (kBarrelEmcTowerId = kBarrelEmcTowerIdentifier)
      PARAMETER (kBarrelEmcPreShowerId = kBarrelEmcPreShowerIdentifier)
      PARAMETER (kBarrelSmdEtaStripId = kBarrelSmdEtaStripIdentifier)
      PARAMETER (kBarrelSmdPhiStripId = kBarrelSmdPhiStripIdentifier)
      PARAMETER (kEndcapEmcTowerId = kEndcapEmcTowerIdentifier)
      PARAMETER (kEndcapEmcPreShowerId = kEndcapEmcPreShowerIdentifier)
      PARAMETER (kEndcapSmdUStripId = kEndcapSmdUStripIdentifier)
      PARAMETER (kEndcapSmdVStripId = kEndcapSmdVStripIdentifier)
      PARAMETER (kZdcWestId = kZdcWestIdentifier)
      PARAMETER (kZdcEastId = kZdcEastIdentifier)
      PARAMETER (kMwpcWestId = kMwpcWestIdentifier)
      PARAMETER (kMwpcEastId = kMwpcEastIdentifier)
      PARAMETER (kTpcSsdId    = kTpcSsdIdentifier)
      PARAMETER (kTpcSvtId    = kTpcSvtIdentifier)
      PARAMETER (kTpcSsdSvtId = kTpcSsdSvtIdentifier)
      PARAMETER (kSsdSvtId    = kSsdSvtIdentifier)
      PARAMETER (kPhmdCpvId = kPhmdCpvIdentifier)
      PARAMETER (kPhmdId = kPhmdIdentifier)
      PARAMETER (kPxlId       = kPxlIdentifier)
      PARAMETER (kIstId       = kIstIdentifier)
      PARAMETER (kFgtId       = kFgtIdentifier)
      PARAMETER (kEtrId       = kEtrIdentifier)
      PARAMETER (kFpdWestId   = kFpdWestIdentifier)
      PARAMETER (kFpdEastId   = kFpdEastIdentifier) 
      PARAMETER (kFmsId       = kFmsIdentifier)    
  
C     $Id: StDetectorId.inc,v 2.3 2012/01/24 03:00:22 perev Exp $
C
C     $Log: StDetectorId.inc,v $
C     Revision 2.3  2012/01/24 03:00:22  perev
C     Etr detector added
C
C     Revision 2.2  2009/04/06 19:23:53  ullrich
C     Add detector Ids for FPD East/West and FMS.
C
C     Revision 2.1  2004/04/26 16:35:19  fisyak
C     Move enumerations from pams/global/inc => StEvent
C
C     Revision 1.9  2002/12/18 02:28:52  lbarnby
C     PMD identifiers added
C





