/***************************************************************************
 *
 * $Id: L3_Reader.cxx,v 1.3 2000/07/06 18:16:01 ward Exp $
 *
 * Author: Christof Struck, struck@star.physics.yale.edu
 ***************************************************************************
 *
 * Description: L3 unpacking code
 *
 *
 *
 * change log:
 *   06 Jun 00 CS initial version
 *
 ***************************************************************************
 *
 * $Log: L3_Reader.cxx,v $
 * Revision 1.3  2000/07/06 18:16:01  ward
 * Install L3 code from Christof Struck.
 *
 *
 **************************************************************************/
#include "L3_Reader.hh"

//-------- L3_Reader----------------------------------------

L3_Reader::L3_Reader(EventReader *er, Bank_L3_P *pL3P)
{
  pBankL3P = pL3P;
  pBankL3GTD = NULL;
  pBankL3SECP = NULL;
  pBankL3SECCD = NULL;
  pBankL3SECTP = NULL;
}


Bank_L3_GTD * L3_Reader::getL3_GTD ()
{
  if (pBankL3P->tracks.length==0) {
        L3ERROR(INFO_MISSING_BANK, "no L3_GTD bank");
        return NULL;
  }

  pBankL3GTD = (Bank_L3_GTD *) ((INT32 *)pBankL3P + pBankL3P->tracks.offset);
  if (strncmp(pBankL3GTD->header.BankType, CHAR_L3_GTD, 8) != 0) {
        L3ERROR(ERR_BAD_HEADER, "bad L3_GTD header");
	pBankL3GTD = NULL;
	return pBankL3GTD;
  }

  if (pBankL3GTD->swap() < 0) L3ERROR(ERR_SWAP, "swap L3_GTD");

  return pBankL3GTD;
}


Bank_L3_SECP * L3_Reader::getL3_SECP (int sec)
{
  if (pBankL3P->sector[sec-1].length==0) {
        L3secERROR(INFO_MISSING_BANK, "no L3_SECP bank", sec);
        pBankL3SECP = NULL;
	return pBankL3SECP;
  }

  pBankL3SECP = (Bank_L3_SECP *) ((INT32 *)pBankL3P + pBankL3P->sector[sec-1].offset);
  if (strncmp(pBankL3SECP->header.BankType, CHAR_L3_SECP, 8) != 0) {
        L3secERROR(ERR_BAD_HEADER, "bad L3_SECP header", sec);
	pBankL3SECP = NULL;
	return pBankL3SECP;
  }

  if (pBankL3SECP->swap() < 0) L3secERROR(ERR_SWAP, "swap L3_SECP", sec);

  return pBankL3SECP;
}


Bank_L3_SECCD * L3_Reader::getL3_SECCD (int sec)
{
  // get L3_SECP for this sector first
  pBankL3SECP = getL3_SECP(sec);
  if (pBankL3SECP==NULL) {
        pBankL3SECCD = NULL;
	return pBankL3SECCD;
  }
  
  // now check on L3_SECCD
  if (pBankL3SECP->sl3clusterp.length==0) {
        L3secERROR(INFO_MISSING_BANK, "no L3_SECCD bank", sec);
        pBankL3SECCD = NULL;
	return pBankL3SECCD;
  }

  pBankL3SECCD = (Bank_L3_SECCD *) ((INT32 *)pBankL3SECP + pBankL3SECP->sl3clusterp.offset);
  if (strncmp(pBankL3SECCD->header.BankType, CHAR_L3_SECCD, 8) != 0) {
        L3secERROR(ERR_BAD_HEADER, "bad L3_SECCD header", sec);
	pBankL3SECCD = NULL;
	return pBankL3SECCD;
  }

  if (pBankL3SECCD->swap() < 0) L3secERROR(ERR_SWAP, "swap L3_SECCD", sec);

  return pBankL3SECCD;
}


Bank_L3_SECTP * L3_Reader::getL3_SECTP (int sec)
{
  // get L3_SECP for this sector first
  pBankL3SECP = getL3_SECP(sec);
  if (pBankL3SECP==NULL) {
        pBankL3SECTP = NULL;
	return pBankL3SECTP;
  }
  
  // now check on L3_SECTP
  if (pBankL3SECP->trackp.length==0) {
        L3secERROR(INFO_MISSING_BANK, "no L3_SECTP bank", sec);
        pBankL3SECTP = NULL;
	return pBankL3SECTP;
  }

  pBankL3SECTP = (Bank_L3_SECTP *) ((INT32 *)pBankL3SECP + pBankL3SECP->trackp.offset);
  if (strncmp(pBankL3SECTP->header.BankType, CHAR_L3_SECTP, 8) != 0) {
        L3secERROR(ERR_BAD_HEADER, "bad L3_SECTP header", sec);
	pBankL3SECTP = NULL;
	return pBankL3SECTP;
  }

  if (pBankL3SECTP->swap() < 0) L3secERROR(ERR_SWAP, "swap L3_SECTP", sec);

  return pBankL3SECTP;
}


GlobalTrackReader * L3_Reader::getGlobalTrackReader ()
{
  GlobalTrackReader *gtr = new GlobalTrackReader (this);
  if (!gtr->initialize()) {
        //cout << "ERROR: getGlobalTrackReader FAILED" << endl;
	delete gtr;
	return NULL;
  }

  return gtr;
}


Sl3ClusterReader * L3_Reader::getSl3ClusterReader (int sec)
{
  Sl3ClusterReader *cr = new Sl3ClusterReader (sec, this);
  if (!cr->initialize()) {
        //cout << "ERROR: getSl3ClusterReader FAILED" << endl;
	delete cr;
	return NULL;
  }
  return cr;
}


Sl3TrackReader * L3_Reader::getSl3TrackReader (int sec)
{
  Sl3TrackReader *tr = new Sl3TrackReader (sec, this);
  if (!tr->initialize()) {
        //cout << "ERROR: getSl3TrackReader FAILED" << endl;
	delete tr;
	return NULL;
  }
  return tr;
}


//--------- GlobalTrackReader ------------------------------

GlobalTrackReader::GlobalTrackReader (L3_Reader *l3r)
{
  l3 = l3r;
  pL3GTD = NULL;
  tracks = NULL;
  nTracks     = 0;
  nHits       = 0;
  glbVertex.x = 0;
  glbVertex.y = 0;
  glbVertex.z = 0;
}


int GlobalTrackReader::initialize () 
{
  pL3GTD = l3->getL3_GTD();
  if (pL3GTD == NULL) {
        //cout << "no L3_GTD found" << endl;
	return FALSE;
  }
  
  tracks  = pL3GTD->track;
  nTracks = pL3GTD->nTracks;
  nHits   = pL3GTD->nHits;

  glbVertex.x = pL3GTD->xVert * 1e-6;
  glbVertex.y = pL3GTD->yVert * 1e-6;
  glbVertex.z = pL3GTD->zVert * 1e-6;

  return TRUE;
}


// ---------- Sl3ClusterReader -----------------------

Sl3ClusterReader::Sl3ClusterReader (int sec, L3_Reader *l3r)
{
  l3 = l3r;
  sector = sec;
  pL3SECCD = NULL;
  cluster  = NULL;
  nCluster = 0;
}


int Sl3ClusterReader::initialize ()
{
  pL3SECCD = l3->getL3_SECCD(sector);
  if (pL3SECCD == NULL) {
        //cout << "no L3_SECCD found" << endl;
	return FALSE;
  }
  
  cluster  = pL3SECCD->cluster;
  nCluster = pL3SECCD->nrClusters_in_sector;

  return TRUE;
}


// ---------- Sl3TrackReader -----------------------

Sl3TrackReader::Sl3TrackReader (int sec, L3_Reader *l3r)
{
  l3 = l3r;
  sector = sec;
  pL3LTD = NULL;
  tracks  = NULL;
  nTracks = 0;
  nHits   = 0;
  cpuTime = 0;
  realTime = 0;
  paraSet = 0;
  locVertex.x = 0;
  locVertex.y = 0;
  locVertex.z = 0;
}


int Sl3TrackReader::initialize ()
{
  pL3SECTP = l3->getL3_SECTP(sector);
  if (pL3SECTP == NULL) {
        //cout << "no L3_SECTP found" << endl;
	return FALSE;
  }

  // check existence of local track bank
  if (pL3SECTP->banks[0].length == 0) {
        pL3secERROR(INFO_MISSING_BANK, "no L3_LTD bank", sector);
	return FALSE;
  }

  pL3LTD = (Bank_L3_LTD *) ((INT32 *)pL3SECTP + pL3SECTP->banks[0].offset);
  if (strncmp(pL3LTD->header.BankType, CHAR_L3_LTD, 8) != 0) {
        pL3secERROR(ERR_BAD_HEADER, "bad L3_LTD header", sector);
	return FALSE;
  }

  if (pL3LTD->swap() < 0) pL3secERROR(ERR_SWAP, "swap L3_LTD", sector);


  //pL3LTD->header.print();
  //printf("+++++>> L3_LTD: nTracks %i\n",
  //       (int) (pL3LTD->header.BankLength * 4 - sizeof(Bank_Header)) / sizeof(localTrack));

  tracks  = pL3LTD->track;
  nTracks = pL3SECTP->nTracks;
  nHits   = pL3SECTP->nHits;
  cpuTime = pL3SECTP->cpuTime;
  realTime = pL3SECTP->realTime;
  paraSet = pL3SECTP->para;
  locVertex.x = pL3SECTP->xVert;
  locVertex.y = pL3SECTP->yVert;
  locVertex.z = pL3SECTP->zVert;

  return TRUE;
}
