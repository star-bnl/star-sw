/*
 * Description: L3 unpacking code
 *             
 ****************************************************************************/

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
        cout << "ERROR: no L3_GTD bank" << endl;
        pBankL3GTD = NULL;
  }
  else {
        pBankL3GTD = (Bank_L3_GTD *) ((INT32 *)pBankL3P + pBankL3P->tracks.offset);
        if (strncmp(pBankL3GTD->header.BankType, CHAR_L3_GTD, 8) != 0) {
	      cout << "ERROR: L3_GTD missing bank id" << endl;
	      pBankL3GTD = NULL;
	}
  }
  return pBankL3GTD;
}


Bank_L3_SECP * L3_Reader::getL3_SECP (int sec)
{
  if (pBankL3P->sector[sec-1].length==0) {
        cout << "ERROR: no L3_SECP bank for sector " << sec << endl;
        pBankL3SECP = NULL;
  }
  else {
        pBankL3SECP = (Bank_L3_SECP *) ((INT32 *)pBankL3P + pBankL3P->sector[sec-1].offset);
        if (strncmp(pBankL3SECP->header.BankType, CHAR_L3_SECP, 8) != 0) {
	      cout << "ERROR: L3_SECP missing bank id" << endl;
	      pBankL3SECP = NULL;
	}
  }
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
        cout << "ERROR: no L3_SECCD bank for sector " << sec << endl;
        pBankL3SECCD = NULL;
  }
  else {
        pBankL3SECCD = (Bank_L3_SECCD *) ((INT32 *)pBankL3SECP + pBankL3SECP->sl3clusterp.offset);
        if (strncmp(pBankL3SECCD->header.BankType, CHAR_L3_SECCD, 8) != 0) {
	      cout << "ERROR: L3_SECCD missing bank id" << endl;
	      pBankL3SECCD = NULL;
	}
  }
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
        cout << "ERROR: no L3_SECTP bank for sector " << sec << endl;
        pBankL3SECTP = NULL;
  }
  else {
        pBankL3SECTP = (Bank_L3_SECTP *) ((INT32 *)pBankL3SECP + pBankL3SECP->trackp.offset);
        if (strncmp(pBankL3SECTP->header.BankType, CHAR_L3_SECTP, 8) != 0) {
	      cout << "ERROR: L3_SECTP missing bank id" << endl;
	      pBankL3SECTP = NULL;
	}
  }
  return pBankL3SECTP;
  
}


GlobalTrackReader * L3_Reader::getGlobalTrackReader ()
{
  GlobalTrackReader *gtr = new GlobalTrackReader (this);
  if (!gtr->initialize()) {
        cout << "ERROR: getGlobalTrackReader FAILED" << endl;
	delete gtr;
	return NULL;
  }

  return gtr;
}


Sl3ClusterReader * L3_Reader::getSl3ClusterReader (int sec)
{
  Sl3ClusterReader *cr = new Sl3ClusterReader (sec, this);
  if (!cr->initialize()) {
        cout << "ERROR: getSl3ClusterReader FAILED" << endl;
	delete cr;
	return NULL;
  }
  return cr;
}


Sl3TrackReader * L3_Reader::getSl3TrackReader (int sec)
{
  Sl3TrackReader *tr = new Sl3TrackReader (sec, this);
  if (!tr->initialize()) {
        cout << "ERROR: getSl3TrackReader FAILED" << endl;
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
        cout << "no L3_GTD found" << endl;
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
        cout << "no L3_SECCD found" << endl;
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
        cout << "no L3_SECTP found" << endl;
	return FALSE;
  }

  // check existence of local track bank
  if (pL3SECTP->banks[0].length == 0) {
        cout << "ERROR: no L3_LTD bank for sector " << sector << endl;
	return FALSE;
  }

  pL3LTD = (Bank_L3_LTD *) ((INT32 *)pL3SECTP + pL3SECTP->banks[0].offset);
  if (strncmp(pL3LTD->header.BankType, CHAR_L3_LTD, 8) != 0) {
        cout << "ERROR: L3_LTD missing bank id" << endl;
	return FALSE;
  }

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
