/***************************************************************************
 *
 * $Id: L3_Reader.cxx,v 1.8 2000/09/30 16:13:46 fisyak Exp $
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
 *   25 Jul 00 CS added i960 cluster reader
 *   09 Aug 00 CS removed 'cout' statement in i960 reader
 *   11 Sep 00 CS removed memory leak in L3_Reader
 *
 ***************************************************************************
 *
 * $Log: L3_Reader.cxx,v $
 * Revision 1.8  2000/09/30 16:13:46  fisyak
 * take out derivative and result from Streamer
 *
 * Revision 1.7  2000/09/11 16:31:12  struck
 * removed memory leak in L3_Reader
 *
 * Revision 1.6  2000/08/09 15:27:03  struck
 * removed 'cout' statement in i960ClusterReader
 *
 * Revision 1.5  2000/07/26 02:38:06  struck
 * minor changes
 *
 * Revision 1.4  2000/07/26 02:12:28  struck
 * added i960 cluster reader
 *
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
  gtr = NULL;
  scr = NULL;
  str = NULL;
  icr = NULL;
}

L3_Reader::~L3_Reader()
{
  delete gtr;
  delete scr;
  delete str;
  delete icr;
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


Bank_TPCSECLP * L3_Reader::getTPCSECLP (int sec)
{
  // get L3_SECP for this sector first
  pBankL3SECP = getL3_SECP(sec);
  if (pBankL3SECP==NULL) {
        pBankTPCSECLP = NULL;
	return pBankTPCSECLP;
  }
  
  // now check on TPCSECLP
  if (pBankL3SECP->clusterp.length==0) {
        L3secERROR(INFO_MISSING_BANK, "no TPCSECLP bank", sec);
        pBankTPCSECLP = NULL;
	return pBankTPCSECLP;
  }

  pBankTPCSECLP = (Bank_TPCSECLP *) ((INT32 *)pBankL3SECP + pBankL3SECP->clusterp.offset);
  if (strncmp(pBankTPCSECLP->header.BankType, CHAR_TPCSECLP, 8) != 0) {
        L3secERROR(ERR_BAD_HEADER, "bad TPCSECLP header", sec);
	pBankTPCSECLP = NULL;
	return pBankTPCSECLP;
  }

  if (pBankTPCSECLP->swap() < 0) L3secERROR(ERR_SWAP, "swap TPCSECLP", sec);

  return pBankTPCSECLP;

}


Bank_TPCRBCLP * L3_Reader::getTPCRBCLP (int sec, int rb)
{
  pBankTPCSECLP = getTPCSECLP(sec);
  if (pBankTPCSECLP==NULL) {
        pBankTPCRBCLP = NULL;
	return pBankTPCRBCLP;
  }

  // now check on TPCRBCLP
  if (pBankTPCSECLP->receiverBoard[rb-1].length==0) {
        L3secERROR(INFO_MISSING_BANK, "no TPCRBCLP bank", sec);
        pBankTPCRBCLP = NULL;
	return pBankTPCRBCLP;
  }

  pBankTPCRBCLP = (Bank_TPCRBCLP *) ((INT32 *)pBankTPCSECLP + pBankTPCSECLP->receiverBoard[rb-1].offset);
  if (strncmp(pBankTPCRBCLP->header.BankType, CHAR_TPCRBCLP, 8) != 0) {
        L3secERROR(ERR_BAD_HEADER, "bad TPCRBCLP header", sec);
	pBankTPCRBCLP = NULL;
	return pBankTPCRBCLP;
  }

  if (pBankTPCRBCLP->swap() < 0) L3secERROR(ERR_SWAP, "swap TPCRBCLP", sec);

  return pBankTPCRBCLP;

}


Bank_TPCMZCLD * L3_Reader::getTPCMZCLD (int sec, int rb, int mz)
{
  pBankTPCRBCLP = getTPCRBCLP(sec, rb);
  if (pBankTPCRBCLP==NULL) {
        pBankTPCMZCLD = NULL;
	return pBankTPCMZCLD;
  }

  // now check on TPCMZCLD
  if (pBankTPCRBCLP->mezzBoard[mz-1].length==0) {
        L3secERROR(INFO_MISSING_BANK, "no TPCMZCLD bank", sec);
        pBankTPCMZCLD = NULL;
	return pBankTPCMZCLD;
  }

  pBankTPCMZCLD = (Bank_TPCMZCLD *) ((INT32 *)pBankTPCRBCLP + pBankTPCRBCLP->mezzBoard[mz-1].offset);
  if (strncmp(pBankTPCMZCLD->header.BankType, CHAR_TPCMZCLD, 8) != 0) {
        L3secERROR(ERR_BAD_HEADER, "bad TPCMZCLD header", sec);
	pBankTPCMZCLD = NULL;
	return pBankTPCMZCLD;
  }

  if (pBankTPCMZCLD->swap() < 0) L3secERROR(ERR_SWAP, "swap TPCMZCLD", sec);

  return pBankTPCMZCLD;

}


GlobalTrackReader * L3_Reader::getGlobalTrackReader ()
{
  // only one reader per event
  if (!gtr) {
        gtr = new GlobalTrackReader (this);
	if (!gtr->initialize()) {
	      //cout << "ERROR: getGlobalTrackReader FAILED" << endl;
	      return NULL;
	}
  }
  return gtr;
}


Sl3ClusterReader * L3_Reader::getSl3ClusterReader (int sec)
{
  if (!scr) scr = new Sl3ClusterReader (this);
  if (!scr->initialize(sec)) {
        //cout << "ERROR: getSl3ClusterReader FAILED" << endl;
        return NULL;
  }
  return scr;
}


Sl3TrackReader * L3_Reader::getSl3TrackReader (int sec)
{
  if (!str) str = new Sl3TrackReader (this);
  if (!str->initialize(sec)) {
        //cout << "ERROR: getSl3TrackReader FAILED" << endl;
        return NULL;
  }
  return str;
}


I960ClusterReader * L3_Reader::getI960ClusterReader (int sec)
{
  if (!icr) icr = new I960ClusterReader (this);
  if (!icr->initialize(sec)) {
        //cout << "ERROR: getI960ClusterReader FAILED" << endl;
	return NULL;
  }
  return icr;
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

Sl3ClusterReader::Sl3ClusterReader (L3_Reader *l3r)
{
  l3 = l3r;
  sector = -1;
  pL3SECCD = NULL;
  cluster  = NULL;
  nCluster = 0;
}


int Sl3ClusterReader::initialize (int sec)
{
  // is this sector already initialized?
  if (sector!=sec) {
        // set sector number
        sector = sec;
	pL3SECCD = l3->getL3_SECCD(sector);
	if (pL3SECCD == NULL) {
	      //cout << "no L3_SECCD found" << endl;
	      return FALSE;
	}
	cluster  = pL3SECCD->cluster;
	nCluster = pL3SECCD->nrClusters_in_sector;
  }
  return TRUE;
}


// ---------- I960ClusterReader --------------------

I960ClusterReader::I960ClusterReader (L3_Reader *l3r)
{
  l3 = l3r;
  sector = -1;
  cluster = new l3_cluster[maxClusterPerSector];
  nCluster = 0;
  for (int rb=0; rb<12; rb++) {
        for (int mz=0; mz<3; mz++) {
	      pBankTPCMZCLD[rb][mz] = NULL;
	}
  }
}


I960ClusterReader::~I960ClusterReader ()
{
  delete [] cluster;
}


int I960ClusterReader::initialize (int sec)
{
  Bank_TPCMZCLD *cld;

  struct Centroids {
    unsigned short x;
    unsigned short t;
  }; 

  struct SpacePt {
    Centroids centroids;
    unsigned short flags;
    unsigned short q;
  };
  // is this sector already initialized?
  if (sector!=sec) {
        // set sector number
        sector = sec;
	nCluster = 0;
	// get number of clusters
	int rb;
	for (rb=1; rb<=12; rb++) {
	      for (int mz=1; mz<=3; mz++) {
		    //pointer to TPCMZCLD bank
		    pBankTPCMZCLD[rb-1][mz-1] = l3->getTPCMZCLD(sector, rb, mz); 
		    cld = pBankTPCMZCLD[rb-1][mz-1];
		    if (!cld) continue;
		    int *ptr = (int *)&cld->stuff;
		    // count total number of clusters for memory allocation
		    for (int ir=0; ir<cld->numberOfRows; ir++) {
		          int row = *ptr++;
			  int nHitsThisRow = *ptr++;  // bump pointer to beginning of space points
			  nCluster += nHitsThisRow;   // add num space pts to running total
			  ptr += 2 * nHitsThisRow;
		    }
	      }
	}
	//cout << "sector "<<sector<<": found " 
	//     <<nCluster<<" space pts" <<endl;

	if (nCluster>maxClusterPerSector) {
	      cout << "ERROR: L3_Reader: reached maxClusterPerSector limit!" << endl;
	      return FALSE;
	}
	if (cluster==NULL) {
	      cout << "failed to allocate cluster structures " << endl;
	      return FALSE;
	}


	// copy i960 cluster into l3 cluster struct
	l3_cluster *pcluster;
	pcluster = cluster;

	for ( rb=1; rb<=12; rb++) {
	      for (int mz=1; mz<=3; mz++) {
		    cld = pBankTPCMZCLD[rb-1][mz-1];  // pointer to TPCMZCLD bank
		    if (!cld) continue;
		    int *ptr = &cld->stuff[0];
		    for (int ir=0; ir<cld->numberOfRows; ir++){
		          int row = *ptr++;
			  int nsp = *ptr++;           // bump pointer to beginning of space points
			  for (int isp=0; isp<nsp; isp++, ptr+=2) {
			        SpacePt *hit = (SpacePt *)ptr;
				pcluster->pad     = hit->centroids.x;
				pcluster->time    = hit->centroids.t;
				pcluster->charge  = hit->q;
				pcluster->flags   = hit->flags;
				pcluster->padrow  = row;
				pcluster->RB_MZ   = 16 * rb + mz;
				pcluster->trackId = 0; // no track ass. for i960 cluster
				pcluster++;
			  }
		    }
	      }
	}
  }
  return TRUE;
}


// ---------- Sl3TrackReader -----------------------

Sl3TrackReader::Sl3TrackReader (L3_Reader *l3r)
{
  l3 = l3r;
  sector = -1;
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


int Sl3TrackReader::initialize (int sec)
{
  // is this sector already initialized?
  if (sector!=sec) {
        // set sector number
        sector = sec;
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
  }
  return TRUE;
}
