/***************************************************************************
 *
 * $Id: L3_Reader.cxx,v 1.13 2007/12/24 06:04:20 fine Exp $
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
 * Revision 1.13  2007/12/24 06:04:20  fine
 * introduce OLDEVP namespace to allow ole and new EVP library concurrently
 *
 * Revision 1.12  2001/09/24 21:42:56  struck
 * cs: changed vertex info to float (unit [cm]) in Bank_L3_GTD
 *
 * Revision 1.11  2001/08/20 05:37:45  struck
 * removed naming conflicts with 'Stl3Utils/foreign/L3Formats.h'
 *
 * Revision 1.10  2001/08/17 17:12:27  struck
 * cs: getI960ClusterReader() now returns NULL pointer if number of clusters == 0
 *
 * Revision 1.9  2001/07/17 19:16:11  struck
 * update to 2001 data format (backwards compatible)Z
 *
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



//##########################################################
//-     L3_Reader
//##########################################################
using namespace OLDEVP;
L3_Reader::L3_Reader(EventReader *er, Bank_L3_P *pL3P)
{
  mBankL3P = pL3P;
  mBankL3SUMD = NULL;
  mBankL3GTD = NULL;
  mBankL3SECP = NULL;
  mBankL3SECCD = NULL;
  mBankL3SECTP = NULL;
  mBankTPCSECLP = NULL;
  mBankTPCRBCLP = NULL;
  mBankTPCMZCLD = NULL;
  mSVT = NULL;
  mFTPC = NULL;
  mEMC = NULL;
  mAlr = NULL;
  mGtr = NULL;
  mScr = NULL;
  mStr = NULL;
  mIcr = NULL;

  // DetectorReader checks if L3_P bank exits
  // so we can access it here without checking again
  mTime = mBankL3P->time;
  mGl3Id = mBankL3P->gl3Id;
  mL3sum = (L3_Summary *)mBankL3P->L3_Summary;
}

//----------------------------------------------------------

L3_Reader::~L3_Reader()
{
  delete mAlr;
  delete mGtr;
  delete mScr;
  delete mStr;
  delete mIcr;
}

//----------------------------------------------------------

Bank_L3_SUMD * L3_Reader::getL3_SUMD()
{
  // check format version first
  if (mBankL3P->header.FormatNumber < 4) {
        L3ERROR(INFO_MISSING_BANK, "wrong data format, no L3_SUMD bank");
	return NULL;
  }

  // now check for the offset/length
  if (mBankL3P->summary_data.length==0) {
        L3ERROR(INFO_MISSING_BANK, "no L3_SUMD bank");
        return NULL;
  }

  mBankL3SUMD = (Bank_L3_SUMD *) ((INT32 *)mBankL3P + mBankL3P->summary_data.offset);
  if (strncmp(mBankL3SUMD->header.BankType, CHAR_L3_SUMD, 8) != 0) {
        L3ERROR(ERR_BAD_HEADER, "bad L3_SUMD header");
	mBankL3SUMD = NULL;
	return mBankL3SUMD;
  }

  if (mBankL3SUMD->swap() < 0) L3ERROR(ERR_SWAP, "swap L3_SUMD");

  return mBankL3SUMD;
}

//----------------------------------------------------------

Bank_L3_GTD * L3_Reader::getL3_GTD ()
{
  if (mBankL3P->tracks.length==0) {
        L3ERROR(INFO_MISSING_BANK, "no L3_GTD bank");
        return NULL;
  }

  mBankL3GTD = (Bank_L3_GTD *) ((INT32 *)mBankL3P + mBankL3P->tracks.offset);
  if (strncmp(mBankL3GTD->header.BankType, CHAR_L3_GTD, 8) != 0) {
        L3ERROR(ERR_BAD_HEADER, "bad L3_GTD header");
	mBankL3GTD = NULL;
	return mBankL3GTD;
  }

  if (mBankL3GTD->swap() < 0) L3ERROR(ERR_SWAP, "swap L3_GTD");

  return mBankL3GTD;
}

//----------------------------------------------------------

Bank_L3_SECP * L3_Reader::getL3_SECP (int sec)
{
  if (mBankL3P->sector[sec-1].length==0) {
        L3secERROR(INFO_MISSING_BANK, "no L3_SECP bank", sec);
        mBankL3SECP = NULL;
	return mBankL3SECP;
  }

  mBankL3SECP = (Bank_L3_SECP *) ((INT32 *)mBankL3P + mBankL3P->sector[sec-1].offset);
  if (strncmp(mBankL3SECP->header.BankType, CHAR_L3_SECP, 8) != 0) {
        L3secERROR(ERR_BAD_HEADER, "bad L3_SECP header", sec);
	mBankL3SECP = NULL;
	return mBankL3SECP;
  }

  if (mBankL3SECP->swap() < 0) L3secERROR(ERR_SWAP, "swap L3_SECP", sec);

  return mBankL3SECP;
}

//----------------------------------------------------------

Bank_L3_SECCD * L3_Reader::getL3_SECCD (int sec)
{
  // get L3_SECP for this sector first
  mBankL3SECP = getL3_SECP(sec);
  if (mBankL3SECP==NULL) {
        mBankL3SECCD = NULL;
	return mBankL3SECCD;
  }
  
  // now check on L3_SECCD
  if (mBankL3SECP->sl3clusterp.length==0) {
        L3secERROR(INFO_MISSING_BANK, "no L3_SECCD bank", sec);
        mBankL3SECCD = NULL;
	return mBankL3SECCD;
  }

  mBankL3SECCD = (Bank_L3_SECCD *) ((INT32 *)mBankL3SECP + mBankL3SECP->sl3clusterp.offset);
  if (strncmp(mBankL3SECCD->header.BankType, CHAR_L3_SECCD, 8) != 0) {
        L3secERROR(ERR_BAD_HEADER, "bad L3_SECCD header", sec);
	mBankL3SECCD = NULL;
	return mBankL3SECCD;
  }

  if (mBankL3SECCD->swap() < 0) L3secERROR(ERR_SWAP, "swap L3_SECCD", sec);

  return mBankL3SECCD;
}

//----------------------------------------------------------

Bank_L3_SECTP * L3_Reader::getL3_SECTP (int sec)
{
  // get L3_SECP for this sector first
  mBankL3SECP = getL3_SECP(sec);
  if (mBankL3SECP==NULL) {
        mBankL3SECTP = NULL;
	return mBankL3SECTP;
  }
  
  // now check on L3_SECTP
  if (mBankL3SECP->trackp.length==0) {
        L3secERROR(INFO_MISSING_BANK, "no L3_SECTP bank", sec);
        mBankL3SECTP = NULL;
	return mBankL3SECTP;
  }

  mBankL3SECTP = (Bank_L3_SECTP *) ((INT32 *)mBankL3SECP + mBankL3SECP->trackp.offset);
  if (strncmp(mBankL3SECTP->header.BankType, CHAR_L3_SECTP, 8) != 0) {
        L3secERROR(ERR_BAD_HEADER, "bad L3_SECTP header", sec);
	mBankL3SECTP = NULL;
	return mBankL3SECTP;
  }

  if (mBankL3SECTP->swap() < 0) L3secERROR(ERR_SWAP, "swap L3_SECTP", sec);

  return mBankL3SECTP;
}

//----------------------------------------------------------

Bank_TPCSECLP * L3_Reader::getTPCSECLP (int sec)
{
  // get L3_SECP for this sector first
  mBankL3SECP = getL3_SECP(sec);
  if (mBankL3SECP==NULL) {
        mBankTPCSECLP = NULL;
	return mBankTPCSECLP;
  }
  
  // now check on TPCSECLP
  if (mBankL3SECP->clusterp.length==0) {
        L3secERROR(INFO_MISSING_BANK, "no TPCSECLP bank", sec);
        mBankTPCSECLP = NULL;
	return mBankTPCSECLP;
  }

  mBankTPCSECLP = (Bank_TPCSECLP *) ((INT32 *)mBankL3SECP + mBankL3SECP->clusterp.offset);
  if (strncmp(mBankTPCSECLP->header.BankType, CHAR_TPCSECLP, 8) != 0) {
        L3secERROR(ERR_BAD_HEADER, "bad TPCSECLP header", sec);
	mBankTPCSECLP = NULL;
	return mBankTPCSECLP;
  }

  if (mBankTPCSECLP->swap() < 0) L3secERROR(ERR_SWAP, "swap TPCSECLP", sec);

  return mBankTPCSECLP;

}

//----------------------------------------------------------

Bank_TPCRBCLP * L3_Reader::getTPCRBCLP (int sec, int rb)
{
  mBankTPCSECLP = getTPCSECLP(sec);
  if (mBankTPCSECLP==NULL) {
        mBankTPCRBCLP = NULL;
	return mBankTPCRBCLP;
  }

  // now check on TPCRBCLP
  if (mBankTPCSECLP->receiverBoard[rb-1].length==0) {
        L3secERROR(INFO_MISSING_BANK, "no TPCRBCLP bank", sec);
        mBankTPCRBCLP = NULL;
	return mBankTPCRBCLP;
  }

  mBankTPCRBCLP = (Bank_TPCRBCLP *) ((INT32 *)mBankTPCSECLP + mBankTPCSECLP->receiverBoard[rb-1].offset);
  if (strncmp(mBankTPCRBCLP->header.BankType, CHAR_TPCRBCLP, 8) != 0) {
        L3secERROR(ERR_BAD_HEADER, "bad TPCRBCLP header", sec);
	mBankTPCRBCLP = NULL;
	return mBankTPCRBCLP;
  }

  if (mBankTPCRBCLP->swap() < 0) L3secERROR(ERR_SWAP, "swap TPCRBCLP", sec);

  return mBankTPCRBCLP;

}

//----------------------------------------------------------

Bank_TPCMZCLD * L3_Reader::getTPCMZCLD (int sec, int rb, int mz)
{
  mBankTPCRBCLP = getTPCRBCLP(sec, rb);
  if (mBankTPCRBCLP==NULL) {
        mBankTPCMZCLD = NULL;
	return mBankTPCMZCLD;
  }

  // now check on TPCMZCLD
  if (mBankTPCRBCLP->mezzBoard[mz-1].length==0) {
        L3secERROR(INFO_MISSING_BANK, "no TPCMZCLD bank", sec);
        mBankTPCMZCLD = NULL;
	return mBankTPCMZCLD;
  }

  mBankTPCMZCLD = (Bank_TPCMZCLD *) ((INT32 *)mBankTPCRBCLP + mBankTPCRBCLP->mezzBoard[mz-1].offset);
  if (strncmp(mBankTPCMZCLD->header.BankType, CHAR_TPCMZCLD, 8) != 0) {
        L3secERROR(ERR_BAD_HEADER, "bad TPCMZCLD header", sec);
	mBankTPCMZCLD = NULL;
	return mBankTPCMZCLD;
  }

  if (mBankTPCMZCLD->swap() < 0) L3secERROR(ERR_SWAP, "swap TPCMZCLD", sec);

  return mBankTPCMZCLD;
}

//----------------------------------------------------------

int * L3_Reader::getSVT_Bank(int sector)
{
  // check format version first
  if (mBankL3P->header.FormatNumber < 4) {
        L3ERROR(INFO_MISSING_BANK, "wrong data format, no SVT");
	return NULL;
  }

  // now check for the offset/length
  if (mBankL3P->svt[sector-1].length==0) {
        L3ERROR(INFO_MISSING_BANK, "no SVT bank");
        return NULL;
  }

  mSVT = (INT32 *)mBankL3P + mBankL3P->svt[sector-1].offset;

  return mSVT;
}

//----------------------------------------------------------

int * L3_Reader::getFTPC_Bank(int sector)
{
  // check format version first
  if (mBankL3P->header.FormatNumber < 4) {
        L3ERROR(INFO_MISSING_BANK, "wrong data format, no FTPC");
	return NULL;
  }

  // now check for the offset/length
  if (mBankL3P->ftpc[sector-1].length==0) {
        L3ERROR(INFO_MISSING_BANK, "no FTPC bank");
        return NULL;
  }

  mFTPC = (INT32 *)mBankL3P + mBankL3P->ftpc[sector-1].offset;

  return mFTPC;
}

//----------------------------------------------------------

int * L3_Reader::getEMC_Bank()
{
  // check format version first
  if (mBankL3P->header.FormatNumber < 4) {
        L3ERROR(INFO_MISSING_BANK, "wrong data format, no EMC");
	return NULL;
  }

  // now check for the offset/length
  if (mBankL3P->emc.length==0) {
        L3ERROR(INFO_MISSING_BANK, "no EMC bank");
        return NULL;
  }

  mEMC = (INT32 *)mBankL3P + mBankL3P->emc.offset;

  return mEMC;
}


//----------------------------------------------------------

Gl3AlgorithmReader * L3_Reader::getGl3AlgorithmReader()
{
  // only one reader per event
  if (!mAlr) {
        mAlr = new Gl3AlgorithmReader (this);
	if (!mAlr->initialize()) {
	      //cout << "ERROR: getGlobalTrackReader FAILED" << endl;
	      return NULL;
	}
  }
  return mAlr;
}


//----------------------------------------------------------

GlobalTrackReader * L3_Reader::getGlobalTrackReader ()
{
  // only one reader per event
  if (!mGtr) {
        mGtr = new GlobalTrackReader (this);
	if (!mGtr->initialize()) {
	      //cout << "ERROR: getGlobalTrackReader FAILED" << endl;
	      return NULL;
	}
  }
  return mGtr;
}

//----------------------------------------------------------

Sl3ClusterReader * L3_Reader::getSl3ClusterReader (int sec)
{
  if (!mScr) mScr = new Sl3ClusterReader (this);
  if (!mScr->initialize(sec)) {
        //cout << "ERROR: getSl3ClusterReader FAILED" << endl;
        return NULL;
  }
  return mScr;
}

//----------------------------------------------------------

Sl3TrackReader * L3_Reader::getSl3TrackReader (int sec)
{
  if (!mStr) mStr = new Sl3TrackReader (this);
  if (!mStr->initialize(sec)) {
        //cout << "ERROR: getSl3TrackReader FAILED" << endl;
        return NULL;
  }
  return mStr;
}

//----------------------------------------------------------

I960ClusterReader * L3_Reader::getI960ClusterReader (int sec)
{
  if (!mIcr) mIcr = new I960ClusterReader (this);
  if (!mIcr->initialize(sec)) {
        //cout << "ERROR: getI960ClusterReader FAILED" << endl;
	return NULL;
  }
  return mIcr;
}


//##########################################################
//--------- Gl3AlgorithmReader ------------------------------
//##########################################################

Gl3AlgorithmReader::Gl3AlgorithmReader(L3_Reader *l3r)
{
  mL3r = l3r;
  mL3SUMD = NULL;
  mAlgData = NULL;
  mNProcessed = 0;
  mNReconstructed = 0;
  mNAlg = 0;
}


int Gl3AlgorithmReader::initialize()
{
  mL3SUMD = mL3r->getL3_SUMD();
  if (mL3SUMD == NULL) {
        //cout << "no L3_SUMD found" << endl;
        return FALSE;
  }

  mNProcessed = mL3SUMD->nProcessed;
  mNReconstructed = mL3SUMD->nReconstructed;
  mNAlg = mL3SUMD->nAlg;
  mAlgData = mL3SUMD->alg;

  return TRUE;
}


//##########################################################
//--------- GlobalTrackReader ------------------------------
//##########################################################

GlobalTrackReader::GlobalTrackReader (L3_Reader *l3r)
{
  mL3 = l3r;
  mL3GTD = NULL;
  mTracks = NULL;
  mNTracks     = 0;
  mNHits       = 0;
  mGlobalVertex.x = 0;
  mGlobalVertex.y = 0;
  mGlobalVertex.z = 0;
}


int GlobalTrackReader::initialize () 
{
  mL3GTD = mL3->getL3_GTD();
  if (mL3GTD == NULL) {
        //cout << "no L3_GTD found" << endl;
	return FALSE;
  }
  
  mTracks  = mL3GTD->track;
  mNTracks = mL3GTD->nTracks;
  mNHits   = mL3GTD->nHits;

  mGlobalVertex.x = mL3GTD->xVert; // now read out as [cm]
  mGlobalVertex.y = mL3GTD->yVert;
  mGlobalVertex.z = mL3GTD->zVert;

  return TRUE;
}



//##########################################################
// ---------- Sl3ClusterReader -----------------------
//##########################################################

Sl3ClusterReader::Sl3ClusterReader (L3_Reader *l3r)
{
  mL3 = l3r;
  mSector = -1;
  mL3SECCD = NULL;
  mCluster  = NULL;
  mNCluster = 0;
}


int Sl3ClusterReader::initialize (int sec)
{
  // is this sector already initialized?
  if (mSector!=sec) {
        // set sector number
        mSector = sec;
	mL3SECCD = mL3->getL3_SECCD(mSector);
	if (mL3SECCD == NULL) {
	      //cout << "no L3_SECCD found" << endl;
	      return FALSE;
	}
	mCluster  = mL3SECCD->cluster;
	mNCluster = mL3SECCD->nrClusters_in_sector;
  }
  return TRUE;
}



//##########################################################
// ---------- I960ClusterReader --------------------
//##########################################################

I960ClusterReader::I960ClusterReader (L3_Reader *l3r)
{
  mL3 = l3r;
  mSector = -1;
  mCluster = new L3_Cluster[maxClusterPerSector];
  mNCluster = 0;
  for (int rb=0; rb<12; rb++) {
        for (int mz=0; mz<3; mz++) {
	      mBankTPCMZCLD[rb][mz] = NULL;
	}
  }
}


I960ClusterReader::~I960ClusterReader ()
{
  delete [] mCluster;
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
  if (mSector!=sec) {
        // set sector number
        mSector = sec;
	mNCluster = 0;
	// get number of clusters
	int rb;
	for (rb=1; rb<=12; rb++) {
	      for (int mz=1; mz<=3; mz++) {
		    //pointer to TPCMZCLD bank
		    mBankTPCMZCLD[rb-1][mz-1] = mL3->getTPCMZCLD(mSector, rb, mz); 
		    cld = mBankTPCMZCLD[rb-1][mz-1];
		    if (!cld) continue;
		    int *ptr = (int *)&cld->stuff;
		    // count total number of clusters for memory allocation
		    for (int ir=0; ir<cld->numberOfRows; ir++) {
		          //int row = *ptr++;
		          ptr++;
			  int nHitsThisRow = *ptr++;  // bump pointer to beginning of space points
			  mNCluster += nHitsThisRow;  // add num space pts to running total
			  ptr += 2 * nHitsThisRow;
		    }
	      }
	}
	//cout << "sector "<<sector<<": found " 
	//     <<nCluster<<" space pts" <<endl;

	if (mNCluster>maxClusterPerSector) {
	      cout << "ERROR: L3_Reader: reached maxClusterPerSector limit!" << endl;
	      return FALSE;
	}
	if (mCluster==NULL) {
	      cout << "failed to allocate cluster structures " << endl;
	      return FALSE;
	}
	// check number of clusters
	// convenient: if mNCluster==0 return FALSE
	if (mNCluster==0) {
	      cout << "no i960 clusters found" << endl;
	      return FALSE;
	}


	// copy i960 cluster into l3 cluster struct
	L3_Cluster *pcluster;
	pcluster = mCluster;

	for ( rb=1; rb<=12; rb++) {
	      for (int mz=1; mz<=3; mz++) {
		    cld = mBankTPCMZCLD[rb-1][mz-1];  // pointer to TPCMZCLD bank
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



//##########################################################
// ---------- Sl3TrackReader -----------------------
//##########################################################

Sl3TrackReader::Sl3TrackReader (L3_Reader *l3r)
{
  mL3 = l3r;
  mSector = -1;
  mL3LTD = NULL;
  mTracks  = NULL;
  mNTracks = 0;
  mNHits   = 0;
  mCpuTime = 0;
  mRealTime = 0;
  mParaSet = 0;
  mSectorVertex.x = 0;
  mSectorVertex.y = 0;
  mSectorVertex.z = 0;
}


int Sl3TrackReader::initialize (int sec)
{
  // is this sector already initialized?
  if (mSector!=sec) {
        // set sector number
        mSector = sec;
	mL3SECTP = mL3->getL3_SECTP(mSector);
	if (mL3SECTP == NULL) {
	      //cout << "no L3_SECTP found" << endl;
	      return FALSE;
	}

	// check existence of local track bank
	if (mL3SECTP->banks[0].length == 0) {
	      pL3secERROR(INFO_MISSING_BANK, "no L3_LTD bank", mSector);
	      return FALSE;
	}

	mL3LTD = (Bank_L3_LTD *) ((INT32 *)mL3SECTP + mL3SECTP->banks[0].offset);
	if (strncmp(mL3LTD->header.BankType, CHAR_L3_LTD, 8) != 0) {
	      pL3secERROR(ERR_BAD_HEADER, "bad L3_LTD header", mSector);
	      return FALSE;
	}

	if (mL3LTD->swap() < 0) pL3secERROR(ERR_SWAP, "swap L3_LTD", mSector);


	//mL3LTD->header.print();
	//printf("+++++>> L3_LTD: nTracks %i\n",
	//       (int) (mL3LTD->header.BankLength * 4 - sizeof(Bank_Header)) / sizeof(localTrack));

	mTracks  = mL3LTD->track;
	mNTracks = mL3SECTP->nTracks;
	mNHits   = mL3SECTP->nHits;
	mCpuTime = mL3SECTP->cpuTime;
	mRealTime = mL3SECTP->realTime;
	mParaSet = mL3SECTP->para;
	mSectorVertex.x = mL3SECTP->xVert;
	mSectorVertex.y = mL3SECTP->yVert;
	mSectorVertex.z = mL3SECTP->zVert;
  }
  return TRUE;
}
