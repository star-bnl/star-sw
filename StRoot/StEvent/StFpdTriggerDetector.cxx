/***************************************************************************
 *
 * $Id: StFpdTriggerDetector.cxx,v 2.2 2004/08/04 17:26:07 ullrich Exp $
 *
 * Author: Akio Ogawa, Jul 2004
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFpdTriggerDetector.cxx,v $
 * Revision 2.2  2004/08/04 17:26:07  ullrich
 * Fixed bug in assignement operator.
 *
 * Revision 2.1  2004/08/03 17:20:33  ullrich
 * Initial Revision.
 *
 **************************************************************************/

#include <algorithm>
#include "StFpdTriggerDetector.h"
#include "tables/St_dst_TrgDet_Table.h"
#include "StTriggerData.h"

static const char rcsid[] = "$Id: StFpdTriggerDetector.cxx,v 2.2 2004/08/04 17:26:07 ullrich Exp $";

ClassImp(StFpdTriggerDetector)

StFpdTriggerDetector::StFpdTriggerDetector()
{
    clear();
    init();
}

StFpdTriggerDetector::StFpdTriggerDetector(const dst_TrgDet_st& t)
{
    clear();
    init();
}

StFpdTriggerDetector::StFpdTriggerDetector(const StTriggerData& t)
{  
    init();
    for(int ew=0; ew<2; ew++) {
	for(unsigned int nstbps=0; nstbps<mMaxModule; nstbps++) {
	    for(unsigned int tower=0; tower<mMaxTower[nstbps]; tower++) 
		mAdc[ew][nstbps][tower]=t.fpd((StBeamDirection)ew,nstbps,tower);
	    for(int bd=0; bd<mMaxBoard; bd++) 
		mLayer1[ew][nstbps][bd]=t.fpdLayer1DSM((StBeamDirection)ew,nstbps,bd);
	    mLayer2[ew][nstbps]=t.fpdLayer2DSM((StBeamDirection)ew,nstbps);
	}
    }
}

StFpdTriggerDetector::StFpdTriggerDetector(const StFpdTriggerDetector& det)
{
    init();
    copy(det.mEN, det.mEN+mMaxNS, mEN);
    copy(det.mES, det.mES+mMaxNS, mES);
    copy(det.mET, det.mET+mMaxTB, mET);
    copy(det.mEB, det.mEB+mMaxTB, mEB);
    copy(det.mEPN, det.mEPN+mMaxPS, mEPN);
    copy(det.mEPS, det.mEPS+mMaxPS, mEPS);
    copy(det.mWN, det.mWN+mMaxNS, mWN);
    copy(det.mWS, det.mWS+mMaxNS, mWS);
    copy(det.mWT, det.mWT+mMaxTB, mWT);
    copy(det.mWB, det.mWB+mMaxTB, mWB);
    copy(det.mWPN, det.mWPN+mMaxPS, mWPN);
    copy(det.mWPS, det.mWPS+mMaxPS, mWPS);
    for (int i=0; i<2; i++)
	for (int j; j<mMaxModule; j++) {
	    for (int k=0; k<mMaxBoard; k++) mLayer1[i][j][k] = det.mLayer1[i][j][k] ;
	    mLayer2[i][j] = det.mLayer2[i][j];
	}
}

StFpdTriggerDetector&
StFpdTriggerDetector::operator=(const StFpdTriggerDetector& det)
{
    if (this != &det) {
	init();
	copy(det.mEN, det.mEN+mMaxNS, mEN);
	copy(det.mES, det.mES+mMaxNS, mES);
	copy(det.mET, det.mET+mMaxTB, mET);
	copy(det.mEB, det.mEB+mMaxTB, mEB);
	copy(det.mEPN, det.mEPN+mMaxPS, mEPN);
	copy(det.mEPS, det.mEPS+mMaxPS, mEPS);
	copy(det.mWN, det.mWN+mMaxNS, mWN);
	copy(det.mWS, det.mWS+mMaxNS, mWS);
	copy(det.mWT, det.mWT+mMaxTB, mWT);
	copy(det.mWB, det.mWB+mMaxTB, mWB);
	copy(det.mWPN, det.mWPN+mMaxPS, mWPN);
	copy(det.mWPS, det.mWPS+mMaxPS, mWPS);
	for (int i=0; i<2; i++)
	    for (int j; j<mMaxModule; j++) {
		for (int k=0; k<mMaxBoard; k++) mLayer1[i][j][k] = det.mLayer1[i][j][k] ;
		mLayer2[i][j] = det.mLayer2[i][j];
	    }
    }
    return *this;
}

StFpdTriggerDetector::~StFpdTriggerDetector() {/* noop */}

void 
StFpdTriggerDetector::init()
{
    mMaxTower[0]=mMaxNS; mMaxTower[1]=mMaxNS; 
    mMaxTower[2]=mMaxTB; mMaxTower[3]=mMaxTB; 
    mMaxTower[4]=mMaxPS; mMaxTower[5]=mMaxPS; 
    mAdc[0][0]=mEN;mAdc[0][1]=mES;mAdc[0][2]=mET;mAdc[0][3]=mEB;
    mAdc[0][4]=mEPN;mAdc[0][5]=mEPS;
    mAdc[1][0]=mWN;mAdc[1][1]=mWS;mAdc[1][2]=mWT;mAdc[1][3]=mWB;
    mAdc[1][4]=mWPN;mAdc[1][5]=mWPS;
}

void 
StFpdTriggerDetector::clear()
{
    fill_n(mEN, static_cast<size_t>(mMaxNS), 0);
    fill_n(mES, static_cast<size_t>(mMaxNS), 0);
    fill_n(mET, static_cast<size_t>(mMaxTB), 0);
    fill_n(mEB, static_cast<size_t>(mMaxTB), 0);
    fill_n(mEPN,static_cast<size_t>(mMaxPS), 0);
    fill_n(mEPS,static_cast<size_t>(mMaxPS), 0);
    fill_n(mWN, static_cast<size_t>(mMaxNS), 0);
    fill_n(mWS, static_cast<size_t>(mMaxNS), 0);
    fill_n(mWT, static_cast<size_t>(mMaxTB), 0);
    fill_n(mWB, static_cast<size_t>(mMaxTB), 0);
    fill_n(mWPN, static_cast<size_t>(mMaxPS), 0);
    fill_n(mWPS, static_cast<size_t>(mMaxPS), 0);
    for (int i=0; i<2; i++)
	for (int j; j<mMaxModule; j++) {
	    for (int k=0; k<mMaxBoard; k++) mLayer1[i][j][k] = 0;
	    mLayer2[i][j]=0;
	}
}

unsigned int
StFpdTriggerDetector::numberOfTowers(unsigned int nstbps) const
{
  if (nstbps<mMaxModule)
      return mMaxTower[nstbps];
  else
      return 0;
}

unsigned int
StFpdTriggerDetector::numberOfModules() const {return mMaxModule;}

unsigned int
StFpdTriggerDetector::numberOfLayer1Boards() const {return mMaxBoard;}

unsigned int
StFpdTriggerDetector::adc(StBeamDirection ew, unsigned int nstbps, unsigned int tower) const
{
    if (nstbps<mMaxModule) {  
	if (tower < mMaxTower[nstbps])
	    return static_cast<unsigned int>(mAdc[ew][nstbps][tower]);
    }
    return 0;
}

unsigned int
StFpdTriggerDetector::layer1(StBeamDirection ew, unsigned int nstbps, unsigned int board) const
{
    if (nstbps<mMaxModule && board<mMaxBoard)
	return static_cast<unsigned int>(mLayer1[ew][nstbps][board]);
    return 0;
}

unsigned int
StFpdTriggerDetector::layer2(StBeamDirection ew, unsigned int nstbps) const
{
    if (nstbps<mMaxModule)
	return static_cast<unsigned int>(mLayer2[ew][nstbps]);
    return 0;
}

void
StFpdTriggerDetector::setAdc(StBeamDirection ew, unsigned int nstbps, unsigned int tower, unsigned char v)
{
    if (nstbps<mMaxModule)
	if (tower < mMaxTower[nstbps])
	    mAdc[ew][nstbps][tower]=v;
}

void
StFpdTriggerDetector::setLayer1(StBeamDirection ew, unsigned int nstbps, unsigned int board, unsigned short v) 
{
    if (nstbps<mMaxModule && board<mMaxBoard)
	mLayer1[ew][nstbps][board]=v;
}

void
StFpdTriggerDetector::setLayer2(StBeamDirection ew, unsigned int nstbps, unsigned short v)
{
    if (nstbps<mMaxModule)
	mLayer2[ew][nstbps]=v;
}

void 
StFpdTriggerDetector::dump() const
{
    printf("StFpdTriggerDetector::dump()\n");
    printf("numberOfModules()=%d numberOfLayer1Boards()=%d\n",numberOfModules(),numberOfLayer1Boards());
    for(int ew=0; ew<2; ew++){
	if(ew==0) printf("East\n");
	else      printf("West\n");    
	for(unsigned int nstbps=0; nstbps<numberOfModules(); nstbps++){
	    printf("Module=%d numberOfTowers=%d\n",nstbps,numberOfTowers(nstbps));
	    for(unsigned int tower=0; tower<numberOfTowers(nstbps); tower++){
		printf("%4d ",adc((StBeamDirection)ew,nstbps,tower));
		if(numberOfTowers(nstbps)==49 && tower%7==6) printf("\n");
		if(numberOfTowers(nstbps)==25 && tower%5==4) printf("\n");
		if(numberOfTowers(nstbps)==7  && tower%7==6) printf("\n");
	    }
	    printf("Layer1=");
	    for(unsigned int bd=0; bd<numberOfLayer1Boards(); bd++){
		printf("%4d ",layer1((StBeamDirection)ew,nstbps,bd));
	    }
	    printf("\nLayer2=%d\n",layer2((StBeamDirection)ew,nstbps));
	}
    }
}	   
