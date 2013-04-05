/***************************************************************************
 *
 * $Id: StTrackFitTraits.cxx,v 2.24 2013/04/05 15:11:33 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackFitTraits.cxx,v $
 * Revision 2.24  2013/04/05 15:11:33  ullrich
 * Changes due to the addition of StTrackMassFit (Yuri)
 *
 * Revision 2.23  2013/02/16 02:19:14  perev
 * Bug fix, double counting of fitpoints
 *
 * Revision 2.22  2012/05/06 02:28:51  perev
 * Again the new logic for total numbers of fit points. bugFixed
 *
 * Revision 2.21  2012/04/29 22:49:48  fisyak
 * Back to old scheme of no. of fitted points for Sti, keep as possiblity a new scheme for Stv
 *
 * Revision 2.20  2012/04/27 01:45:07  perev
 * Logic for total numbers of fit points changed
 *
 * Revision 2.19  2011/10/05 20:59:44  perev
 * Comments++
 *
 * Revision 2.18  2009/11/23 16:34:07  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.17  2008/03/13 16:57:36  ullrich
 * Add include to comply with ROOT.
 *
 * Revision 2.16  2007/10/11 21:52:32  ullrich
 * Added member to handle number of fit points for PXL and IST.
 *
 * Revision 2.15  2004/12/02 23:35:13  ullrich
 * Added misisng setXXX functions.
 *
 * Revision 2.14  2004/08/13 18:15:42  ullrich
 * Added +1 to the number of fit points when bool flag is set.
 *
 * Revision 2.13  2004/08/12 17:22:31  fisyak
 * Switch to automatic streamer for version >4 to account new no. of fit points definition
 *
 * Revision 2.12  2004/08/10 14:20:21  calderon
 * Putting the streamers back in.  They should not be needed, but
 * apparently removing them causes more problems.  Yuri tested that
 * putting them back in allows reading files again.
 *
 * Revision 2.11  2004/08/05 22:24:32  ullrich
 * Changes to the handling of numberOfPoints() to allow ITTF more flexibility.
 *
 * Revision 2.10  2001/05/04 19:49:51  perev
 * Streamer to account old ROOT2
 *
 * Revision 2.9  2001/04/09 22:57:05  perev
 * forget STAR I/O
 *
 * Revision 2.8  2001/04/05 04:00:58  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.7  2001/03/24 03:35:00  perev
 * clone() -> clone() const
 *
 * Revision 2.6  2001/03/16 20:57:44  ullrich
 * Covariant matrix now stored in TArrayF.
 *
 * Revision 2.5  2000/02/22 23:24:08  ullrich
 * Fixed bug in covariantMatrix().
 *
 * Revision 2.4  2000/01/20 14:43:39  ullrich
 * Fixed bug in numberOfFitPoints(). Sum was wrong.
 *
 * Revision 2.3  1999/12/21 15:09:18  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 2.2  1999/11/01 12:45:14  ullrich
 * Modified unpacking of point counter
 *
 * Revision 2.1  1999/10/28 22:27:32  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:42:59  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#include <assert.h>
#include "TFile.h"
#include <algorithm>
#include "StTrackFitTraits.h"
#include "StParticleTypes.hh"
#include "StParticleTable.hh"
#include "TClass.h"
#include "TMath.h"
#if !defined(ST_NO_NAMESPACES)
using std::fill_n;
using std::copy;
#endif

ClassImp(StTrackFitTraits)

static const char rcsid[] = "$Id: StTrackFitTraits.cxx,v 2.24 2013/04/05 15:11:33 ullrich Exp $";

//_____________________________________________________________________________
StTrackFitTraits::StTrackFitTraits()
{
    mPidHypothesis = 0;
    mNumberOfFitPoints = 0x8000;
    mNumberOfFitPointsTpc = 0;
    mNumberOfFitPointsFtpcWest = 0;
    mNumberOfFitPointsFtpcEast = 0;
    mNumberOfFitPointsSvt = 0;
    mNumberOfFitPointsSsd = 0;
    mNumberOfFitPointsPxl = 0;
    mNumberOfFitPointsIst = 0;
    mPrimaryVertexUsedInFit = false;
    fill_n(mChi2, 2, 0);
}


//_____________________________________________________________________________
StTrackFitTraits::StTrackFitTraits(UShort_t pid, UShort_t nfp,
                                   Float_t chi[2], Float_t cov[15])
{
    mPidHypothesis = pid;
    mNumberOfFitPoints = 0x8000;
    copy(chi, chi+2, mChi2);
    mCovariantMatrix.Set(15, cov);
    mNumberOfFitPointsTpc = 0;
    mNumberOfFitPointsFtpcWest = 0;
    mNumberOfFitPointsFtpcEast = 0;
    mNumberOfFitPointsSvt = 0;
    mNumberOfFitPointsSsd = 0;
    mNumberOfFitPointsPxl = 0;
    mNumberOfFitPointsIst = 0;
    mPrimaryVertexUsedInFit = false;
    assert(check());
}
//_____________________________________________________________________________
StTrackFitTraits::StTrackFitTraits(UShort_t pid, UShort_t nfp,
                                   Float_t chi[2], TArrayF &cov)
{
    mPidHypothesis = pid;
    mNumberOfFitPoints = nfp|0x8000;
    copy(chi, chi+2, mChi2);
    mCovariantMatrix = cov;
    mNumberOfFitPointsTpc = 0;
    mNumberOfFitPointsFtpcWest = 0;
    mNumberOfFitPointsFtpcEast = 0;
    mNumberOfFitPointsSvt = 0;
    mNumberOfFitPointsSsd = 0;
    mNumberOfFitPointsPxl = 0;
    mNumberOfFitPointsIst = 0;
    mPrimaryVertexUsedInFit = false;
    assert(check());
}

//_____________________________________________________________________________
StTrackFitTraits::~StTrackFitTraits() {/* noop */}

//_____________________________________________________________________________
UShort_t StTrackFitTraits::numberOfFitPoints() const
{
    int result;
    
    // y2012 version
    result = numberOfFitPoints(kTpcId) 		+
    numberOfFitPoints(kFtpcWestId) 	+
    numberOfFitPoints(kFtpcEastId) 	+
    numberOfFitPoints(kSvtId)      	+
    numberOfFitPoints(kSsdId) 		+	
    numberOfFitPoints(kPxlId) 		+
    numberOfFitPoints(kIstId);	
    if (mNumberOfFitPoints&0x8000) result += (mNumberOfFitPoints&0x7FFF);
    if (mPrimaryVertexUsedInFit) result++;
    return (UShort_t)result;
}

//_____________________________________________________________________________
UShort_t StTrackFitTraits::numberOfFitPoints(StDetectorId det) const
{
    //
    // Old and obsolete
    //
    if (mNumberOfFitPoints && (mNumberOfFitPoints<0x8000)) {    
        // 1*tpc + 1000*svt + 10000*ssd (Helen/Spiros Oct 29, 1999)
        switch (det) {
            case kFtpcWestId:
            case kFtpcEastId:
            case kTpcId:
                return mNumberOfFitPoints%1000;
                break;
            case kSvtId:
                return (mNumberOfFitPoints%10000)/1000;
                break;
            case kSsdId:
                return mNumberOfFitPoints/10000;
                break;
            default:
                return 0;
        }
    }
    //
    // New version
    //
    else {
        switch (det) {
            case kFtpcWestId:
                return mNumberOfFitPointsFtpcWest;
                break;
            case kFtpcEastId:
                return mNumberOfFitPointsFtpcEast;
                break;
            case kTpcId:
                return mNumberOfFitPointsTpc;
                break;
            case kSvtId:
                return mNumberOfFitPointsSvt;
                break;
            case kSsdId:
                return mNumberOfFitPointsSsd;
                break;
            case kPxlId:
                return mNumberOfFitPointsPxl;
                break;
            case kIstId:
                return mNumberOfFitPointsIst;
                break;
            default:	//sum of all
                return (mNumberOfFitPoints&0x7FFF)
                + mNumberOfFitPointsFtpcWest
                + mNumberOfFitPointsFtpcEast
                + mNumberOfFitPointsTpc
                + mNumberOfFitPointsSvt
                + mNumberOfFitPointsSsd
                + mNumberOfFitPointsPxl
                + mNumberOfFitPointsIst;
        }
    }
}

//_____________________________________________________________________________
StParticleDefinition* StTrackFitTraits::pidHypothesis() const
{
    return StParticleTable::instance()->findParticleByGeantId(mPidHypothesis);
}

//_____________________________________________________________________________
Double_t StTrackFitTraits::chi2(UInt_t i) const
{
    if (i < 2)
        return mChi2[i];
    else
        return 0;
}

//_____________________________________________________________________________
StMatrixF StTrackFitTraits::covariantMatrix() const
{
    StMatrixF m(5,5);
#define mCovariantMatrix ((TArrayF&)mCovariantMatrix)         //temporary HACK VP
    if (mCovariantMatrix.GetSize() == 15) {
        /** StiKalmanTrackNode::getGlobalTpt, contains only covariance matrix cc[15] in tpt convension 
         returns the node information in TPT representation
         double x[6],  : state, for a definition, in radial implementation
         rad  - radius at start (cm). See also comments
         phi  - azimuthal angle  (in rad)      
         z    - z-coord. (cm)                 
         psi  - azimuthal angle of pT vector (in rads)     
         tanl - tan(dip) =pz/pt               
         q/pt -  
         double cc[15] : error matrix of the state "x" rad is fixed
         code definition adopted here, where:
         Units
         ______|________________|____________
         phi*R |  0  1  2  3  4 |  deg*cm
         z0   |  1  5  6  7  8 |    cm
         tanl  |  2  6  9 10 11 |    1         covar(i)
         psi  |  3  7 10 12 13 |   deg
         q/pt  |  4  8 11 13 14 | e*1/(GeV/c)
         -----------------------------------
         
         and where phi  = atan2(y0,x0)*(180 deg/pi)
         R    = sqrt(x0*x0 + y0*y0)
         q/pt = icharge*invpt; (This is what the 
         radius of curvature actually
         determines)
         PhiPhi PhiZ PhiTan PhiPsi PhiPt
         ZZ   ZTan   ZPsi     ZPt
         TanTan TanPsi TanPt
         PsiPsi PsiPt
         PtPt
         */
        m(1,1) = mCovariantMatrix[0];			//yy
        m(1,2) = m(2,1) = mCovariantMatrix[1];		//
        m(1,3) = m(3,1) = mCovariantMatrix[2];
        m(1,4) = m(4,1) = mCovariantMatrix[3];
        m(1,5) = m(5,1) = mCovariantMatrix[4];
        m(2,2) = mCovariantMatrix[5];			//zz
        m(2,3) = m(3,2) = mCovariantMatrix[6];
        m(2,4) = m(4,2) = mCovariantMatrix[7];
        m(2,5) = m(5,2) = mCovariantMatrix[8];
        m(3,3) = mCovariantMatrix[9];			//tanLtanL
        m(3,4) = m(4,3) = mCovariantMatrix[10];
        m(3,5) = m(5,3) = mCovariantMatrix[11];
        m(4,4) = mCovariantMatrix[12];			//PsiPsi deg
        m(4,5) = m(5,4) = mCovariantMatrix[13];		
        m(5,5) = mCovariantMatrix[14];			//PtiPti
    } 
    else if (mCovariantMatrix.GetSize() == 9) {
        /* contain variables x(tanL,Psi,Pti=-q/pT) and their cov. matrix
         tanLtanL[3] 
         tanLPsi[4]  PsiPsi[5]
         tanLPti[6]  PsiPti[7]  PtiPti[8]
         */
        Double_t rd = TMath::RadToDeg();
        m(3,3) = mCovariantMatrix[3];			//tanLtanL
        m(3,4) = m(4,3) = mCovariantMatrix[4]*rd;         //tanLPsi deg
        m(3,5) = m(5,3) = mCovariantMatrix[6];            //tanLPti
        m(4,4) = mCovariantMatrix[5]*rd*rd;		//PsiPsi  deg
        m(4,5) = m(5,4) = mCovariantMatrix[7];		//PsiPti  deg
        m(5,5) = mCovariantMatrix[8];			//PtiPti
    }
#undef mCovariantMatrix                                 //temporary HACK VP
    return m;
}
//_____________________________________________________________________________
Bool_t StTrackFitTraits::check() const {
    if (mCovariantMatrix.GetSize() == 15) 
        return (mCovariantMatrix[0]  > 0 &&
                mCovariantMatrix[5]  > 0 &&
                mCovariantMatrix[9]  > 0 &&
                mCovariantMatrix[12] > 0 &&
                mCovariantMatrix[14] > 0);
    if (mCovariantMatrix.GetSize() == 9) 
        return (mCovariantMatrix[3] > 0 &&
                mCovariantMatrix[5] > 0 &&
                mCovariantMatrix[8] > 0);
    return (mCovariantMatrix.GetSize() == 0);
}
//_____________________________________________________________________________
bool StTrackFitTraits::primaryVertexUsedInFit() const
{ return mPrimaryVertexUsedInFit;}

//_____________________________________________________________________________
void StTrackFitTraits::clearCovariantMatrix() {mCovariantMatrix.Set(0);}

//_____________________________________________________________________________
void StTrackFitTraits::setNumberOfFitPoints(unsigned char val, StDetectorId det)
{
    mNumberOfFitPoints|=  0x8000;  // make sure old method is NOT active
    switch (det) {
        case kUnknownId:
            break;
        case kFtpcWestId:
            mNumberOfFitPointsFtpcWest = val;
            break;
        case kFtpcEastId:
            mNumberOfFitPointsFtpcEast = val;
            break;
        case kTpcId:
            mNumberOfFitPointsTpc = val;
            break;
        case kSvtId:
            mNumberOfFitPointsSvt = val;
            break;
        case kSsdId:
            mNumberOfFitPointsSsd = val;
            break;
        case kPxlId:
            mNumberOfFitPointsPxl = val;
            break;
        case kIstId:
            mNumberOfFitPointsIst = val;
            break;
        default:
            mNumberOfFitPoints += val; mNumberOfFitPoints|=0x8000;
            break;
    }
}

//_____________________________________________________________________________
void StTrackFitTraits::setPrimaryVertexUsedInFit(bool val)
{mPrimaryVertexUsedInFit = val;}

//_____________________________________________________________________________
void StTrackFitTraits::setPidHypothesis(UShort_t val)
{
    mPidHypothesis = val;
}

//_____________________________________________________________________________
void StTrackFitTraits::setChi2(Float_t val, UInt_t i)
{
    if (i<2) mChi2[i] = val;
}

//_____________________________________________________________________________
void StTrackFitTraits::setCovariantMatrix(Float_t val[15])
{
    mCovariantMatrix.Set(15, val);
    assert(check());
}
//_____________________________________________________________________________
void StTrackFitTraits::setCovariantMatrix(TArrayF &cov)
{
    mCovariantMatrix = cov;
    assert(check());
}

//_____________________________________________________________________________
void StTrackFitTraits::Streamer(TBuffer &R__b)
{
    //        Stream an object of class StTrackFitTraits.
    
    if (R__b.IsReading()) {
        UInt_t R__s, R__c;
        Version_t R__v = R__b.ReadVersion(&R__s, &R__c);
        if (R__v > 4) {
            Class()->ReadBuffer(R__b, this, R__v, R__s, R__c);
            return;
        }
        StObject::Streamer(R__b);
        
        R__b >> (UShort_t&)mPidHypothesis;
        R__b >> (UShort_t&)mNumberOfFitPoints;
        
        if (R__v==2 && gFile && gFile->GetVersion()%100000<30000)
        { Int_t dumy; R__b >> dumy;}
        
        R__b.ReadFastArray(mChi2,2);
        mCovariantMatrix.Streamer(R__b);
        
    } 
    else {
        Class()->WriteBuffer(R__b,this);
    }
}
//________________________________________________________________________________
StThreeVectorF StTrackFitTraits::momentum() {
    StThreeVectorF mom;
    if (mCovariantMatrix.GetSize() == 9) {
        if (mCovariantMatrix[2]) {
            Float_t pT = 1./TMath::Abs(mCovariantMatrix[2]);
            Float_t tanL = mCovariantMatrix[0];
            Float_t phi =  mCovariantMatrix[1];
            mom = StThreeVectorF(pT*TMath::Cos(phi),pT*TMath::Sin(phi),pT*tanL);
        }
    }
    return mom;
}//________________________________________________________________________________
StThreeVectorF StTrackFitTraits::momentumErrors() {
    StMatrixF Cxyz = momentumCovariance();
    return StThreeVectorF(Cxyz(0,0) > 0 ? TMath::Sqrt(Cxyz(0,0)) : 13,
                          Cxyz(1,1) > 0 ? TMath::Sqrt(Cxyz(1,1)) : 13,
                          Cxyz(2,2) > 0 ? TMath::Sqrt(Cxyz(2,2)) : 13);
}//________________________________________________________________________________
StMatrixF StTrackFitTraits::momentumCovariance() {
    StMatrixF Cxyz(3,3);
    if (mCovariantMatrix[2]) {
        StMatrixF C(3,3);
        C(0,0) =          mCovariantMatrix[3]; 
        C(0,1) = C(1,0) = mCovariantMatrix[4]; 
        C(1,1) =          mCovariantMatrix[5];
        C(0,2) = C(2,0) = mCovariantMatrix[6]; 
        C(1,2) = C(2,1) = mCovariantMatrix[7]; 
        C(2,2) =          mCovariantMatrix[8]; 
        StThreeVectorF P = momentum();
        Float_t pT = 1./TMath::Abs(mCovariantMatrix[2]);
        static Float_t One = 1.;
        Float_t pTs = -pT*TMath::Sign(One,mCovariantMatrix[2]);
        StMatrixF F(3,3);
        //     tanL               Psi                 Pti
        F(0,0) =  0;  F(0,1) =- P.y(); F(0,2) = pTs*P.x(); // p_x
        F(1,0) =  0;  F(1,1) =  P.x(); F(1,2) = pTs*P.y();
        F(2,0) = pT;  F(2,1) =      0; F(2,2) = pTs*P.z();
        StMatrixF FT = F.T();
        Cxyz = FT*C*F;
    }
    return Cxyz;
}
