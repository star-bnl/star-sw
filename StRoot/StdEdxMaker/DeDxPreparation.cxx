// DeDxPreparation.cxx
// M.L. Miller
// 6/00

#include <cmath>
#include "TreeEntryClasses.h"
#include "TpcMapUtilities.h"
#include "DeDxPreparation.h"
#include "TMinuit.h"
// StEvent
#include "StEventTypes.h"
#include "StThreeVectorD.hh"
// StDb
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"
#include "StDbUtilities/StTpcPadCoordinate.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StTpcDb/StTpcDb.h"
//#include "StTpcDb/StTpcDbMaker.h"
#include "StTpcDb/StTpcPadPlaneI.h"
// STL
#include <algorithm>
using std::sort;
#include "fee_map.h"
//#include "CorrectionTable.h"
#include "CorrectionTable2.h"
#include "PadCorrTable.h"
TF1 *fFunc = 0;
TH1 *h1 = 0;
TCanvas  *fCanvas  = 0;
Float_t dEdx[45],dx[45];
Int_t nflag0points = 45;

TMinuit *gMinuit = new TMinuit(2);
const Int_t NpGaus = 12;
Double_t parGaus[NpGaus] = {0.486236,-0.204430,0.460330,
			    0.016537,1.642435,1.085395,
			    0.198163,0.102950,0.790066,
			    0.000863,-2.143906,0.698302};
Double_t perGaus[NpGaus] = {0.001589,0.000449,0.000624,
			    0.000382,0.028459,0.007643,
			    0.001293,0.003442,0.002275,
			    0.000042,0.051532,0.020009};
extern void fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *u, Int_t flag);
extern    Double_t fGaus(Double_t x);
extern    Double_t gGaus(Double_t x);
 
static DeDxPreparation *gPrep = 0;
//constructor-------------------------------------------
DeDxPreparation::DeDxPreparation()
{
  gPrep = this;
    buildMaps();
    clear();
}

DeDxPreparation::DeDxPreparation(StTrack* tck, int tnum, int minprow, int maxprow)
    : TpcHitVecUtilities(tck, tnum)
{
  gPrep = this;
    m_FillTuple = false;
    m_minPadRow = minprow;
    m_maxPadRow = maxprow;
    buildMaps();
    dxNorm();
}

DeDxPreparation::~DeDxPreparation()
{
}

void DeDxPreparation::buildMaps()
{
    //Build map with an outward pointing normal keyed by sector number
    for (int sector=1; sector<=24; sector++) {
	m_SectorNormalMap[sector] = sectorNormal(sector);
    }

    //Build map with a point on outside, center, and inside of padrow (this plus normal define a plane)
    StTpcCoordinateTransform transformer(gStTpcDb);
    for (int sector=1; sector<=24; sector++) {
	
	for (int padrow=1; padrow<=45; padrow++) {
	    double padlength;
	    if (padrow<14) {
		padlength = gStTpcDb->PadPlaneGeometry()->innerSectorPadLength();}
	    else {
		padlength = gStTpcDb->PadPlaneGeometry()->outerSectorPadLength();}
	    
	    //Get the position of the padrow center, transform to local sector coordinates
	    StTpcPadCoordinate padCoord(sector, padrow, 1, 1);
	    StTpcLocalSectorCoordinate lsMidCoord;
	    transformer(padCoord, lsMidCoord);

	    //Boost the local y value by +- padlength / 2.
	    const StThreeVector<double>& lsPos = lsMidCoord.position();
	    StTpcLocalSectorCoordinate lsTopCoord(lsPos.x(),
						  lsPos.y()+padlength/2.,
						  lsPos.z(),
						  sector);
	    StTpcLocalSectorCoordinate lsBotCoord(lsPos.x(),
						  lsPos.y()-padlength/2.,
						  lsPos.z(),
						  sector);
	    //Transform back to global coordinates
	    StGlobalCoordinate gBotCoord, gMidCoord, gTopCoord;
	    transformer(lsTopCoord, gTopCoord);
	    transformer(lsBotCoord, gBotCoord);
	    transformer(lsMidCoord, gMidCoord);
	    const StThreeVector<double>& gTopPos = gTopCoord.position();
	    const StThreeVector<double>& gMidPos = gMidCoord.position();
	    const StThreeVector<double>& gBotPos = gBotCoord.position();
	    const StThreeVectorD gTopPosD(gTopPos.x(),gTopPos.y(),gTopPos.z());
	    const StThreeVectorD gMidPosD(gMidPos.x(),gMidPos.y(),gMidPos.z());
	    const StThreeVectorD gBotPosD(gBotPos.x(),gBotPos.y(),gBotPos.z());

	    //Store in this form
	    PadrowLocation padLocation(gTopPosD, gMidPosD, gBotPosD);
	    HitMapKey myKey;
	    myKey.sector = sector;
	    myKey.padrow = padrow;
	    m_PadrowMap.insert(padrowMapValType(myKey,padLocation));
	    //cout <<sector<<"\t"<<padrow<<"\t"<<endl;
	    //padLocation.print();
	}
    }

    cout<<"Done Building Maps"<<endl;
    return;
}

//Normalize by the pathlength, fill charge vector
void DeDxPreparation::dxNorm(){
  for (vector<StTpcHit*>::iterator it = m_tpcHitVec.begin(); it != m_tpcHitVec.end(); it++) {
    //Cut on flag values
    if (keepHit(*it)) {
      double ds = findPathLength(*it);
      if (ds != 0.) {
	if (m_FillTuple && !fillPerHitTuple((*it),ds)) {
	  m_normChargeVec.push_back((*it)->charge() / ds );
	}
      }
    }
    sort (m_normChargeVec.begin(), m_normChargeVec.end());
    findAvgZ();
  }
}
void DeDxPreparation::findAvgZ()
{
    if (m_normChargeVec.size() != 0) {
	double N = static_cast<double>(m_normChargeVec.size());
	double sumz = 0.;
	double sumzsqd = 0.;
	for (vector<double>::const_iterator it=m_normChargeVec.begin(); it!=m_normChargeVec.end(); it++) {
	    sumz = sumz + log(*it);
	    sumzsqd = sumzsqd + (log(*it) * log(*it) );
	}
	m_AvgZ = sumz/N;
	m_AvgZSquared = sumzsqd/N;
    }
    else {
	m_AvgZ=-1.;
	m_AvgZSquared=-1.;
    }
    return;
}
//Hit Filter
bool DeDxPreparation::keepHit(StTpcHit* tpcHit)
{
    if (!tpcHit->flag() && (tpcHit->padrow() >= m_minPadRow) && (tpcHit->padrow() <= m_maxPadRow) ) {return true;}
    else {return false;}

}
//Outward pointing normal of the sector
StThreeVectorD DeDxPreparation::sectorNormal(int sector)
{
    int numSectors = gStTpcDb->Dimensions()->numberOfSectors();
    double beta = (sector > 12) ?(numSectors-sector)*2.*M_PI/(static_cast<double>(numSectors)/2.): sector*2.*M_PI/(static_cast<double>(numSectors)/2.);
    StThreeVectorD vec(sin(beta), cos(beta), 0.);
    return vec;
}

//Caluclate Pathlength using StHelixD
double DeDxPreparation::findPathLength(StTpcHit* tpcHit)
{
    double ds=0.;
    HitMapKey mykey;    //Build a key to the map (sector, padrow)
    mykey.sector = tpcHit->sector();
    mykey.padrow = tpcHit->padrow();
    PadrowLocation padLoc = m_PadrowMap[mykey];
    const StThreeVectorD normal = m_SectorNormalMap[tpcHit->sector()];
    double s_out = m_StTrack->geometry()->helix().pathLength(padLoc.outsidePoint(), normal);
    double s_in  = m_StTrack->geometry()->helix().pathLength(padLoc.insidePoint(), normal);
    ds = s_out-s_in;
    if (ds < 0.) {ds = -1.*ds;}
    if (s_out==DBL_MAX || s_in==DBL_MAX) {ds = 0.;}
    return ds;
}

//this is a temporary, bad place to do this
Int_t DeDxPreparation::fillPerHitTuple(StTpcHit* hit, double ds)
{
    double cangle = crossingAngle(hit);
    HitEntry hitEntry(hit);
   
    hitEntry.setDx(ds);
    // Find Pad
    Float_t xlocal;
    
    StGlobalCoordinate global(hitEntry.m_xhit,hitEntry.m_yhit,hitEntry.m_zhit);
    StTpcLocalSectorCoordinate localSector;
    StTpcPadCoordinate pad;
    StTpcCoordinateTransform transform(gStTpcDb);
    transform(global,localSector); 
    transform(global,pad);
    //    *isect = pad.sector();
    xlocal = localSector.position().x(); 
    hitEntry.m_pad =
      (int)( -xlocal/gStTpcDb->PadPlaneGeometry()->PadPitchAtRow(hitEntry.m_padrow) + 0.5 + 
      0.5*gStTpcDb->PadPlaneGeometry()->numberOfPadsAtRow(hitEntry.m_padrow));
    hitEntry.m_fee = -1;
    hitEntry.m_rdo = -1;
    for (int iFee=0;iFee<noFee;iFee++) {
      for (int pin=0;pin<noPin;pin++) {
	if (row_vs_fee[iFee][pin] == hitEntry.m_padrow &&
	    pad_vs_fee[iFee][pin] == hitEntry.m_pad) {
	  hitEntry.m_fee = iFee;
	  hitEntry.m_pin = pin;
          hitEntry.m_rdo = rdo_vs_fee[iFee][pin];
	  break;
	}
	if (hitEntry.m_fee != -1) break;
      }
    }
//     Int_t fee2 = 2*hitEntry.m_fee + hitEntry.m_padrow%2;
//     if (correction[fee2][hitEntry.m_sector-1]<0) return -1;
//     hitEntry.m_de /= correction[fee2][hitEntry.m_sector-1];
    if (!CorrCoef[hitEntry.m_sector-1][hitEntry.m_padrow-1][hitEntry.m_pad-1]) 
      printf("Correction[%i][%i][%i] ---> == 0\n",
	     hitEntry.m_sector,hitEntry.m_padrow,hitEntry.m_pad);
    else hitEntry.m_de *= 
	   CorrCoef[hitEntry.m_sector-1][hitEntry.m_padrow-1][hitEntry.m_pad-1];
    
    hitEntry.setCrossingAngle(cangle);
    m_HitEntryVec.push_back(hitEntry);
    
    return 0;
}

//Calculate Crossing Angle
double DeDxPreparation::crossingAngle(StTpcHit* hit)
{
    const StThreeVectorD normal = m_SectorNormalMap[hit->sector()];

    //Get the pathlength at the middle of the pad, a big mess because of no template support
    const StGlobalCoordinate& gMidCoord(hit->position());
    const StThreeVector<double>& gMidPos = gMidCoord.position();
    const StThreeVectorD gMidPosD(gMidPos.x(),gMidPos.y(),gMidPos.z());

    const double pathlength = m_StTrack->geometry()->helix().pathLength(gMidPosD);
    const StThreeVectorD momentum = m_StTrack->geometry()->helix().momentumAt(pathlength, m_BField);
    const StThreeVectorD xymomentum(momentum.x(), momentum.y(), 0.);
    const double crossingAngle = normal.angle(xymomentum);

    return 180./3.14159*crossingAngle;
}

//Accessors-----------------------------------------------------------------
void DeDxPreparation::clearAll()
{
    clear();
    TpcHitVecUtilities::clear();  //clear the base class, too
    return;
}

//Clear DeDxPreparation info, save base class info
void DeDxPreparation::clear()
{
    m_HitEntryVec.clear();
    m_normChargeVec.clear();
    m_minPadRow=0;
    m_maxPadRow=45;
    m_AvgZ = 0.;
    m_AvgZSquared = 0.;
    m_fitZ = 0.;
    m_fitdZ = 0.;
    m_fitS = 0.;
    m_fitdS = 0.;
    m_BField =0.;
    m_debug = false;
    m_FillTuple = false;
    return;
}

void DeDxPreparation::setFillTuple(bool val)
{
    m_FillTuple = val;
    return;
}

void DeDxPreparation::setMinPadrow(int prow)
{
    m_minPadRow = prow;
    return;
}

void DeDxPreparation::setMaxPadrow(int prow)
{
    m_maxPadRow = prow;
    return;
}

//The number of hits that passed the keepHit() cut
int DeDxPreparation::numberOfGoodHits() const
{
    return m_normChargeVec.size();
}

const vector<HitEntry>& DeDxPreparation::hitEntryVec() const
{
    return m_HitEntryVec;
}

const vector<double>& DeDxPreparation::normChargeVec() const
{
    return m_normChargeVec;
}

//STL Utitlies------------------------------
void DeDxPreparation::printHitEntryVec()
{
    for (vector<HitEntry>::const_iterator it = m_HitEntryVec.begin(); it != m_HitEntryVec.end(); it++) {
	(*it).print();
    }
    return;
}

void DeDxPreparation::printNormChargeVec()
{
    for (vector<double>::const_iterator it = m_normChargeVec.begin(); it != m_normChargeVec.end(); it++) {
	cout<<*it<<endl;
    }
    return;
}
//________________________________________________________________________________
void DeDxPreparation::doFit()
{
    Int_t N = m_HitEntryVec.size();
    if (N != 0) {
	for (vector<HitEntry>::const_iterator it=m_HitEntryVec.begin(); 
	     it!=m_HitEntryVec.end(); it++) {
	  cout << "\tx\t" << (*it).m_xhit  
	       << "\ty\t" << (*it).m_yhit  
	       << "\tz\t" << (*it).m_zhit  
	       << "\tpadrow\t" << (*it).m_padrow  
	       << "\tsector\t" << (*it).m_sector  	
	       << "\tde\t" << (*it).m_de  
	       << "\tdx\t" << (*it).m_dx  
	       << "\tcrossingangle\t" << (*it).m_crossingangle << endl;  
	}
    }
    return;
}
//________________________________________________________________________________
Double_t fGaus(Double_t x) {
  Double_t val = 0;
  for (Int_t j=0; j<NpGaus; j+=3){
    Double_t dev = (x - parGaus[j+1])/parGaus[j+2];
    //      printf ("dev = %f\n",dev);
    val += parGaus[j]*exp(-0.5*dev*dev);
  }
  return val;
}
//________________________________________________________________________________
Double_t gGaus(Double_t x) {//derivatives
  Double_t val = 0;
  for (Int_t j=0; j<NpGaus; j+=3){
    Double_t dev = (x - parGaus[j+1])/parGaus[j+2];
    //      printf ("dev = %f\n",dev);
    val += parGaus[j]*exp(-0.5*dev*dev)*(-dev/parGaus[j+2]);
  }
  return val;
}
//________________________________________________________________________________
void DeDxPreparation::plotResults()
{
  if (fCanvas) {
#if 0
    Double_t m, dm, s, ds;
    gMinuit->GetParameter(0, m, dm);
    //    gMinuit->GetParameter(1, s, ds);
    Int_t nBin = 20;
    Double_t minBin = -5, maxBin = 5;
    if (h1) delete h1;
    h1 = new TH1F("h1","de/dx of hit dist", nBin, minBin, maxBin);
    for (int i=0; i<nflag0points; i++) {h1->Fill(((dEdx[i]-m)*sqrt(dx[i]))/s);}
    h1->SetNormFactor(2.);
    
    fCanvas->cd();
    h1->Draw();
//     if (fLine) delete fLine;
//     fLine = new TLine(mean, 0., mean, nflag0points/2.);
//     fLine->SetLineColor(2);
//     fLine->SetLineStyle(2);
//     fLine->Draw("same");
    if (!fFunc) {
      //      for (int k=0;k<NpGaus;k++) printf ("params[%i] = %f\n",k,parGaus[k]);
      fFunc = new TF1("g3us","gaus(0)+gaus(3)+gaus(6)+gaus(9)",minBin, maxBin);
      fFunc->SetParameters(parGaus);
    }
    fFunc->Draw("same");
    fCanvas->Update();
    
//     cout <<"Enter any integer to continue"<<endl;
//     int temp;
//     cin>>temp;
#endif
  }    
  return;
}
//________________________________________________________________________________
void fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag)
{
  f = 0.;
  gin[0] = 0.;
  gin[1] = 0.;
  for (int i=0;i<nflag0points; i++) {
    //    Double_t xx = (dEdx[i]-par[0])/par[1]*sqrt(dx[i]);
    Double_t xx = (dEdx[i]-par[0])*sqrt(dx[i]*par[1]);
    Double_t ff = fGaus(xx);
    Double_t sc = sqrt(par[1]*dx[i]);
    Double_t val = ff*sc;
    f -= log( val );
    Double_t gg = gGaus(xx);
    gin[0] += gg/ff*sc;
    gin[1] -= 0.5/par[1]*(xx*gg/ff+1.);
    //    printf ("pars %f %f xx = %f f=%f\n",par[0],par[1],xx,f);
  }
  //   printf ("par = %f %f iflag = %i  f = %f gin = %f\n",par[0],par[1],iflag,f,gin[0]);
  if (iflag == 3) { // terminating entry 
#if 0
    plotResults();
#endif
  }
}
//________________________________________________________________________________
void DeDxPreparation::DoFitZ()
{
  nflag0points = 0;
  m_TpcLength = 0;
  m_TrackZ = 0;
  
  for (vector<HitEntry>::const_iterator it=m_HitEntryVec.begin(); 
       it!=m_HitEntryVec.end(); it++) {
    if ((*it).m_dx <= 0.0 || (*it).m_dx > 10.0) continue;
    if ((*it).m_de <= 0.0 || (*it).m_de > 1.e-4) continue;
    dx[nflag0points] = (*it).m_dx;
    dEdx[nflag0points] = log(1.e6*(*it).m_de/dx[nflag0points]);
    m_TpcLength += dx[nflag0points];
    m_TrackZ += (*it).m_zhit;
    //    printf("%2i dEdx %f dx %f\n",nflag0points,dEdx[nflag0points],dx[nflag0points]);
    nflag0points++;
  }
  if (nflag0points>0) m_TrackZ /= nflag0points;
  Double_t avz = avgZ()+log(1.e6);
  if (nflag0points>10) {
    Double_t arglist[10];
    Int_t ierflg = 0;
    gMinuit->SetFCN(fcn);
    //    gMinuit->SetPrintLevel(-1);
    arglist[0] = -1;
    gMinuit->mnexcm("set print",arglist, 1, ierflg);
    gMinuit->mnexcm("set NOW",arglist, 0, ierflg);
    gMinuit->mnexcm("CLEAR",arglist, 0, ierflg);
    arglist[0] = 0.5;
    gMinuit->mnexcm("SET ERR", arglist ,1,ierflg);
    gMinuit->mnparm(0, "mean", avz, 0.01,0.,0.,ierflg); //First Guess
    gMinuit->mnparm(1, "scale", 1.0, 0.01, 0.2, 20.0, ierflg);
    arglist[0] = 2;
    gMinuit->mnexcm("FIX",arglist,1,ierflg);
    
    arglist[0] = 1.;  
    gMinuit->mnexcm("SET GRAD",arglist,1,ierflg);
    //    gMinuit->mnexcm("SET STRAT",arglist,1,ierflg);
    arglist[0] = 500;
    arglist[1] = 1.;
    gMinuit->mnexcm("MIGRAD", arglist ,2,ierflg);
    //    gMinuit->mnexcm("HESSE  ",arglist,0,ierflg);
    //    gMinuit->mnexcm("end",arglist,0,ierflg);
// Print results
    Double_t edm,errdef;
    Int_t nvpar,nparx,icstat;
    gMinuit->mnstat(m_ValFitted,edm,errdef,nvpar,nparx,icstat);
    //    gMinuit->mnprin(3,m_ValFitted);
    
    //Print Results to screen
    gMinuit->GetParameter(0, m_fitZ, m_fitdZ);
    //cout <<"mean:\t\t"<<m_fitZ<<"\t +- "<<m_fitdZ<<endl;
    gMinuit->GetParameter(1, m_fitS, m_fitdS);
    //cout <<"scale:\t\t"<<m_fitS<<"\t +- "<<m_fitdS<<endl;
  }
  else {
    m_fitZ = m_fitdZ = m_ValFitted = m_fitS = m_fitdS = -999.;
  }
  //  cout <<"\narithmeticMean:\t"<< avz <<endl;
}
