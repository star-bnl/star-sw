// $Id: StLaserEventMaker.cxx,v 1.38 2007/10/16 15:28:42 fisyak Exp $
// $Log: StLaserEventMaker.cxx,v $
// Revision 1.38  2007/10/16 15:28:42  fisyak
// Make laser tree splitted
//
// Revision 1.37  2007/05/29 22:22:24  fine
// Introduce logger-based output
//
// Revision 1.36  2007/05/24 20:35:47  jeromel
// Fixes: m_clock unu-initialized, Float_t m_clockNominal replaced class DM,
// naming changed to l_clockNominal.
//
// Revision 1.35  2007/04/17 05:08:18  perev
// GetTFile()==>StMaker. Jerome request
//
// Revision 1.34  2005/02/03 16:17:28  jecc
// Correct for laser travel time from first to last mirror.
// Select narrow range around laser z peak position to determine <z>.
// Fix very old typo in histos names! Problematic only if reading laserhist.root file.
//
// Revision 1.33  2004/03/19 23:13:46  pfachini
// Defining event(0) in the constructor.
//
// Revision 1.32  2004/03/11 21:02:37  pfachini
// The minimum number of valid tracks (minValidTracks) for a good drift velocity
// calculation was lowered to 450 if both east and west lasers are up and 225
// if one of them is down. If one of the lasers is down, the drift velocity for
// east and west will be the same. The condition for a down laser was changed
// from (fzlIntegralEastHigh()< 100. || fzlIntegralEastLow() < 100.) to
// (fzlIntegralEastHigh()/nEvents() < 1. || fzlIntegralEastLow()/nEvents() < 1.)
// and similarly to fzlIntegralWestHigh and fzlIntegralWestLow.
//
// Revision 1.31  2004/03/09 20:32:21  pfachini
// Lowering the number of tracks threshold from 250 to 225 if the west side is down.
// The east side has the sector 20 problem which will reduce the numbers of tracks.
// This change is for run in 2004.
//
// Revision 1.30  2003/09/13 00:42:30  perev
// XDF obsolete + small fixes
//
// Revision 1.29  2003/09/02 17:58:40  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.28  2003/07/09 21:51:57  pfachini
// The minimum number of valid tracks (minValidTracks) for a good drift velocity
// calculation is 500 if both east and west lasers are up and 250 if one of them
// is down. If one of the lasers is down, the drift velocity for east and west will
// be the same.
//
// Revision 1.27  2003/05/15 15:31:31  perev
// tpc oriented off
//
// Revision 1.26  2003/03/01 17:46:08  pfachini
// Fixing the error message (writing numberTracks->GetMean() instead of numberTracks->GetRMS()).
//
// Revision 1.25  2003/01/29 20:35:06  pfachini
// Introducing a sanity check: table written out only if drift velocit > 5.0 cm/us
//
// Revision 1.24  2003/01/21 02:47:59  jeromel
// Prevent spurious crash and modify messaging. Fixed minTracks using enum.
//
// Revision 1.23  2002/02/20 16:14:20  pfachini
// The clock is now obtained from Jon Gans offline db code
//
// Revision 1.22  2002/01/24 23:56:31  pfachini
// Correcting for the clock
//
// Revision 1.21  2002/01/03 22:41:14  jeromel
// Forgot to change the calibration file name. Also added doxygen-like comments
// and documentation. Trimmed traling spaces + ident in some places.
//
// Revision 1.20  2002/01/03 20:39:38  jeromel
// /10^6 for consistency
//
// Revision 1.19  2001/12/23 20:08:04  pfachini
// *** empty log message ***
//
// Revision 1.9  2001/07/17 17:17:57  love
// phi variable added to lasertrack def
//
// Revision 1.8  2001/03/26 18:27:00  love
// Added many features.  Calculates DOCA for laser tracks to mirror positions.
//  POCA  for non laser events to x,y = 0,0.
//
// 8 Jan 2001 Change curvature cut in DOCA from .0001 to .000001
// Revision 1.7  2000/07/26 22:49:40  didenko
// add one more parameter in tpt
//
// Revision 1.6  2000/06/26 22:11:40  fisyak
// remove params
//
// Revision 1.5  2000/04/24 14:36:34  love
// Write clock, drivel, tzero on Event Header.
// truncate psi angles to 0-180 range (a mistake)
// Expand doca to do straight tracks and do 12 laser sectors, add z cut.
// Expand to 2000 possible track numbers.
//
// Revision 1.4  2000/02/15 21:00:59  love
// Added invp and nfit track variables to Hit structure
//
// Revision 1.3  1999/12/08 18:22:59  love
// fix code to remove Linux compiler warning - Bill Love
//
// Revision 1.2  1999/11/30 14:34:34  love
// Corrections to make CC5 compile.  DOCA routine added.
//
//

/*!
 *
 * \class StLaserEventMaker
 * \author love and fachini
 * \date  1999/09/28
 *
 * StLaserEventMaker class. Initially copied from St_LSEvent_Maker
 * and changed LSEvent to LaserEvent. This maker produces calibration
 * file similar to those produced by the tpcT0Maker. Method based on
 * laser information.
 *
 */
#include <Stiostream.h>
#include "StLaserEventMaker.h"
#include "St_DataSetIter.h"
#include "tpc/St_tcl_Module.h"
#include "tpc/St_tph_Module.h"
#include "tpc/St_tpt_Module.h"
#include "tables/St_tfc_adcxyz_Table.h"
#include "tables/St_tpt_track_Table.h"
#include "tables/St_starClockOnl_Table.h"
#include "TTree.h"
#include "StLaserEvent/StLaserEvent.h"
#include "tables/St_tpcDriftVelocity_Table.h"
#include "tables/St_type_index_Table.h"
#include "tables/St_dst_vertex_Table.h"
#include "StTpcDb/StTpcDb.h"
#include "StDetectorDbMaker/StDetectorDbClock.h"
#include "TMath.h"
#include "TFile.h"
ClassImp(StLaserEventMaker)

//_____________________________________________________________________________
  StLaserEventMaker::StLaserEventMaker(const char *name):
    StMaker(name),
    m_clock(0),
    m_tpg_pad_plane(0),
    m_type(0),
    m_tpt_pars(0),
    event(0)
{
  //  mHistOut   = kFALSE;
  mHistOut=kTRUE;
  m_mklaser=kTRUE;
  m_lasers =kTRUE;
  m_rowmin=14;
  m_rowmax=45;
}
//_____________________________________________________________________________
StLaserEventMaker::~StLaserEventMaker(){}

//_____________________________________________________________________________
void StLaserEventMaker::Clear(Option_t *option){
  if (event) event->Clear(option);
  StMaker::Clear(option);
}

//_____________________________________________________________________________
/// Create tables
Int_t StLaserEventMaker::InitRun(int RunId){

  // 		TPG parameters
  m_tpg_pad_plane =(St_tpg_pad_plane *) GetChain()->FindObject("tpg_pad_plane");
  if (!m_tpg_pad_plane) Error("Init","tpc/tpgpar is not initialized. \n");
  assert(m_tpg_pad_plane);


  // 		TCL parameters
  TDataSet *tpcpars = GetInputDB("tpc/tclpars");
  assert(tpcpars);

  TDataSetIter       gime(tpcpars);
  m_type = (St_tcl_tpc_index_type *) gime("type");
  if (!m_type) Error("Init"," Clustering parameters have not been initialized");
  assert(m_type);

  // 		TPT parameters
  tpcpars = GetInputDB("tpc/tptpars");
  gime.Reset(tpcpars);
  m_tpt_pars  = (St_tpt_pars* ) gime("tpt_pars");
  if (!(m_tpt_pars))
    Error("Init", "tpt parameters have not been initialized" );
  assert(m_tpt_pars);

  //  Create a root tree. (let controlling Macro make the file?)
  LOG_INFO << "Making the laser TTree" << endm;
  event = new StLaserEvent();
  GetTFile()->cd();
  m_laser = new TTree("laser","Tpc laser track tree");
  m_laser->SetAutoSave(100000000); //Save every 100 MB
  Int_t bufsize= 64000;
  Int_t split = 99;
  if (split)  bufsize /= 4;
  m_laser->Branch("event", "StLaserEvent",&event, bufsize, split);

  //  Create histograms
  LOG_INFO << "Making Histograms" << endm;
  fzLaser = new TH1F("fzLaser","fzLaser",100,-200,200);
  fzlWestHigh = new TH1F("fzlWestHigh","fzlWestHigh",100,165,190);
  fzlWestLow = new TH1F("fzlWestLow","fzlWestLow",100,15,40);
  fzlEastHigh = new TH1F("fzlEastHigh","fzlEastHigh",100,-190,-165);
  fzlEastLow = new TH1F("fzlEastLow","fzlEastLow",100,-40,-15);
  numberTracks = new TH1F("numberTracks","numberTracks",100,5,2005);
  numberEvents = new TH1F("numberEvents","numberEvents",2,0,2);
  driftVelocityRec = new TH1F("driftVelocityRec","driftVelocityRec",100,5000000,6000000);
  AddHist(fzLaser);
  AddHist(fzlEastHigh);
  AddHist(fzlEastLow);
  AddHist(fzlWestHigh);
  AddHist(fzlWestLow);
  AddHist(numberTracks);
  AddHist(numberEvents);
  AddHist(driftVelocityRec);
  date = 0;
  time = 0;

  return kStOk;
}

//_____________________________________________________________________________
Int_t StLaserEventMaker::Make(){

  if (date==0) {date = GetDate();LOG_INFO << "date = " << date << endm;}
  if (time==0) {time = GetTime();LOG_INFO << "time = " << time << endm;}

  TDataSet *tpc_data =  GetInputDS("tpc_hits");
  if (!tpc_data) return kStWarn;

  // 		Clusters exist -> do tracking
  TDataSetIter gime(tpc_data);
  St_tcl_tphit     *tphit = (St_tcl_tphit     *) gime("tphit");
  if (!tphit) return kStWarn;
#if 0
  // Move the hits according to the ExB corrections. We have a flag.

     if(tphit && m_undoExB){
       LOG_INFO << " correct the hits for ExB effects" << endm;
       tcl_tphit_st *h = tphit->GetTable();
       for (int i = 0;i<tphit->GetNRows();i++,h++){
	 UndoExB(&h->x,&h->y,&h->z);
       }
     }
     // Move the hits by Jim Thomas' distortion removal.
    if(m_undoDistort){
    m_mag = new StMagUtilities() ;
    Float_t x[3], xprime[3];
    tcl_tphit_st  *spc   = tphit->GetTable();
    for(Int_t i=0; i<tphit->GetNRows(); i++, spc++){
     //ExB corections
       x[0] = spc->x;
       x[1] = spc->y;
       x[2] = spc->z;
       m_mag->UndoDistortion(x,xprime);   // input x[3], return xprime[3]
       spc->x = xprime[0];
       spc->y = xprime[1];
       spc->z = xprime[2];
    }
    }

#endif
  St_tpt_track  *tptrack = new St_tpt_track("tptrack",maxNofTracks);
  m_DataSet->Add(tptrack);

  St_dst_vertex  *clusterVertex = new St_dst_vertex("clusterVertex",1);
  Add(clusterVertex);



  //			call TPT tracker
  if (Debug()) LOG_INFO << " start tpt run " << endm;

  Int_t Res_tpt = tpt(m_tpt_pars,tphit,tptrack,clusterVertex);


  if (Res_tpt != kSTAFCV_OK) {
    LOG_INFO << "Problem with tpt... returns " <<Res_tpt<< endm;
    return kStErr;}

  if (Debug()) LOG_INFO << " finish tpt run " << endm;

  MakeHistograms(); // tracking histograms
  return kStOK;
}

//_____________________________________________________________________________
/*!
 * reset static clones arrays
 * go get event number from the event data
 */
void StLaserEventMaker::MakeHistograms()
{
  Int_t evno = 0;
  Float_t C_RAD_PER_DEG = 0.017453292;
  if (m_mklaser) {

    evno = GetEventNumber();
    m_runno = GetRunNumber();
    Float_t m_drivel = gStTpcDb->DriftVelocity();
    driftVelocityReco = m_drivel;
    driftVelocityRec->Fill(driftVelocityReco);
    Float_t m_tzero = gStTpcDb->Electronics()->tZero();
    Float_t l_clockNominal = gStTpcDb->Electronics()->samplingFrequency();
    clockNominal = l_clockNominal;
    //Float_t m_clock = 0;
    //TDataSet* rundb=GetDataBase("RunLog/onl");
    //if (rundb) {
    //St_starClockOnl* starclock = (St_starClockOnl*)rundb->Find("starClockOnl");
    //if (starclock) {
    //starClockOnl_st* clkstr = (starClockOnl_st*)starclock->GetArray();
    //if (clkstr) m_clock = clkstr->frequency/1000000.0;
    //}
    //}
    //if (m_clock == 0) {
    //m_clock = l_clockNominal;
    //cout << "No real clock! Clock is set to be ClockNominal then." << endm;
    //}
    StDetectorDbClock* dbclock = StDetectorDbClock::instance();
    double freq = dbclock->getCurrentFrequency()/1000000.0;
    clock = freq;
    Float_t m_trigger = gStTpcDb->triggerTimeOffset();
    m_date = GetDate();
    m_time = GetTime();
    //   m_tzero = 1.0; m_clock = 1.0;

    // Fill the event header.
    event->SetHeader(evno, m_runno, m_date, m_time,
		     m_tzero, m_drivel, m_clock, m_trigger);
    LOG_INFO << "Event "<< evno << " Run " << m_runno << endm;
    LOG_INFO << " tZero "<< m_tzero << " trigger " << m_trigger << endm;
    LOG_INFO << " clock "<< m_clock << " clockNominal " << l_clockNominal << endm;
    LOG_INFO << " drivel " << m_drivel << endm;
    LOG_INFO << " freq "<< freq << endm;

    //  Make the "laser"  TTree  Should be controllable.
    // Create an iterator for the track dataset
    TDataSetIter tpc_tracks(m_DataSet);
    St_tpt_track * n_track = (St_tpt_track *) tpc_tracks["tptrack"];
    Int_t ntks=n_track->GetNRows();
    //Create matching arrays to hold the sector and laser source
    // point and phi angle for each track
    Int_t *sector = new Int_t[ntks];
    Float_t *xl = new Float_t[ntks];
    Float_t *yl = new Float_t[ntks];
    Float_t *zl = new Float_t[ntks];
    Float_t *phil = new Float_t[ntks];
    tpt_track_st *ta = n_track->GetTable();
    for(int itk=0;itk<ntks;itk++,ta++){
      if(m_lasers){
	DOCA(ta->r0, ta->phi0, ta->z0, ta->psi, ta->tanl, ta->curvature,
	     ta->q, &sector[itk], &xl[itk], &yl[itk], &zl[itk]);
      }
      else
	POCA(ta->r0, ta->phi0, ta->z0, ta->psi, ta->tanl, ta->curvature,
	     ta->q, &xl[itk], &yl[itk], &zl[itk], &phil[itk]);
    }
    //
    St_tfc_adcxyz  *n_adc = 0;
    St_tcl_tphit  *n_hit = 0;
    St_tcl_tpcluster *n_clus  = 0;
    TDataSet *tpc_hits = GetDataSet("tpc_hits");
    if (tpc_hits) {
      TDataSetIter tpc_data(tpc_hits);
      n_hit      = (St_tcl_tphit *) tpc_data["tphit"];
      n_clus     = (St_tcl_tpcluster *)  tpc_data["tpcluster"];
    }
    if(n_hit){
      tcl_tphit_st *h = n_hit->GetTable();
      for (int i = 0;i<n_hit->GetNRows();i++,h++){
	// got a hit - calculate some missing properties.
	// calculate the ExB shift.
	Float_t xhold = h->x;Float_t yhold = h->y;
	UndoExB(&h->x,&h->y,&h->z);
	Float_t exbdx = h->x - xhold;Float_t exbdy = h->y - yhold;
	h->x = xhold;h->y = yhold;
	Int_t tof = h->track/1000; // get the track number
	if(tof){
	  tpt_track_st *te = n_track->GetTable();
	  for(int itrk=0;itrk<ntks;itrk++,te++){//find the right track
	    if(te->id == tof){
	      Float_t x1 = te->r0*cos(te->phi0*C_RAD_PER_DEG);
	      Float_t y1 = te->r0*sin(te->phi0*C_RAD_PER_DEG);
	      Float_t z1 = te->z0;
	      Float_t radius = 1.0/te->curvature;
	      Float_t psic = (te->psi + te->q*90)*C_RAD_PER_DEG;
	      Float_t xc = x1-radius*cos(psic);
	      Float_t yc = y1-radius*sin(psic);
	      Float_t resy = (TMath::Sqrt((h->x-xc)*(h->x-xc)+(h->y-yc)*(h->y-yc))
			      -radius)/cos(h->alpha*C_RAD_PER_DEG);
	      Float_t resz = h->z-z1-te->tanl*
		TMath::Sqrt((h->x-x1)*(h->x-x1) + (h->y-y1)*(h->y-y1));
	      Float_t phi = te->psi;

	      event->AddHit(h->q,h->x,h->y,h->z,h->row,h->track, h->flag,
			    sector[itrk],zl[itrk],phi,te->invp*te->q,te->nfit,
			    resy,resz,h->alpha,h->lambda,h->prf,h->zrf,exbdx,exbdy);
	    }
	  }
	}
	else
	  event->AddHit(h->q,h->x,h->y,h->z,h->row,h->track, h->flag,
			0,0,0,0,0,0,0,h->alpha,h->lambda,h->prf,h->zrf,exbdx,exbdy);
      }
      LOG_INFO << n_hit->GetNRows() << " hits, " ;
    }
    tpt_track_st *t = n_track->GetTable();
    Int_t ngtk=0;
    for(int itrk=0;itrk<ntks;itrk++,t++){
      if (t->flag>0){
	ngtk++;
	Float_t phi = t->psi;
	Float_t tlam = t->tanl;
        event->AddTrack(t->flag,t->hitid,t->id,t->id_globtrk,
			t->ndedx, t->nfit, t->nrec, t->npos, t->q,
			t->chisq[0], t->chisq[1], t->dedx[0], t->invp, t->curvature,
			phi, tlam, t->phi0, t->r0, t->z0, sector[itrk],xl[itrk],
			yl[itrk],zl[itrk], phil[itrk] );
	Float_t fzl = zl[itrk];
	Float_t nfits = t->nfit;
	if (fzl > 165 && fzl < 190 && nfits > 15) fzlWestHigh->Fill(fzl);
	if (fzl > 15 && fzl < 40 && nfits > 15) fzlWestLow->Fill(fzl);
	if (fzl > -190 && fzl < -165 && nfits > 15) fzlEastHigh->Fill(fzl);
	if (fzl > -40 && fzl < -15 && nfits > 15) fzlEastLow->Fill(fzl);
	fzLaser->Fill(fzl);
      }
    } //end of itrk for loop
    numberTracks->Fill(ngtk);
    numberEvents->Fill(1);
    LOG_INFO <<  ntks << " total tracks " << ngtk << " good tracks" << endm;
    delete [] sector;
    delete [] xl;
    delete [] yl;
    delete [] zl;
    Int_t npixwrit=0;
    // Find the adc table.
    //???     St_DataSet *tpc_raw = GetDataSet("tpc_hits");
    TDataSet *tpc_raw = GetDataSet("tpc_hits");
    if(tpc_raw){
      TDataSetIter tpcadc(tpc_raw);
      n_adc = (St_tfc_adcxyz *) tpcadc["adcxyz"];
    }
    if(n_adc){

      tfc_adcxyz_st *p = n_adc->GetTable();
      LOG_INFO << n_adc->GetNRows() << " pixels in adcxyz table, " ;
      for(int iadc=0;iadc<n_adc->GetNRows();iadc++,p++){
	if(p->row >=m_rowmin && p->row<=m_rowmax){
	  event->AddPixel(100*p->sector+p->row,p->pad,p->bucket,
			  p->adc,p->x,p->y,p->z);
	  npixwrit++;
	}
      }
    }
    LOG_INFO << npixwrit <<" pixels written to event " << evno << endm;

    m_laser->Fill(); //Fill the Tree
  }  //end of if(m_mklaser)
}  // end of MakeHistograms member.


//_____________________________________________________________________________
/*!
 * Calculate distance of closest approach to the 12 laser sources
 * for the track and return the sector number for the smallest.
 * uses helices when  curvature is non zero - straight lines else.
 */
void StLaserEventMaker::DOCA(Float_t r0,Float_t phi0,Float_t z0,
			     Float_t psi, Float_t tanl, Float_t curvature, Int_t q,
			     Int_t *sector, Float_t *xl, Float_t *yl, Float_t *zl) {
  static const Float_t zcut[13]={-165,-140,-105,-75,-45,-15,
                15,45,75,105,135,165,195};
  static const Float_t zpt[12] = {-179.2, -151.6, -120.5, -90.7, -59.6,
               -32.0, 32.0, 59.7, 90.7, 120.6, 151.7, 179.2 };
  static const Float_t xpt[12][6]={
               { -171.674,-168.918,2.731,171.654,168.907,-2.745},
	       { -171.445,-169.099,2.294,171.448,169.164,-2.286},
	       { -172.108,-169.118,2.955,172.127,169.136,-2.962},
	       { -171.636,-169.594,2.063,171.599,169.566,-2.054},
	       { -172.343,-169.602,2.713,172.347,169.595,-2.738},
               { -172.096,-169.816,2.272,172.117,169.813,-2.247},
               { 172.090,169.791,-2.264,-172.120,-169.841,2.256},
               { 172.312,169.576,-2.745,-172.316,-169.548,2.747},
               { 171.627,169.547,-2.013,-171.652,-169.570,2.062},
               { 172.099,169.149,-2.943,-172.100,-169.161,2.977},
               { 171.411,169.102,-2.320,-171.410,-169.134,2.292},
               { 171.665,168.924,-2.760,-171.664,-168.913,2.745},

  };
  static const Float_t ypt[12][6]={
               {95.937,-100.671,-196.611,-95.927,100.693,196.647},
               {96.312,-100.322,-196.623,-96.288,100.308,196.577},
               {95.959,-101.048,-197.049,-95.954,101.068,197.007},
               {96.718,-100.234,-197.018,-96.708,100.282,197.027},
               {96.341,-101.054,-197.440,-96.322,101.066,197.397},
               {96.729,-100.653,-197.399,-96.649,100.681,197.387},
               {96.744,-100.674,-197.391,-96.676,100.665,197.433},
               {96.361,-101.048,-197.385,-96.350,101.162,197.403},
               {96.695,-100.269,-196.969,-96.716,100.287,196.998},
               {95.944,-101.073,-197.026,-95.949,101.083,197.022},
               {96.303,-100.273,-196.630,-96.294,100.281,196.626},
               {95.918,-100.714,-196.637,-95.929,100.702,196.627},
  };
    Float_t x, y, z, disxy;
    Int_t iz;
    for (iz=0;iz<13;iz++) if(z0<zcut[iz])break;

   *sector = 99;  *xl = 0.0; *yl = 0.0; *zl = 0.0;
   if(iz==6)return; // between -15 and 15 - no laser rafts there.
    if(iz>6)iz--;
    if(iz>11) return; // only 12 laser z's.

  //
    Float_t ang = 0.017453292 * phi0;
    Float_t x0 = r0 * cos(ang);
    Float_t y0 = r0 * sin(ang);
    ang = 0.017453292 * psi;
    Float_t px = cos(ang); Float_t py = sin(ang);
    Float_t test = 200.0; // cutoff the source match at 10 X 10 cm
    if(curvature>0.000001)
      {
	// helix track, calculate circle center position

	Double_t xc = x0 + q*py/curvature;
	Double_t yc = y0 - q*px/curvature;
	for (int i=0;i<6;i++){
	  Float_t xp = xpt[iz][i]; Float_t yp= ypt[iz][i];
	  Double_t d = xc - xp; Double_t a = yc - yp;
	  Double_t c = d/a;
	  Double_t dy = 1./TMath::Sqrt(1. + c*c)/curvature;
	  Double_t dx = c*dy;
	  if(a<0) { x = xc + dx;  y = yc + dy;}
	  else    { x = xc - dx;  y = yc - dy;}
	  Float_t disq = (x-xp)*(x-xp) + (y-yp)*(y-yp);
	  if (disq<test) {test=disq;
            *xl=x; *yl=y;  *sector = 2*i+14;
	    if(iz>5) *sector-=12;
    	    Float_t sign =1.0; // account for direction to origin
            if((*xl*px+*yl*py)<0) sign=-1.0;
            disxy = TMath::Sqrt((x-x0)*(x-x0)+ (y-y0)*(y-y0));
            *zl = z0 + sign*tanl*disxy;}
	}
      }
    else
      {
      //Straight line tracks
      Float_t slope = tan(psi*0.017453292);
      Float_t ax = 1./(x0 - y0/slope);
      Float_t ay = 1./(y0 - x0*slope);
      for (int i=0;i<6;i++){
	  Float_t xp = xpt[iz][i]; Float_t yp= ypt[iz][i];
	  Float_t d = ax*ax +ay*ay;
	  x = (ax + ay*ay*xp - ax*ay*yp)/d;
	  y = (ay + ax*ax*yp - ax*ay*xp)/d;
   	  Float_t sign =1.0; // account for direction to origin
          if((x*px+y*py)<0) sign=-1.0;
          disxy = TMath::Sqrt((x-x0)*(x-x0)+ (y-y0)*(y-y0));
          z = z0 + sign*tanl*disxy;
          if(TMath::Abs(z-zpt[iz])<15.0){
	    Float_t disq = (x-xp)*(x-xp) + (y-yp)*(y-yp);
	    if (disq<test) {
	      test=disq;
	      *xl=x; *yl=y; *zl=z;  *sector = 2*i+14;
	      if(iz>5) *sector-=12;
	  }
	 }
	}
      }
}

//_____________________________________________________________________________
/*!
 * Calculate point of closest approach to the centerline of the beam.
 */
  void StLaserEventMaker::POCA(Float_t r0,Float_t phi0,Float_t z0,
                      Float_t psi, Float_t tanl, Float_t curvature, Int_t q,
                      Float_t *xl, Float_t *yl, Float_t *zl, Float_t *phil) {
    Float_t x, y, z, disxy, xp = 0.15, yp = 0.15;
    *xl = 100.0; *yl = 100.0; *zl = 0.0, *phil=0.0;

    Float_t ang = 0.017453292 * phi0;
    Float_t x0 = r0 * cos(ang);
    Float_t y0 = r0 * sin(ang);
    ang = 0.017453292 * psi;
    Float_t px = cos(ang); Float_t py = sin(ang);
    Float_t test = 200.0; // cutoff the source match at 10 X 10 cm
    if(curvature>0.0001)
      {
	// helix track, calculate circle center position
	Float_t xc = x0 + q*py/curvature;
	Float_t yc = y0 - q*px/curvature;
        Float_t d = xc - xp; Float_t a = yc - yp;
        Float_t c = d/a;
        Float_t dy = 1./TMath::Sqrt(1. + c*c)/curvature;
        Float_t dx = c*dy;
        if(a<0) { x = xc + dx;  y = yc + dy;}
	else    { x = xc - dx;  y = yc - dy;}
	Float_t disq = (x-xp)*(x-xp) + (y-yp)*(y-yp);
	if (disq<test) {
            *xl=x; *yl=y;
            disxy = TMath::Sqrt((x-x0)*(x-x0)+ (y-y0)*(y-y0));
            *zl = z0 - tanl*disxy;
	    // calculate phi angle at the POCA to the vertex.
	    *phil = 57.29578*atan(a/d) +90.0;
	    if(a*q*(*phil-90.0)<0)*phil+=180.0;
	}
	}
    else
      {
      //Straight line tracks
      Float_t slope = tan(psi*0.017453292);
      Float_t ax = 1./(x0 - y0/slope);
      Float_t ay = 1./(y0 - x0*slope);
      Float_t d = ax*ax +ay*ay;
      x = (ax + ay*ay*xp - ax*ay*yp)/d;
      y = (ay + ax*ax*yp - ax*ay*xp)/d;
      //extrapolate back to x,y
      disxy = TMath::Sqrt((x-x0)*(x-x0)+ (y-y0)*(y-y0));
      z = z0 + -tanl*disxy;
      Float_t disq = (x-xp)*(x-xp) + (y-yp)*(y-yp);
	 if (disq<test) {
	 *xl=x; *yl=y; *zl=z; *phil=psi;
	  }
      }
  }

//_____________________________________________________________________________
/*
 *
 *  Compute the Integral of Br/Bz from the specified point to the pad plane.
 *
 *  Convert it to a displacement (ie distortion).  x, y,z is the reported
 *  location of the track-hit and the actual location as calculated
 *  by this program and replaced in the x, y, z variables.
 *  x, y, z in STAR TPC coord. system (cm units)
 *
 *  Fitted Function is in terms of z, r, phi - scaled to the range -1 to 1.
 *  Separate functions fitted to the positive and negative z TPC ends.
 *  Note that 2 dimensional arrays in C go across rows while Fortran goes
 *  down columns
 *
 */
void StLaserEventMaker::UndoExB(Float_t *x, Float_t *y, Float_t *z){

  const float   TAU_1   = 0.35e-10   ;     // Tau Parameters valid for P9 gas
  const float   TAU_2   = 0.29e-10   ;     // from the Aleph Experiment
  const double  PI      = 3.14159265 ;
  const double  ZSCALE  = 105.0      ;
  const double  ROFF    = 126.7      ;
  const double  RSCALE  =  66.7      ;

  //  This version for the half field in the positive z direction
  //  Data below this point should really be in a database and read
  //  in on user command.

  const float Bmax = 2500. ;
  const int NCOEFF =22;
  const int PCOEFF =19;


  double ncoef [22]     = { 0.86519044E-01, 0.53515316E-01, 0.76126441E-01,
                           -0.25630806E-01, 0.39710304E-01,-0.23678712E-01,
                            0.21143267E-01, 0.21130090E-01,-0.27967561E-01,
                            0.14842647E-01, 0.18729207E-01, 0.91055100E-02,
                            0.12635363E-01, 0.12567390E-01,-0.10256438E-01,
                            0.71242234E-02, 0.49913415E-02,-0.40337429E-02,
                            0.31642346E-02,-0.82095956E-02,-0.64247543E-02,
                            0.36472205E-02   } ;

  int ibasng [22][3]    = { { 10,  0,  0},{ 20, 10,  0},{ 10, 10,  0},
                            {  0, 20,  0},{ 20,  0,  0},{ 30,  0,  0},
                            {  0, 30,  0},{  0,  0,  0},{ 30, 10,  0},
                            { 10, 20,  0},{ 20, 20,  0},{  0, 40,  0},
                            {  0, 10, 20},{ 10, 30,  0},{ 30, 20,  0},
                            { 10,  0, 40},{  0,  0, 50},{ 40,  0,  0},
                            {  0,  0, 40},{ 10, 10, 30},{  0, 10, 30},
                            {  0,  0, 30} } ;

  double pcoef [19]     = {-0.56271269E-01, 0.56316758E-01, 0.32334931E-01,
                           -0.40870593E-01,-0.26015326E-01, 0.30634023E-01,
                           -0.19830788E-01, 0.51648488E-01, 0.81722594E-02,
                           -0.21299166E-01,-0.21224224E-01, 0.11237955E-01,
                           -0.91026999E-02,-0.76372695E-02, 0.12071229E-01,
                            0.11819910E-01,-0.99196176E-02, 0.49532689E-02,
                            0.58692146E-02   } ;

  int ibasps [19][3]   = {{ 20, 10,  0},{ 10,  0,  0},{  0, 10,  0},
                          { 20,  0,  0},{ 30, 10,  0},{  0, 20,  0},
			  {  0, 30,  0},{ 10, 10,  0},{  0,  0,  0},
                          { 30,  0,  0},{ 20, 20,  0},{ 10, 20,  0},
                          {  0, 40,  0},{  0,  0, 20},{ 10, 30,  0},
                          { 10,  0, 20},{ 30, 20,  0},{ 40,  0,  0},
                          { 10,  0, 30}  } ;


  double  Omega, Const_1, Const_2 ;
  double  p, p0, p1, p2, phi ;
  float   r, xv[3] ;
  float   brobz, delxy[2], xp, yp, zp ;
  int     num ;


  Omega     =  Bmax * 8.9875518e6 / 0.511 ;
  Const_1   =  Omega*TAU_1 / ( 1. +  Omega*TAU_1*Omega*TAU_1 ) ;
  Const_2   =  Omega*TAU_2*Omega*TAU_2 / ( 1. + Omega*TAU_2*Omega*TAU_2 ) ;
  xp = *x;
  yp = *y;
  zp = *z;
  r    = TMath::Sqrt( xp*xp + yp*yp ) ;
  xv[1] = ( r - ROFF ) / RSCALE ;
  phi  = atan2( yp, xp ) ;
  if ( phi < 0 ) phi = phi + 2*PI ;
  xv[2] = ( phi - PI ) / PI ;

  brobz = 0.0 ;

  if ( zp < 0 )

    {
      xv[0] = ( zp + ZSCALE ) / ZSCALE ;
      for ( int k = 0 ; k < NCOEFF ; k++ )
        {
	  p = 1 ;
	  for ( int i = 0 ; i < 3 ; i++ )
	    {
	      num = ibasng[k][i] / 10 ;
	      if ( num != 0 )
		{
		  p0 = 1.0 ;
		  p1 = xv[i] ;
		  for ( int j = 2 ; j <= num ; j++ )
		    {
		      p2 = 2 * xv[i] * p1 - p0 ;
		      p0 = p1 ;
		      p1 = p2 ;
		    }
		  p = p * p1 ;
		}
	    }
	  brobz = brobz + ncoef[k] * p ;
	}
      delxy[0] = brobz * (Const_2*cos(phi) + Const_1*sin(phi)) ;
      delxy[1] = brobz * (Const_2*sin(phi) - Const_1*cos(phi)) ;
    }

  else

    {
      xv[0] = ( zp - ZSCALE ) / ZSCALE ;
      for ( int k = 0 ; k < PCOEFF ; k++ )
	{
	  p = 1 ;
	  for ( int i = 0 ; i < 3 ; i++ )
	    {
	      num = ibasps[k][i] / 10 ;
	      if ( num != 0 )
		{
		  p0 = 1.0 ;
		  p1 = xv[i] ;
		  for ( int j = 2 ; j <= num ; j++ )
		    {
		      p2 = 2 * xv[i] * p1 - p0 ;
		      p0 = p1 ;
		      p1 = p2 ;
		    }
		  p = p * p1 ;
		}
	    }
	  brobz = brobz + pcoef[k] * p ;
	}
      delxy[0] = -brobz * (Const_2*cos(phi) + Const_1*sin(phi)) ;
      delxy[1] = -brobz * (Const_2*sin(phi) - Const_1*cos(phi)) ;
    }
  xp = xp + delxy[0];
  yp = yp + delxy[1];

  *x = xp; *y = yp; *z = zp;
}

//_____________________________________________________________________________
Int_t StLaserEventMaker::Finish() {
  if (numberTracks){

    if (fzlIntegralEastHigh()/nEvents() < 1. || fzlIntegralEastLow()/nEvents() < 1.) {//in case east laser was down
      gMessMgr->Warning() << "StLaserEventMaker::no east laser events. Drift Velocity east and west will be the same!!! " << endm;
      if (nTracks() >= minValidTracks/2.) {
	velocityWest = 147.164*driftVelocityReco/fabs(fzlAverageWestHigh()-fzlAverageWestLow());
	velocityEast = velocityWest;
	//Now correcting for laser velocity 
	//v=d/t = d/(d_u/v_u - d/v_l) = v_o*(1/(1-v_o/v_l))
	//d=d_real; d_u=d_used; v_u=v_used; v_l=v_laser; v_o=d*v_u/d_u
	velocityEast = velocityEast*(1./(1.-velocityEast/29979245800.));
	velocityWest = velocityWest*(1./(1.-velocityWest/29979245800.));
	//Now correcting for the clock...
	velocityEast = velocityEast*clock/clockNominal;
	velocityWest = velocityWest*clock/clockNominal;
	velocityEast = velocityEast/1000000.0;
	velocityWest = velocityWest/1000000.0;
	if ((velocityWest > 5.) && (velocityEast > 5.)) {
	  WriteTableToFile();
	} else {
	  gMessMgr->Error() << "StLaserEventMaker:: no laser events. Drift Velocity East = " << velocityEast << " and Drift Velocity West = " << velocityWest << " which is lower than the minimum of " << " 5.0 cm/us" << endm;
	}
      } else {
	gMessMgr->Error() << "StLaserEventMaker:: no laser events. Number Tracks = " << numberTracks->GetMean() << " which is lower than the minimum of " << minValidTracks/2. << " tracks requested for good laser events when one of the lasers (east or west) is down. No table will be written" << endm;  
      }
    } else {
      
      if (fzlIntegralWestHigh()/nEvents() < 1. || fzlIntegralWestLow()/nEvents() < 1.) {//in case west laser was down
	gMessMgr->Warning() << "StLaserEventMaker:: no west laser events. Drift Velocity east and west will be the same!!! " << endm;
	if (nTracks() >= minValidTracks/2.) {
	  velocityEast = 147.199*driftVelocityReco/fabs(fzlAverageEastHigh()-fzlAverageEastLow());
	  velocityWest = velocityEast;
	  //Now correcting for laser velocity 
	  //v=d/t = d/(d_u/v_u - d/v_l) = v_o*(1/(1-v_o/v_l))
	  //d=d_real; d_u=d_used; v_u=v_used; v_l=v_laser; v_o=d*v_u/d_u
	  velocityEast = velocityEast*(1./(1.-velocityEast/29979245800.));
	  velocityWest = velocityWest*(1./(1.-velocityWest/29979245800.));
	  //Now correcting for the clock...
	  velocityEast = velocityEast*clock/clockNominal;
	  velocityWest = velocityWest*clock/clockNominal;
	  velocityEast = velocityEast/1000000.0;
	  velocityWest = velocityWest/1000000.0;
	  if ((velocityWest > 5.) && (velocityEast > 5.)) {
	    WriteTableToFile();
	  } else {
	    gMessMgr->Error() << "StLaserEventMaker:: no laser events. Drift Velocity East = " << velocityEast << " and Drift Velocity West = " << velocityWest << " which is lower than the minimum of " << " 5.0 cm/us" << endm;
	  }
	} else {
	  gMessMgr->Error() << "StLaserEventMaker:: no laser events. Number Tracks = " << numberTracks->GetMean() << " which is lower than the minimum of " << minValidTracks/2. << " tracks requested for good laser events when one of the lasers (east or west) is down. No table will be written" << endm;  
	}
      } else {
	
	if (nTracks() >= minValidTracks) {
	  velocityEast = 147.199*driftVelocityReco/fabs(fzlAverageEastHigh()-fzlAverageEastLow());
	  velocityWest = 147.164*driftVelocityReco/fabs(fzlAverageWestHigh()-fzlAverageWestLow());
	  //Now correcting for laser velocity 
	  //v=d/t = d/(d_u/v_u - d/v_l) = v_o*(1/(1-v_o/v_l))
	  //d=d_real; d_u=d_used; v_u=v_used; v_l=v_laser; v_o=d*v_u/d_u
	  velocityEast = velocityEast*(1./(1.-velocityEast/29979245800.));
	  velocityWest = velocityWest*(1./(1.-velocityWest/29979245800.));
	  //Now correcting for the clock...
	  velocityEast = velocityEast*clock/clockNominal;
	  velocityWest = velocityWest*clock/clockNominal;
	  velocityEast = velocityEast/1000000.0;
	  velocityWest = velocityWest/1000000.0;
	  if ((velocityWest > 5.) && (velocityEast > 5.)) {
	    WriteTableToFile();
	  } else {
	    gMessMgr->Error() << "StLaserEventMaker:: no laser events. Drift Velocity East = " << velocityEast << " and Drift Velocity West = " << velocityWest << " which is lower than the minimum of " << " 5.0 cm/us" << endm;
	  }
	} else {
	  gMessMgr->Error() << "StLaserEventMaker:: no laser events. Number Tracks = " << numberTracks->GetMean() << " which is lower than the minimum of " << minValidTracks << " tracks requested for good laser events. No table will be written" << endm;  
	}
      }
    }
  } else {
    gMessMgr->Error() << "StLaserEventMaker:: completly empty. Are sure you called this maker on laser events ?? No table will be written ... " << endm;
  }
  
  if (mHistOut){
    WriteHistFile();
  }
  return StMaker::Finish();
}

//_____________________________________________________________________________
/// Print CVS commit information
void StLaserEventMaker::PrintInfo() {
  LOG_INFO << "**************************************************************" << endm;
  LOG_INFO << "* $Id: StLaserEventMaker.cxx,v 1.38 2007/10/16 15:28:42 fisyak Exp $" << endm;;
  LOG_INFO << "**************************************************************" << endm;

  if (Debug()) StMaker::PrintInfo();
}

//_____________________________________________________________________________
void StLaserEventMaker::WriteTableToFile(){
  char filename[80];

  sprintf(filename,"./StarDb/Calibrations/tpc/tpcDriftVelocity.%08d.%06d.C",date,time);
  TString dirname = gSystem->DirName(filename);
  if (gSystem->OpenDirectory(dirname.Data())==0) {
    if (gSystem->mkdir(dirname.Data())) {
      LOG_INFO << "Directory " << dirname << " creation failed" << endm;
      LOG_INFO << "Putting tpcDriftVelocity.C in current directory" << endm;
      for (int i=0;i<80;i++){filename[i]=0;}
      sprintf(filename,"tpcDriftVelocity.%08d.%06d.C",date,time);
    }
  }
  ofstream *out = new ofstream(filename);
  driftTable()->SavePrimitive(*out,"");
  return;
}

 St_tpcDriftVelocity* StLaserEventMaker::driftTable(){
   St_tpcDriftVelocity* table = new St_tpcDriftVelocity("tpcDriftVelocity",1);
   tpcDriftVelocity_st* row = table->GetTable();
   row->cathodeDriftVelocityEast = 0.0;
   row->cathodeDriftVelocityWest = 0.0;
   row->laserDriftVelocityEast = velocityEast;
   row->laserDriftVelocityWest = velocityWest;
   table->SetNRows(1);
   return table;
 }
//_____________________________________________________________________________
void StLaserEventMaker::WriteHistFile(){
  char filename[80];
  sprintf(filename,"laserhist.%08d.%06d.root",date,time);
  TFile out(filename,"RECREATE");
  GetHistList()->Write();
  out.Close();
}


double StLaserEventMaker::fzlAverageEastHigh(){
  int nBins = fzlEastHigh->GetNbinsX();
  int binMax = fzlEastHigh->GetMaximumBin();
  int minBin = TMath::Max(1,binMax-20);
  int maxBin = TMath::Min(nBins,binMax+20);
  fzlEastHigh->GetXaxis()->SetRange(minBin,maxBin);
  double mean = fzlEastHigh->GetMean();
  fzlEastHigh->GetXaxis()->SetRange(1,nBins);
  return mean;
};
double StLaserEventMaker::fzlAverageEastLow(){
  int nBins = fzlEastLow->GetNbinsX();
  int binMax = fzlEastLow->GetMaximumBin();
  int minBin = TMath::Max(1,binMax-20);
  int maxBin = TMath::Min(nBins,binMax+20);
  fzlEastLow->GetXaxis()->SetRange(minBin,maxBin);
  double mean = fzlEastLow->GetMean();
  fzlEastLow->GetXaxis()->SetRange(1,nBins);
  return mean;
};
double StLaserEventMaker::fzlAverageWestHigh(){
  int nBins = fzlWestHigh->GetNbinsX();
  int binMax = fzlWestHigh->GetMaximumBin();
  int minBin = TMath::Max(1,binMax-20);
  int maxBin = TMath::Min(nBins,binMax+20);
  fzlWestHigh->GetXaxis()->SetRange(minBin,maxBin);
  double mean = fzlWestHigh->GetMean();
  fzlWestHigh->GetXaxis()->SetRange(1,nBins);
  return mean;
};
double StLaserEventMaker::fzlAverageWestLow(){
  int nBins = fzlWestLow->GetNbinsX();
  int binMax = fzlWestLow->GetMaximumBin();
  int minBin = TMath::Max(1,binMax-20);
  int maxBin = TMath::Min(nBins,binMax+20);
  fzlWestLow->GetXaxis()->SetRange(minBin,maxBin);
  double mean = fzlWestLow->GetMean();
  fzlWestLow->GetXaxis()->SetRange(1,nBins);
  return mean;
}
double StLaserEventMaker::nTracks(){double mean = numberTracks->GetMean();return mean;};
double StLaserEventMaker::fzlIntegralEastHigh(){double integral = fzlEastHigh->Integral();return integral;};
double StLaserEventMaker::fzlIntegralEastLow(){double integral = fzlEastLow->Integral();return integral;};
double StLaserEventMaker::fzlIntegralWestHigh(){double integral = fzlWestHigh->Integral();return integral;};
double StLaserEventMaker::fzlIntegralWestLow(){double integral = fzlWestLow->Integral();return integral;};
double StLaserEventMaker::nEvents(){double integral = numberEvents->Integral();return integral;};

