// $Id: StLaserEventMaker.cxx,v 1.3 1999/12/08 18:22:59 love Exp $
// $Log: StLaserEventMaker.cxx,v $
// Revision 1.3  1999/12/08 18:22:59  love
// fix code to remove Linux compiler warning - Bill Love
//
// Revision 1.2  1999/11/30 14:34:34  love
// Corrections to make CC5 compile.  DOCA routine added.
//
// Revision 1.1  1999/09/28 15:34:34  love
// change LSEvent to LaserEvent
//

//
// Revision 1.1.1.1  1999/09/28 14:29:31  love
// First release of StLaserEventMaker
//
// copied from St_LSEvent_Maker
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StLaserEventMaker class                                               //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include "StLaserEventMaker.h"
#include "StBFChain.h"
#include "St_DataSetIter.h"
#include "St_XDFFile.h"
#include "tpc/St_tcl_Module.h"
#include "tpc/St_tph_Module.h"
#include "tpc/St_tpt_Module.h"
#include "tables/St_tfc_adcxyz_Table.h"
#include "tables/St_tpt_track_Table.h"
#include "TTree.h"
#include "StLaserEvent/StLaserEvent.h"
#include "tables/St_type_index_Table.h"

ClassImp(StLaserEventMaker)

//_____________________________________________________________________________
  StLaserEventMaker::StLaserEventMaker(const char *name):
    StMaker(name),
    m_tpg_pad_plane(0),
    m_type(0),
    m_tpt_pars(0)
{
  m_mklaser=kTRUE;
  m_rowmin=14;
  m_rowmax=45;
}
//_____________________________________________________________________________
StLaserEventMaker::~StLaserEventMaker(){}
//_____________________________________________________________________________
void StLaserEventMaker::Clear(Option_t *option){
  event->Clear(option);
  StMaker::Clear(option);
}
//_____________________________________________________________________________
Int_t StLaserEventMaker::Init(){
  // Create tables
  
  St_DataSet *tpcpars = GetInputDB("params/tpc");
  assert(tpcpars);
  
  St_DataSetIter       gime(tpcpars);
  
// 		TPG parameters
  m_tpg_pad_plane = (St_tpg_pad_plane *) gime("tpgpar/tpg_pad_plane");
  if (!(m_tpg_pad_plane)) Error("Init","tpc/tpgpar is not initialized. \n");
  assert(m_tpg_pad_plane);
  
// 		TCL parameters
  m_type = (St_tcl_tpc_index_type *) gime("tclpars/type");
  if (!m_type) Error("Init"," Clustering parameters have not been initialized");
  assert(m_type);
  
// 		TPT parameters
  m_tpt_pars  = (St_tpt_pars* ) gime("tptpars/tpt_pars" );
  if (!(m_tpt_pars)) 
    Error("Init", "tpt parameters have not been initialized" );
  assert(m_tpt_pars);

//  Create a root tree. (let controlling Macro make the file?)
  event = new StLaserEvent();
  ((StBFChain* )GetChain())->GetTFile()->cd();
  m_laser = new TTree("laser","Tpc laser track tree");
  Int_t bufsize= 64000;
  m_laser->Branch("event", "StLaserEvent",&event, bufsize, 1);

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StLaserEventMaker::Make(){
  
  St_DataSet *tpc_data =  GetInputDS("tpc_hits"); 
  if (!tpc_data) return 0;
  
// 		Clusters exist -> do tracking
  St_DataSetIter gime(tpc_data);
  St_tcl_tphit     *tphit = (St_tcl_tphit     *) gime("tphit");
  //  St_tcl_tpc_index *index = (St_tcl_tpc_index *) gime("index");
  //  if (!index) {index = new St_tcl_tpc_index("index",10*maxNofTracks);  m_DataSet->Add(index);}
      
  St_tpt_track  *tptrack = new St_tpt_track("tptrack",maxNofTracks);
  m_DataSet->Add(tptrack);


//			call TPT tracker 
    if (Debug()) cout << " start tpt run " << endl;
    Int_t Res_tpt = tpt(m_tpt_pars,tphit,tptrack);
//                      ==============================
    
    if (Res_tpt != kSTAFCV_OK) {
     cout << "Problem with tpt.." << endl;
      return kStErr;}
    
    if (Debug()) cout << " finish tpt run " << endl;

  MakeHistograms(); // tracking histograms
  return kStOK;
}
//_____________________________________________________________________________
  void StLaserEventMaker::MakeHistograms() {
    // reset static clones arrays
   // go get event number from the event data
   Int_t evno = 0;
   Float_t C_RAD_PER_DEG = 0.017453292;
   if (m_mklaser) {
    
     evno = GetEventNumber();
     m_runno = GetRunNumber();

     // Fill the event header.  Run number and date passed in from macro.
     event->SetHeader(evno, m_runno, m_date);
     cout << "Event "<< evno << " Run " << m_runno << endl;

     //  Make the "laser"  TTree  Should be controllable.
     // Create an iterator for the track dataset
     St_DataSetIter tpc_tracks(m_DataSet);
	St_tpt_track * n_track = (St_tpt_track *) tpc_tracks["tptrack"];
          Int_t ntks=n_track->GetNRows();
	  //Create matching arrays to hold the sector and laser source
	  // point for each track
	  Int_t *sector = new Int_t[ntks];
          Float_t *xl = new Float_t[ntks];
          Float_t *yl = new Float_t[ntks];
          Float_t *zl = new Float_t[ntks];
          tpt_track_st *ta = n_track->GetTable();
	  for(int itk=0;itk<ntks;itk++,ta++){
          DOCA(ta->r0, ta->phi0, ta->z0, ta->psi, ta->tanl, ta->curvature,
           ta->q, &sector[itk], &xl[itk], &yl[itk], &zl[itk]);
	  }
         
    //
     St_tfc_adcxyz  *n_adc = 0;
     St_tcl_tphit  *n_hit = 0;
     St_tcl_tpcluster *n_clus  = 0;
     St_DataSet *tpc_hits = GetDataSet("tpc_hits");
     if (tpc_hits) {
        St_DataSetIter tpc_data(tpc_hits);
        n_hit      = (St_tcl_tphit *) tpc_data["tphit"];
        n_clus     = (St_tcl_tpcluster *)  tpc_data["tpcluster"];
     }
     if(n_hit){
       tcl_tphit_st *h = n_hit->GetTable();
       for (int i = 0;i<n_hit->GetNRows();i++,h++){
	 // got a hit - calculate some missing properties.
         Int_t tof = h->track/1000; // get the track number
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
            Float_t resy = (sqrt((h->x-xc)*(h->x-xc)+(h->y-yc)*(h->y-yc)) 
                     -radius)/cos(h->alpha*C_RAD_PER_DEG);
            Float_t resz = h->z-z1-te->tanl*
                        sqrt((h->x-x1)*(h->x-x1) + (h->y-y1)*(h->y-y1));

          event->AddHit(h->q,h->x,h->y,h->z,h->row,h->track, h->flag,
       sector[itrk],zl[itrk],te->psi,
       resy,resz,h->alpha,h->lambda,h->prf,h->zrf);
	 }       }}
       cout << n_hit->GetNRows() << " hits, " ; 
     }
        tpt_track_st *t = n_track->GetTable();
     Int_t ngtk=0;         
     for(int itrk=0;itrk<ntks;itrk++,t++){
         if (t->flag>0){
         ngtk++;
         event->AddTrack(t->flag,t->hitid,t->id,t->id_globtrk,
         t->ndedx, t->nfit, t->nrec, t->npos, t->q,
         t->chisq[0], t->chisq[1], t->dedx[0], t->invp, t->curvature,
         t->psi, t->tanl, t->phi0, t->r0, t->z0, sector[itrk],xl[itrk],
         yl[itrk],zl[itrk] );
	 }
     } //end of itrk for loop 
     cout <<  ntks << " total tracks " << ngtk << " good tracks" << endl;
      delete [] sector;
      delete [] xl;
      delete [] yl;
      delete [] zl;
     // Find the adc table.
     St_DataSet *tpc_raw = GetDataSet("tpc_raw");
     if(tpc_raw){
        St_DataSetIter tpcadc(tpc_raw);
        n_adc = (St_tfc_adcxyz *) tpcadc["adcxyz"];
     }
     if(n_adc){
         Int_t npixwrit=0;
         tfc_adcxyz_st *p = n_adc->GetTable();
         cout << n_adc->GetNRows() << " pixels in adcxyz table, " ;
         for(int iadc=0;iadc<n_adc->GetNRows();iadc++,p++){
	   if(p->row >=m_rowmin && p->row<=m_rowmax && p->z<-15.0){
	     event->AddPixel(100*p->sector+p->row,p->pad,p->bucket,
                   p->adc,p->x,p->y,p->z);
	     npixwrit++;
	   }
	 }
	 cout << npixwrit <<" pixels written to event " << evno << endl;
     }
     m_laser->Fill(); //Fill the Tree
   }  //end of if(m_mklaser)
}  // end of MakeHistograms member.
//_____________________________________________________________________________
  void StLaserEventMaker::DOCA(Float_t r0,Float_t phi0,Float_t z0,
                      Float_t psi, Float_t tanl, Float_t curvature, Int_t q,
                      Int_t *sector, Float_t *xl, Float_t *yl, Float_t *zl) {
  // calculate distance of closest approach to the 6 laser sources
  // for the track and return the sector number for the smallest.
  static const Float_t zcut[6]={-165, -136, -106, -75, -48, -15};
  static const Float_t xpt[6][6]={
               { -171.674,-168.918,2.731,171.654,168.907,-2.745},
	       { -171.445,-169.099,2.294,171.448,169.164,-2.286},
	       { -172.108,-169.118,2.955,172.127,169.136,-2.962},
	       { -171.636,-169.594,2.063,171.599,169.566,-2.054},
	       { -172.343,-169.602,2.713,172.347,169.595,-2.738},
             { -172.096,-169.816,2.272,172.117,169.813,-2.247}};
  static const Float_t ypt[6][6]={
               {95.937,-100.671,-196.611,-95.927,100.693,196.647},
               {96.312,-100.322,-196.623,-96.288,100.308,196.577},
               {95.959,-101.048,-197.049,-95.954,101.068,197.007},
               {96.718,-100.277,-197.018,-96.708,100.282,197.027},
               {96.341,-101.054,-197.440,-96.322,101.066,197.397},
               {96.729,-100.653,-197.399,-96.649,100.681,197.387}};
    Float_t x, y;
    Int_t iz;
    for (iz=0;iz<6;iz++) if(z0<zcut[iz])break;
  //
    Float_t ang = 0.017453292 * phi0;
    Float_t x0 = r0 * cos(ang);
    Float_t y0 = r0 * sin(ang);
    // calculate circle center position
    ang = 0.017453292 * psi;
    Float_t px = cos(ang); Float_t py = sin(ang);
    Float_t xc = x0 + q*py/curvature;
    Float_t yc = y0 - q*px/curvature;
    Float_t test = 200.0; // cutoff the source match at 10 X 10 cm
    *sector = 99;  *xl = 0.0; *yl = 0.0; *zl = 0.0;
       for (int i=0;i<6;i++){
    Float_t xp = xpt[iz][i]; Float_t yp= ypt[iz][i];
    Float_t d = xc - xp; Float_t a = yc - yp;
    Float_t c = d/a;  
    Float_t dy = 1./sqrt(1. + c*c)/curvature;
    Float_t dx = c*dy;
    if(a<0) { x = xc + dx;  y = yc + dy;}
    else    { x = xc - dx;  y = yc - dy;}
    Float_t disq = (x-xp)*(x-xp) + (y-yp)*(y-yp);
      if (disq<test) {test=disq; 
                    *xl=x; *yl=y;  *sector = 2*i+14;
		    Float_t sign =1.0; // account for direction to origin
         if((*xl*px+*yl*py)<0) sign=-1.0;
                  *zl = z0 + sign*tanl*sqrt((x-x0)*(x-x0)+ (y-y0)*(y-y0));}
       }
  }
//______________________________________________________________________________

//_____________________________________________________________________________

