#include "StTGeant3.h"
#include "TCallf77.h" 
#include "TLorentzVector.h"
#include "TClonesArray.h"
#include "TParticle.h"
#include "TGeoManager.h"
#include "TVirtualGeoTrack.h"
#include "StTGeant3gu.h"
#include "G3Bridge.h"
StTGeant3     *StTGeant3gu::fgGeant3=0;
TGeoManager *StTGeant3gu::fgGeoMgr=0;
//______________________________________________________________________
void StTGeant3gu::InitBridge() 
{
   fgGeant3=(StTGeant3*)TVirtualMC::GetMC();
   fgGeoMgr=gGeoManager;
   G3Bridge *b = G3Bridge::Instance();
   if (!b) return;
   b->m_gudtim = (void*)&Gudtim;
   b->m_guplsh = (void*)&Guplsh;
   b->m_guhadr = (void*)&Guhadr;
   b->m_guout  = (void*)&Guout;
   b->m_guphad = (void*)&Guphad;
   b->m_gudcay = (void*)&Gudcay;
   b->m_gudigi = (void*)&Gudigi;
   b->m_guiget = (void*)&Guiget;
   b->m_guinme = (void*)&Guinme;
   b->m_guinti = (void*)&Guinti;
   b->m_gunear = (void*)&Gunear;
   b->m_guskip = (void*)&Guskip;
   b->m_guswim = (void*)&Guswim;
   b->m_guview = (void*)&Guview;
   b->m_gupara = (void*)&Gupara;
   b->m_gutrak = (void*)&Gutrak;
   b->m_gutrev = (void*)&Gutrev;
   b->m_gufld  = (void*)&Gufld;
   b->m_gustep = (void*)&Gustep;
   b->m_gukine = (void*)&Gukine;
}   
   
//______________________________________________________________________
void StTGeant3gu::Gudigi() 
{
//
//    ******************************************************************
//    *                                                                *
//    *       User routine to digitize one event                       *
//    *                                                                *
//    *    ==>Called by : GTRIG                                        *
//    *                                                                *
//    ******************************************************************

}


//______________________________________________________________________
void StTGeant3gu::Guhadr()
{
//
//    ******************************************************************
//    *                                                                *
//    *       User routine to generate one hadronic interaction        *
//    *                                                                *
//    *    ==>Called by : GTHADR,GTNEUT                                *
//    *                                                                *
//    ******************************************************************
//
//
//    ------------------------------------------------------------------
//
      Int_t ihadr = fgGeant3->Gcphys()->ihadr;
      if (ihadr<4)       gheish();
      else if (ihadr==4) flufin();
      else               gfmfin();
}

//______________________________________________________________________
void StTGeant3gu::Guout()
{
//
//    ******************************************************************
//    *                                                                *
//    *       User routine called at the end of each event             *
//    *                                                                *
//    *    ==>Called by : GTRIG                                        *
//    *                                                                *
//    ******************************************************************
//
//
//    ------------------------------------------------------------------
//
  //printf("count_gmedia= %8d\n",count_gmedia);
  //printf("count_gtmedi= %8d\n",count_gtmedi);
  //printf("count_ginvol= %8d\n",count_ginvol);
  //printf("count_gtnext= %8d\n",count_gtnext);
}

//______________________________________________________________________
void StTGeant3gu::Guphad()
{
//
//    ******************************************************************
//    *                                                                *
//    *       User routine to compute Hadron. inter. probabilities     *
//    *                                                                *
//    *    ==>Called by : GTHADR,GTNEUT                                *
//    *                                                                *
//    ******************************************************************
//
//
//    ------------------------------------------------------------------
//
      Int_t ihadr = fgGeant3->Gcphys()->ihadr;
      if (ihadr<4)       gpghei();
      else if (ihadr==4) fldist();
      else               gfmdis();
}

//______________________________________________________________________
void StTGeant3gu::Gudcay()
{
//
//    ******************************************************************
//    *                                                                *
//    *       User routine to decay particles                          *
//    *                                                                *
//    *    ==>Called by : G3DECAY                                      *
//    *                                                                *
//    ******************************************************************
//
//
//    ------------------------------------------------------------------
//
    
    // set decay table
    if (!gMC->GetDecayer()) return;
    gMC->GetDecayer()->ForceDecay();

// Initialize 4-momentum vector    
    Int_t ipart = fgGeant3->Gckine()->ipart;
    static TLorentzVector p;
    
    p[0]=fgGeant3->Gctrak()->vect[3];
    p[1]=fgGeant3->Gctrak()->vect[4];
    p[2]=fgGeant3->Gctrak()->vect[5];
    p[3]=fgGeant3->Gctrak()->vect[6];    
    
// Convert from geant to lund particle code
    Int_t iplund=gMC->PDGFromId(ipart);
// Particle list
    static TClonesArray *particles;
    if(!particles) particles=new TClonesArray("TParticle",1000);
// Decay
    gMC->GetDecayer()->Decay(iplund, &p);
    
// Fetch Particles
    Int_t np = fgGeant3->GetDecayer()->ImportParticles(particles);
    if (np <=1) return;

    TParticle *  iparticle = (TParticle *) particles->At(0);
    Int_t ipF = 0, ipL = 0 ;
    Int_t i,j;

// Array to flag deselected particles
    Int_t*  pFlag = new Int_t[np];
    for (i=0; i<np; i++) pFlag[i]=0;
// Particle loop
    for (i=1; i < np; i++) 
    {
	iparticle = (TParticle *) particles->At(i);
	ipF = iparticle->GetFirstDaughter();
	ipL = iparticle->GetLastDaughter();	
	Int_t kf = iparticle->GetPdgCode();
	Int_t ks = iparticle->GetStatusCode();
//
// Deselect daughters of deselected particles
// and jump skip the current particle
	if (pFlag[i] == 1) {
	    if (ipF > 0) for (j=ipF-1; j<ipL; j++) pFlag[j]=1;
	    continue;
	} // deselected ??
// Particles with long life-time are put on the stack for further tracking
// Decay products are deselected
//	
	if (ks != 1) { 
	    Double_t lifeTime = gMC->GetDecayer()->GetLifetime(kf);
	    if (lifeTime > (Double_t) 1.e-15) {
		if (ipF > 0) for (j=ipF-1; j<ipL; j++) pFlag[j]=1;
	    } else{
		continue;
	    }
	} // ks==1 ?
// Skip neutrinos
	if (kf==12 || kf ==-12) continue;
	if (kf==14 || kf ==-14) continue;
	if (kf==16 || kf ==-16) continue;
	
	Int_t index=fgGeant3->Gcking()->ngkine;
// Put particle on geant stack
// momentum vector
	
	(fgGeant3->Gcking()->gkin[index][0]) = iparticle->Px();
	(fgGeant3->Gcking()->gkin[index][1]) = iparticle->Py();
	(fgGeant3->Gcking()->gkin[index][2]) = iparticle->Pz();
	(fgGeant3->Gcking()->gkin[index][3]) = iparticle->Energy();
	Int_t ilu = gMC->IdFromPDG(kf);

// particle type	
	(fgGeant3->Gcking()->gkin[index][4]) = Float_t(ilu);
// position
	(fgGeant3->Gckin3()->gpos[index][0]) = fgGeant3->Gctrak()->vect[0];
	(fgGeant3->Gckin3()->gpos[index][1]) = fgGeant3->Gctrak()->vect[1];
	(fgGeant3->Gckin3()->gpos[index][2]) = fgGeant3->Gctrak()->vect[2];
// time of flight offset (mm)
	(fgGeant3->Gcking()->tofd[index])    = 0.;
// increase stack counter
	(fgGeant3->Gcking()->ngkine)=index+1;
    }
    delete[] pFlag;
}

//______________________________________________________________________
void StTGeant3gu::Guiget(Int_t&, Int_t&, Int_t&)
{
//
//    ******************************************************************
//    *                                                                *
//    *       User routine for interactive control of GEANT            *
//    *                                                                *
//    *    ==>Called by : <GXINT>, GINCOM                              *
//    *                                                                *
//    ******************************************************************
//
//
//    ------------------------------------------------------------------
//
}

//______________________________________________________________________
void StTGeant3gu::Guinme(Float_t*, Int_t&, Float_t*, Int_t& IYES)
{
//
//    **********************************************
//    *                                            *
//    *    USER ROUTINE TO PROVIDE GINME FUNCTION  *
//    *    FOR ALL USER SHAPES IDENTIFIED BY THE   *
//    *    SHAPE NUMBER SH. POINT IS GIVEN IN X    *
//    *    THE PARAMETERS ARE GIVEN IN P. IYES IS  *
//    *    RETURNED 1 IF POINT IS IN, 0 IF POINT   *
//    *    IS OUT AND LESS THAN ZERO IF SHAPE      *
//    *    NUMBER IS NOT SUPPORTED.                *
//    *                                            *
//    *    ==>Called by : GINME                    *
//    *                                            *
//    **********************************************
//
      IYES=-1;
}

//______________________________________________________________________
void StTGeant3gu::Guinti()
{
//
//    ******************************************************************
//    *                                                                *
//    *       User routine for interactive version                     *
//    *                                                                *
//    *    ==>Called by : <GXINT>,  GINTRI                             *
//    *                                                                *
//    ******************************************************************
//
//
//    ------------------------------------------------------------------
//
}

//______________________________________________________________________
void StTGeant3gu::Gunear(Int_t&, Int_t&, Float_t*, Int_t&)
{
//
//    ******************************************************************
//    *                                                                *
//    *    User search                                                 *
//    *       ISEARC to identify the given volume                      *
//    *       ICALL  to identify the calling routine                   *
//    *              1 GMEDIA like                                     *
//    *              2 GNEXT like                                      *
//    *       X      coordinates (+direction for ICALL=2)              *
//    *       JNEAR  address of default list of neighbours             *
//    *              (list to be overwriten by user)                   *
//    *                                                                *
//    *    Called by : GFTRAC, GINVOL, GTMEDI, GTNEXT, GNEXT, GMEDIA   *
//    *                                                                *
//    ******************************************************************
//
//
//    ------------------------------------------------------------------
//
}

//______________________________________________________________________
void StTGeant3gu::Guskip(Int_t& ISKIP)
{
//
//    ******************************************************************
//    *                                                                *
//    *   User routine to skip unwanted tracks                         *
//    *                                                                *
//    *   Called by : GSSTAK                                           *
//    *   Author    : F.Bruyant                                        *
//    *                                                                *
//    ******************************************************************
//
//
//    ------------------------------------------------------------------
//
      ISKIP = 0;
}

//______________________________________________________________________
void StTGeant3gu::Guswim(Float_t& CHARGE, Float_t& STEP, Float_t* VECT, Float_t* VOUT)
{
//
//    ******************************************************************
//    *                                                                *
//    *       User routine to control tracking of one track            *
//    *       in a magnetic field                                      *
//    *                                                                *
//    *    ==>Called by : GTELEC,GTHADR,GTMUON                         *
//    *                                                                *
//    ******************************************************************
//
//
//    ------------------------------------------------------------------
//
  Int_t   ifield = fgGeant3->Gctmed()->ifield;
  Float_t fieldm = fgGeant3->Gctmed()->fieldm;

  if (ifield==3) {
    Float_t fldcharge = fieldm*CHARGE;
    g3helx3(fldcharge,STEP,VECT,VOUT);
  }
  else if (ifield==2) g3helix(CHARGE,STEP,VECT,VOUT);
  else                g3rkuta(CHARGE,STEP,VECT,VOUT);
}

//______________________________________________________________________
void StTGeant3gu::Guview(Int_t&, Int_t&, DEFCHARD, Int_t& DEFCHARL)
{
//
//    ******************************************************************
//    *                                                                *
//    *       User routine for interactive version                     *
//    *                                                                *
//    *    ==>Called by : <GXINT>, GINC1                               *
//    *                                                                *
//    ******************************************************************
//
//
//    ------------------------------------------------------------------
//
}

//______________________________________________________________________
void StTGeant3gu::Gupara()
{
//
//    ******************************************************************
//    *                                                                *
//    *       User routine called every time a particle falls below    *
//    *       parametrization threshold. This routine should create    *
//    *       the parametrization stack, and, when this is full,       *
//    *       parametrize the shower and track the geantinos.          *
//    *                                                                *
//    *    ==>Called by : GTRACK                                       *
//    *                                                                *
//    ******************************************************************
//
//
//    ------------------------------------------------------------------
//
}

//______________________________________________________________________
Float_t StTGeant3gu::Gudtim(Float_t&, Float_t&, Int_t&, Int_t&)
{
//
//    ******************************************************************
//    *                                                                *
//    *       User function called by GCDRIF to return drift time      *
//    *                                                                *
//    *    ==>Called by : GCDRIF                                       *
//    *                                                                *
//    ******************************************************************
//
//
//    ------------------------------------------------------------------
//
      return 0;
}


//______________________________________________________________________
Float_t StTGeant3gu::Guplsh(Int_t&, Int_t&)
{
//
//    ******************************************************************
//    *                                                                *
//    *                                                                *
//    *    ==>Called by : GLISUR                                       *
//    *                                                                *
//    ******************************************************************
//
//
//    ------------------------------------------------------------------
//
//
//*** By default this defines perfect smoothness
      return 1;
}

//______________________________________________________________________
void StTGeant3gu::Gutrak()
{
//
//    ******************************************************************
//    *                                                                *
//    *       User routine to control tracking of one track            *
//    *                                                                *
//    *    ==>Called by : GTREVE                                       *
//    *                                                                *
//    ******************************************************************
//
//
//    ------------------------------------------------------------------
//
     TVirtualMCApplication::Instance()->PreTrack();

     g3track();

     TVirtualMCApplication::Instance()->PostTrack();
}

//______________________________________________________________________
void StTGeant3gu::Gutrev()
{
//
//    ******************************************************************
//    *                                                                *
//    *       User routine to control tracking of one event            *
//    *                                                                *
//    *    ==>Called by : GTRIG                                        *
//    *                                                                *
//    ******************************************************************
//
//
//    ------------------------------------------------------------------
//
  gtreveroot();
}


//______________________________________________________________________
void StTGeant3gu::Gufld(Float_t *x, Float_t *b)
{
  Double_t xdouble[3];
  Double_t bdouble[3];
  for (Int_t i=0; i<3; i++) xdouble[i] = x[i]; 

  TVirtualMCApplication::Instance()->Field(xdouble,bdouble);

  for (Int_t j=0; j<3; j++) b[j] = bdouble[j]; 
}

//______________________________________________________________________
void StTGeant3gu::Gustep()
{
//
//    ******************************************************************
//    *                                                                *
//    *       User routine called at the end of each tracking step     *
//    *       INWVOL is different from 0 when the track has reached    *
//    *              a volume boundary                                 *
//    *       ISTOP is different from 0 if the track has stopped       *
//    *                                                                *
//    *    ==>Called by : GTRACK                                       *
//    *                                                                *
//    ******************************************************************
//
  Int_t ipp, jk, nt;
  Float_t polar[3]={0,0,0};
  Float_t mom[3];
  static TMCProcess pProc;
  
  TVirtualMCApplication *app = TVirtualMCApplication::Instance();
  TVirtualMCStack* stack = gMC->GetStack();
  //     Stop particle if outside user defined tracking region 
  Double_t x, y, z, rmax;
  fgGeant3->TrackPosition(x,y,z);

#if defined(COLLECT_TRACKS)
  Int_t nstep = fgGeant3->Gctrak()->nstep;
//  Int_t copy;
//  Int_t id = gMC->CurrentVolID(copy);
  Bool_t isnew = kFALSE; // gMC->IsNewTrack() returns true just for new used indices
  if (nstep==0) isnew = kTRUE;
  Int_t cid = stack->GetCurrentTrackNumber();
  Int_t cgid = (fgGeoMgr->GetNtracks())?fgGeoMgr->GetCurrentTrack()->GetId():-1;
  printf("step: cid=%i cgid=%i\n", cid,cgid);
  if (cid==cgid) {
     isnew=kFALSE;
  } else {
     printf("getting new track %i\n", cid);
     Int_t ind = fgGeoMgr->GetTrackIndex(cid);
     if (ind>=0) {
        printf("found... not new, making it current\n");
        fgGeoMgr->SetCurrentTrack(ind);
        isnew = kFALSE;
     }
  }     	     
  Int_t mid = stack->CurrentTrackParent();
  printf("mid=%i\n",mid);
  Int_t cpdg = gMC->PDGFromId(fgGeant3->Gckine()->ipart);
  Double_t xo,yo,zo,to;
  TVirtualGeoTrack *track = fgGeoMgr->GetCurrentTrack();
  if (!track) printf("woops, no track\n");
  if (isnew) {
     if (mid<0) {
        // new primary
        Int_t itrack = fgGeoMgr->AddTrack(cid, cpdg);
        fgGeoMgr->SetCurrentTrack(itrack);
        track = fgGeoMgr->GetCurrentTrack();
        printf("NEW PRIMARY %i AT (%g, %g, %g)\n",cid, x,y,z);
     } else {
        // new secondary
        if (!track) {
           printf("NO CURRENT TRACK !!! FATAL\n");
           exit(1);
        }
	if (mid != cgid) {
	   track = fgGeoMgr->GetParentTrackOfId(mid);
           if (!track) track = fgGeoMgr->GetTrackOfId(mid);
           if (!track) {
              printf("NO MOTHER TRACK with id=%i !!! FATAL\n",mid);
              exit(1);
           }
	}   
        track = track->AddDaughter(cid, cpdg);
        fgGeoMgr->SetCurrentTrack(track);
        printf("NEW TRACK %i AT (%g, %g, %g) parent=%i\n",cid, x,y,z,mid);
     }      
         
     TDatabasePDG *pdgdb = TDatabasePDG::Instance();
     if (pdgdb) {
        TParticlePDG *part = pdgdb->GetParticle(cpdg);
        if (part) track->SetName(part->GetName());
     }   
     track->AddPoint(x,y,z,fgGeant3->Gctrak()->tofg);
  } else {
     if (cid != cgid) {
        Int_t ind = fgGeoMgr->GetTrackIndex(cid);
        if (ind<0) {
           printf("Track of id=%i not found !!!\n", cid);
        } else {
	        fgGeoMgr->SetCurrentTrack(ind);
           track = fgGeoMgr->GetCurrentTrack();
        }
     }		
     track->GetLastPoint(xo,yo,zo,to);
     Bool_t skippoint = kFALSE;
     if (!gMC->IsTrackStop() && !gMC->IsTrackOut()) {
        skippoint = (TMath::Abs(xo*xo+yo*yo+zo*zo-x*x-y*y-z*z) < 1E-1)?kTRUE:kFALSE;
     } else {
//        printf("nstep=%i STOPPED\n", nstep);
     }	
     if (!skippoint) track->AddPoint(x,y,z,fgGeant3->Gctrak()->tofg);
  }     		
#endif  
  rmax = app->TrackingRmax();
  if (x*x+y*y > rmax*rmax ||
      TMath::Abs(z) > app->TrackingZmax()) {
	gMC->StopTrack();
  }

  // --- Add new created particles 
  if (gMC->NSecondaries() > 0) {
    pProc=gMC->ProdProcess(0);
    for (jk = 0; jk < fgGeant3->Gcking()->ngkine; ++jk) {
      ipp = Int_t (fgGeant3->Gcking()->gkin[jk][4]+0.5);
      // --- Skip neutrinos! 
      if (ipp != 4) {
        fgGeant3->SetTrack(1,stack->GetCurrentTrackNumber(),gMC->PDGFromId(ipp), fgGeant3->Gcking()->gkin[jk], 
			 fgGeant3->Gckin3()->gpos[jk], polar,fgGeant3->Gctrak()->tofg, pProc, nt, 1., 0);
      }
    }
  }
  // Cherenkov photons here
  if ( fgGeant3->Gckin2()->ngphot ) {
    for (jk = 0; jk < fgGeant3->Gckin2()->ngphot; ++jk) {
      mom[0]=fgGeant3->Gckin2()->xphot[jk][3]*fgGeant3->Gckin2()->xphot[jk][6];
      mom[1]=fgGeant3->Gckin2()->xphot[jk][4]*fgGeant3->Gckin2()->xphot[jk][6];
      mom[2]=fgGeant3->Gckin2()->xphot[jk][5]*fgGeant3->Gckin2()->xphot[jk][6];
      fgGeant3->SetTrack(1, stack->GetCurrentTrackNumber(), gMC->PDGFromId(50),
		       mom,                             //momentum
		       fgGeant3->Gckin2()->xphot[jk],     //position
		       &fgGeant3->Gckin2()->xphot[jk][7], //polarisation
		       fgGeant3->Gckin2()->xphot[jk][10], //time of flight
		       kPCerenkov, nt, 1., 0);
      }
  }
  // --- Particle leaving the setup ?
  if (!gMC->IsTrackOut()) app->Stepping();

  // --- Standard GEANT debug routine 
  //g3pcxyz();
  if(fgGeant3->Gcflag()->idebug) fgGeant3->Gdebug();
}

//______________________________________________________________________
void StTGeant3gu::Gukine ()
{
//
//    ******************************************************************
//    *                                                                *
//    *       Read or Generates Kinematics for primary tracks          *
//    *                                                                *
//    *    ==>Called by : GTRIG                                        *
//    *                                                                *
//    ******************************************************************
//
//
//    ------------------------------------------------------------------
//

  TVirtualMCApplication::Instance()->GeneratePrimaries();
}

// end of extern "C"
