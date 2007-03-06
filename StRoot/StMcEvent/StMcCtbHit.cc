/***************************************************************************
 *
 * $Id: StMcCtbHit.cc,v 2.6 2007/03/06 19:48:17 calderon Exp $
 * $Log: StMcCtbHit.cc,v $
 * Revision 2.6  2007/03/06 19:48:17  calderon
 * Added filling of g2t_ctf_hit.ds into StMcCtbHit.
 *
 * Revision 2.5  2005/09/28 21:30:14  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.4  2005/01/27 23:40:46  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.3  2003/12/02 21:22:03  calderon
 * remove unnecessary #include "StMcTrack.hh"
 *
 * Revision 2.2  2003/04/11 19:58:29  calderon
 * Fixed bug in calculating slat and tray.  Start with an int, make sure
 * i_eta and i_phi begin at zero (was missing before), and finally return the values as unsigned
 * ints.
 *
 * Revision 2.1  2003/02/19 03:29:41  calderon
 * Introduction of CTB classes to repository.
 *
 * Revision 1.0  2003/03/18 00:00:00  gans
 * Introduction of Ctb classes.  Modified several classes
 * accordingly.
 */
#include "StMcCtbHit.hh"
#include "tables/St_g2t_ctf_hit_Table.h"
static const char rcsid[] = "$Id: StMcCtbHit.cc,v 2.6 2007/03/06 19:48:17 calderon Exp $";
#ifdef POOL
StMemoryPool StMcCtbHit::mPool(sizeof(StMcCtbHit));
#endif
ClassImp(StMcCtbHit)
StMcCtbHit::StMcCtbHit() { /* noop */ };

StMcCtbHit::StMcCtbHit(const StThreeVectorF& x,const StThreeVectorF& p,
			 const float de, const float ds, const long key,
			 const long id,
			 StMcTrack* parent)  : StMcHit(x, p, de, ds, key, id, parent)
{ /* noop */ }


StMcCtbHit::StMcCtbHit(g2t_ctf_hit_st* pt)
: StMcHit(StThreeVectorF(pt->x[0], pt->x[1], pt->x[2]),
	  StThreeVectorF(pt->p[0], pt->p[1], pt->p[2]),
	  pt->de,
	  pt->ds,
	  pt->id,
	  pt->volume_id,
	  0), mTof(pt->tof)
{/* noop */ }

StMcCtbHit::~StMcCtbHit() {/* noop */}

ostream&  operator<<(ostream& os, const StMcCtbHit& h)
{
  os << "CtbHit\t" << *((StMcHit *) &h);
  unsigned int tray;
  unsigned int slat;
  h.get_slat_tray(slat,tray);
  os << "\tSlat: " << slat
     << " Tray: " << tray
     << " TofF: " << h.tof();
  return os;
}

void StMcCtbHit::get_slat_tray(unsigned int &slat_out, unsigned int &tray_out ) const {
    long i1 ;
    unsigned int volume = mVolumeId;
    int i_phi = static_cast<int>(fmod(volume,100.)) ;
    int i_eta = 999;
    int slat = 999;
    int tray = 999;
    
    i1     = int(volume/100) ;
    if ( i1 < 20 ) {
	i_phi = 14 - i_phi ;
	if ( i_phi < 1 ) i_phi = i_phi + 60 ;
	if ( i1 == 11 ) i_eta = 3 ;
	else 
	    if ( i1 == 12 ) i_eta = 4 ;
    }
    else if ( i1 > 20 ) {
	i_phi = i_phi - 42 ;
	if ( i_phi < 1 ) i_phi = i_phi + 60 ;
	if ( i1 == 21 ) i_eta = 2 ;
	else 
	    if ( i1 == 22 ) i_eta = 1 ;
    }

    i_phi--;
    i_eta--;
    
    if(i_eta == 0){
        slat = 1;
        tray = i_phi + 102;
        if(tray > 119)
	    tray-= 60;
    }
    if(i_eta == 1){
        slat = 0;
        tray = i_phi + 102;
        if(tray > 119)
	    tray-= 60;
    }
    if(i_eta == 2){
        slat = 0;
        tray = 12 - i_phi;
        if(tray < 0)
	    tray += 60;
    }
    if(i_eta == 3){
        slat = 1;
        tray = 12 - i_phi;
        if(tray < 0)
	    tray += 60;
    }
    slat_out = slat;
    tray_out = tray;
}
