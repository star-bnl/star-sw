#ifndef SL3USTRACK
#define SL3USTRACK
class sl3USTrack { 
public:
    short     id;        /* track id */
    short     q ;
    short     nHits;      /* Number of points assigned to that track */
    float     chisq;     /* xy and sz chi2 squared packed in 16 bits each */
    float     dedx;      /* dE/dx information */
    float     pt  ;      /* pt time charge */
    float     psi;       /* azimuthal angle of the momentum at (r,.. */
    float     tanl;      /* tg of the dip angle at (r,phi,z) */
    float     z0;        /* z   coordinate of the first point */
    float     r0;        /* r   coordinate of the first point */
    float     phi0;      /* phi coordinate of the first point */
    float     Errors ;   /* dpt, dpsi, dtanl errors (10bits*3)  */

    void print ( ) {
        printf ( " id %d nHits %d pt %e tanl %e psi %e \n",
                 id, nHits, pt, tanl, psi ) ;
    }
};
#endif
