struct sl3UPTrack { 
     short     id;        /* track id */
    short     nrec;      /* Number of points assigned to that track */
    float     chisq;     /* xy and sz chi2 squared packed in 16 bits each */
    float     dedx;      /* dE/dx information */
    float     pt  ;      /* pt time charge */
    float     psi;       /* azimuthal angle of the momentum at (r,.. */
    float     tanl;      /* tg of the dip angle at (r,phi,z) */
    float     z0;        /* z coordinate of the first point */
    float     Errors ;   /* dpt, dpsi, dtanl errors (10bits*3)  */
};


