struct sl3MPTrack{
   short    id     ;// id
   short    nHits  ;// # Hits
   float    dedx   ;
   float    s11Xy  ;// Fit parameters
   float    s12Xy  ;// conformal line in xy plane
   float    s22Xy  ;
   float    g1Xy   ;
   float    g2Xy   ;
   float    s11Sz  ;// Fit parameters in sz plane
   float    s12Sz  ;
   float    s22Sz  ;
   float    g1Sz   ;
   float    g2Sz   ;
   float    trackLength ;
};
