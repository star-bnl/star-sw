#ifndef FTFPARA
#define FTFPARA
//
//           fft control parameters                          
//
   class FTF_Para {          
     public:
       FTF_Para ( )  { setDefaults() ; } ;
       void      setDefaults ( ) ;
       short     infoLevel;        // Level of information printed about progress
       short     dr_segm;          // Row search range for segments 
       short     dr_track;         // Row search range for tracks 
       short     dEdx  ;           // dEdx switch
       short     dEdx_n_truncate ; // # points to truncate in dEdx
       short     event_reset   ;   // Flag to reset event in fft 
       short     get_errors    ;   // Flag to switch error calculation
       short     ghost_flag    ;   // =1 when there are ghost hits
       short     go_backwards  ;   // Flag to go backwards at the end of track reco
       short     init;             // Control initialization 
       short     merge_primaries ; // Switch to control primary merging 
       short     mn_hit_trk;       // Minimum # hits per track 
       short     mod_row;          // Modulo pad row number to use 
       short     n_hit_segm;       // # hits in initial segments 
       short     min_n_hits_for_fit;
       short     n_eta;            // # volumes in eta 
       short     n_eta_track;      // # Track areas in eta 
       short     n_phi;            // # volumes in nphi 
       short     n_phi_track;      // # Track areas in nphi 
       short     n_pass_primaries ;// # iterations looking for primaries
       short     n_pass_secondaries;// # iterations looking for secondaries
       short     row_end;          // Row where end track search 
       short     row_start;        // Row where start track search
       short     sz_fit_flag;      // Switch for sz fit 
       float     bfield      ;     // Magnetic field  
       float     chi2_hit_cut;     // Maximum hit chi2 
       float     chi2_hit_good;    // Chi2 to stop looking for next hit 
       float     chi2_track_cut;   // Maximum track chi2 
       float     deta;             // Eta search range 
       float     dphi;             // Phi search range 
       float     deta_merge ;      // Eta difference for track merge 
       float     dphi_merge ;      // Phi difference for track merge
       float     eta_min;          // Min eta to consider 
       float     eta_min_track ;   // Track min eta to consider 
       float     eta_max;          // Max eta to consider 
       float     eta_max_track ;   // Track max eta to consider 
       float     good_distance ;   // In segment building
				   // distance consider good enough 
       float     phi_min;          // Min phi to consider 
       float     phi_min_track ;   // Track min phi to consider 
       float     phi_max;          // Max phi to consider 
       float     phi_max_track ;   // Track max phi to consider 
       float     phiShift      ;   // Shift in phi when calculating phi
       float     pt_min_helix_fit ;// Minimum pt to apply helix fit
       float     max_dis_segm;     // Maximum distance for segments 
       float     segment_max_angle;// Maximum angle between to consecutive track pieces 
	                               // when forming segments. A piece is the connection 
	                               // two hits
       float     sz_error_scale;   // sz error scale 
       float     xy_error_scale;   // xy error scale 
       float     x_vertex      ;   // x position primary vertex 
       float     y_vertex      ;   // y position primary vertex 
       float     dx_vertex     ;
       float     dy_vertex     ;
       float     z_vertex      ;
       float     xy_weight_vertex; // Weight vertex in x-y
       float     phi_vertex      ;
       float     r_vertex        ;
       short     phi_closed ;
       short     primaries  ;
       int       n_rp, n_phip, n_etap, n_phietap ;// Number volumes + 1 
       int       n_phit, n_etat ;                 // Number volumes + 1 
       float phi_slice, eta_slice ;
       float phi_slice_track, eta_slice_track ;
   } ;
#endif

