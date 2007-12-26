#ifdef __CINT__
#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;
#pragma link C++ class StTpcDb;
#pragma link C++ global gStTpcDb;
#pragma link C++ class dEdxY2_t;
#pragma link C++ enum StTpcdEdxCorrection::ESector;
#pragma link C++ enum StTpcdEdxCorrection::EOptions;

#pragma link C++ function   tpc_row_to_y_(float *,float *);
#pragma link C++ function   tpc_pad_to_x_(float *,float *,float *);
#pragma link C++ function   tpc_x_to_pad_(float *,float *,float *);
#pragma link C++ function   tpc_local_to_global_err_(int &,const float *,float *);
#pragma link C++ function   tpc_local_to_global_emx_(int &,const float *,float *);
#pragma link C++ function   tpc_local_to_global_(int *,const float *,float *);
#pragma link C++ function   tpc_localsector_to_local_(int *,const float *,float *);
#pragma link C++ function   tpc_global_to_local_(int *,float *,float *);
#pragma link C++ function   tpc_global_to_local_p_(int *,float *,float *);
#pragma link C++ function   tpc_time_to_z_(int *,int *,int *,int *,float *);
#pragma link C++ function   tpc_z_to_time_(float *,int *,int *,int *,int *);
#pragma link C++ function   tpc_drift_velocity_(float *);
#pragma link C++ function   tpc_drift_volume_length_(float *);
#pragma link C++ function   tpc_row_par_(int *,float *,float *,float *);
#pragma link C++ function   tpc_global_to_sector_(int*, float*);
#pragma link C++ function   tpc_sec24_to_sec12_(int*, int*);
#pragma link C++ function   tpc_pad_time_offset_(int*, int*, int*, float*);
#pragma link C++ function   tpc_rdo_mask_(int*, int*);
#pragma link C++ function   tpc_hit_error_table_(int*, int*, int*, float*);
#endif
