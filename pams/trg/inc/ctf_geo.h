/* ctf_geo.h */
/* This file was made by the idl compiler "stic". Do not edit.
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
/* COMMENTS FROM IDL FILE:
 Controls initialize 
 1=CTB, 2=TOF
 Maximum eta index 
 Minimum eta index 
 Maximum phi index 
 Minimum phi index 
 # counters per tray in rapidity (=z) 
 # counters per tray in phi 
 # trays in rapidity 
 # trays in polar angle phi 
 Counter halft Thickness, radial length in STAR framework 
 Counter half Width, phi length in STAR framework 
 Mean counter inner radius
 Tray half height (radial direction in STAR framework 
 Tray half width  (phi    direction in STAR framework)
 Tray half lenght (z      direction in STAR framework)
 First tray phi of center
 */
#ifndef CTF_GEO_H
#define CTF_GEO_H
#define CTF_GEO_SPEC \
"struct ctf_geo { \
	long init; \
	long detector; \
	long i_eta_max; \
	long i_eta_min; \
	long i_phi_max; \
	long i_phi_min; \
	long n_counter_eta; \
	long n_counter_phi; \
	long n_tray_eta; \
	long n_tray_phi; \
	float counter_thickness; \
	float counter_width; \
	float r; \
	float tray_height; \
	float tray_width; \
	float tray_length; \
	float tray_phi_zero; \
};"
typedef struct ctf_geo_st {
	long init; 
	long detector; 
	long i_eta_max; 
	long i_eta_min; 
	long i_phi_max; 
	long i_phi_min; 
	long n_counter_eta; 
	long n_counter_phi; 
	long n_tray_eta; 
	long n_tray_phi; 
	float counter_thickness; 
	float counter_width; 
	float r; 
	float tray_height; 
	float tray_width; 
	float tray_length; 
	float tray_phi_zero; 
} CTF_GEO_ST;
#endif /* CTF_GEO_H */
