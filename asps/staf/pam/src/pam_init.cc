/* automatically generated file -- DO NOT EDIT */
#include "pamc.h"
#include "pamcc.h"
#include "pamf.h"
extern "C" int pam_init(void), pam_start(void), pam_stop(void);
int pam_init() { return 1; }
int pam_start() {
	pamc_load_ami(ami);
	pamcc_load_ami(ami);
	pamf_load_ami(ami);
return 1; }
int pam_stop() { return 1; }
