/* automatically generated file -- DO NOT EDIT */
#include "amiLib.h"
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
int pam_stop() {
	amiInvoker *i;
	if(NULL != (i = ami->findInvoker("pamc")
		))ami->deleteInvoker("pamc");
	if(NULL != (i = ami->findInvoker("pamcc")
		))ami->deleteInvoker("pamcc");
	if(NULL != (i = ami->findInvoker("pamf")
		))ami->deleteInvoker("pamf");
return 1; }
