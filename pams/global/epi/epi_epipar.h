/* epi_epipar.h */
/* This file was made by the idl compiler "stic". Do not edit.
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
/* COMMENTS FROM IDL FILE:
   epi_epipar.idl
   Table: epi_epipar
        description: Control parameters for global PID module

 */
#ifndef EPI_EPIPAR_H
#define EPI_EPIPAR_H
#define EPI_EPIPAR_SPEC \
"struct epi_epipar { \
	long ctrl_conflict; \
	long ctrl_sharp; \
	long ctrl_wt; \
	float nsig_max; \
	float qfact_min; \
	float sharp_min; \
	float w_emc; \
	float w_svt; \
	float w_tof; \
	float w_tpc; \
	float w_xtpc; \
	float weight_min; \
};"
typedef struct epi_epipar_st {
	long ctrl_conflict; /* =0 ignores conflicts,=1 resolves conflic     */
	long ctrl_sharp; /* sharpness filter option                      */
	long ctrl_wt; /* selects relative weights for detectors       */
	float nsig_max; /* #sigma cut-off                               */
	float qfact_min; /* min. fraction of tpc trk - mc to be good     */
	float sharp_min; /* minimum sharpness cut-off                    */
	float w_emc; /* Relative detector weight for the EMC         */
	float w_svt; /* Relative detector weight for the SVT         */
	float w_tof; /* Relative detector weight for the TOF         */
	float w_tpc; /* Relative detector weight for the TPC         */
	float w_xtpc; /* Relative detector weight for the XTPC        */
	float weight_min; /* min. weight cut-off used in conflict res     */
} EPI_EPIPAR_ST;
#endif /* EPI_EPIPAR_H */
