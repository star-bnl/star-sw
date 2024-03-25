/* spline3.h */
/* This file was made by the idl compiler "stic". Do not edit.
** This was generated for version '(unspecified)'
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
/* COMMENTS FROM IDL FILE:
    spine3.idl

  Table: spine3

       description: keep parameters and knots for TSpline3 creation

 */
#ifndef SPLINE3_H
#define SPLINE3_H
#define SPLINE3_SPEC \
"struct spline3 { \
	long nknots; \
	double Xknots[50]; \
	double Yknots[50]; \
	double ValBeg; \
	double ValEnd; \
	octet option[8]; \
};"
typedef struct spline3_st {
	int nknots; /* no. of knots <= 50 */
	double Xknots[50]; /* X of knots */
	double Yknots[50]; /* Y of knots */
	double ValBeg; 
	double ValEnd; 
	unsigned char option[8]; /* option for boundary condition */
} SPLINE3_ST;
#endif /* SPLINE3_H */
