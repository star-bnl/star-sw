:Evaluate: BeginPackage["LoopTools`"]

:Evaluate: A0::usage = "A0[m] is the one-point one-loop scalar integral.
	m is the mass squared."

:Evaluate: B0::usage = "B0[p, m1, m2] is the two-point one-loop scalar integral.
	p is the external momentum squared and m1 and m2 are the masses squared."

:Evaluate: B1::usage = "B1[p, m1, m2] is the coefficient of k_mu in the two-point one-loop tensor integral B_mu.
	p is the external momentum k squared and m1 and m2 are the masses squared."

:Evaluate: B00::usage = "B00[p, m1, m2] is the coefficient of g_{mu nu} in the two-point one-loop tensor integral B_{mu nu}.
	p is the external momentum squared and m1 and m2 are the masses squared."

:Evaluate: B11::usage = "B11[p, m1, m2] is the coefficient of k_mu k_nu in the two-point one-loop tensor integral B_{mu nu}.
	p is the external momentum k squared and m1 and m2 are the masses squared."

:Evaluate: DB0::usage = "DB0[p, m1, m2] is the derivative of B0[p, m1, m2] with respect to p."

:Evaluate: DB1::usage = "DB1[p, m1, m2] is the derivative of B1[p, m1, m2] with respect to p."

:Evaluate: DB00::usage = "DB00[p, m1, m2] is the derivative of B00[p, m1, m2] with respect to p."

:Evaluate: DB11::usage = "DB11[p, m1, m2] is the derivative of B11[p, m1, m2] with respect to p."

:Evaluate: C0::usage = "C0[p1, p2, p1p2, m1, m2, m3] is the three-point one-loop scalar integral.
	p1, p2, and p1p2 are the external momenta squared and m1, m2, m3 are the masses squared."

:Evaluate: C0i::usage = "C0i[id, p1, p2, p1p2, m1, m2, m3] is the generic three-point loop integral which includes both scalar and tensor coefficients, specified by id.
	For example, C0i[cc0, ...] is the scalar function C_0, C0i[cc112, ...] the tensor coefficient function C_112 etc.
	p1, p2, and p1p2 are the external momenta squared and m1, m2, m3 are the masses squared."

:Evaluate: Cget::usage = "Cget[p1, p2, p1p2, m1, m2, m3] returns a list of all three-point coefficients."

:Evaluate: D0::usage = "D0[p1, p2, p3, p4, p1p2, p2p3, m1, m2, m3, m4] is the four-point scalar one-loop integral.
	p1...p4 are the external momenta squared, p1p2 and p2p3 are the squares of external momenta 1 + 2 and 2 + 3, respectively, and m1...m4 are the masses squared."

:Evaluate: D0i::usage = "D0i[id, p1, p2, p3, p4, p1p2, p2p3, m1, m2, m3, m4] is the generic four-point loop integral which includes both scalar and tensor coefficients, specified by id.
	For example, D0i[dd0, ...] is the scalar function D_0, D0i[dd1233, ...] the tensor function D_{1233} etc.
	p1...p4 are the external momenta squared, p1p2 and p2p3 are the squares of external momenta 1 + 2 and 2 + 3, respectively, and m1...m4 are the masses squared."

:Evaluate: Dget::usage = "Dget[p1, p2, p3, p4, p1p2, p2p3, m1, m2, m3, m4] returns a list of all four-point coefficients."

:Evaluate: PaVe::usage = "PaVe[ind, {pi}, {mi}] is the generalized Passarino-Veltman function used by FeynCalc.
	It is converted to C0i or D0i in LoopTools."

:Evaluate: GetDelta::usage = "GetDelta[] returns the current numerical value of Delta which replaces the divergence 2/(4 - D) - EulerGamma + Log[4 Pi] in LoopTools."

:Evaluate: GetMudim::usage = "GetMudim[] returns the current value for the renormalization scale squared."

:Evaluate: GetLambda::usage = "GetLambda returns the current value for the infrared regulator mass squared."

:Evaluate: SetDelta::usage = "SetDelta[d] sets the numerical value of Delta which replaces the divergence 2/(4 - D) - EulerGamma + Log[4 Pi] in LoopTools."

:Evaluate: SetMudim::usage = "SetMudim[m^2] sets the renormalization scale squared."

:Evaluate: SetLambda::usage = "SetLambda[l^2] sets the infrared regulator mass squared."

:Evaluate: GetCacheLast::usage = "GetCacheLast[c] returns the current position in the cache c, which can be Ccache, Dcache, CCcache, or CDcache.
	This value is needed for SetCacheLast."

:Evaluate: SetCacheLast::usage = "SetCacheLast[c, p] sets the cache pointer in the cache c to p, where c can be Ccache, Dcache, CCcache, or CDcache.
	The integer p is either the position previously obtained by GetCacheLast, or 0 to reset the cache position to the beginning."

:Evaluate: { Ccache, Dcache, CCcache, CDcache,
	cc0, cc1, cc2, cc00, cc11, cc12, cc22, cc001, cc002,
	cc111, cc112, cc122, cc222,
	dd0, dd1, dd2, dd3, dd00, dd11, dd12, dd13, dd22, dd23, dd33,
	dd001, dd002, dd003, dd111, dd112, dd113, dd122, dd123, dd133,
	dd222, dd223, dd233, dd333, dd0000, dd0011, dd0012, dd0013,
	dd0022, dd0023, dd0033, dd1111, dd1112, dd1113, dd1122, dd1123,
	dd1133, dd1222, dd1223, dd1233, dd1333, dd2222, dd2223, dd2233,
	dd2333, dd3333 }

:Evaluate: Begin["`Private`"]

:Begin:
:Function:	fa0
:Pattern:	A0[m_?doubleQ]
:Arguments:	{N[m]}
:ArgumentTypes:	{Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fca0
:Pattern:	A0[m_?complexQ]
:Arguments:	{N[Re[m]], N[Im[m]]}
:ArgumentTypes:	{Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fb0
:Pattern:	B0[p_?doubleQ, m1_?doubleQ, m2_?doubleQ]
:Arguments:	{N[p], N[m1], N[m2]}
:ArgumentTypes:	{Real, Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fcb0
:Pattern:	B0[p_?complexQ, m1_?complexQ, m2_?complexQ]
:Arguments:	{N[Re[p]], N[Im[p]], N[Re[m1]], N[Im[m1]],
		  N[Re[m2]], N[Im[m2]]}
:ArgumentTypes:	{Real, Real, Real, Real, Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fdb0
:Pattern:	DB0[p_?doubleQ, m1_?doubleQ, m2_?doubleQ]
:Arguments:	{N[p], N[m1], N[m2]}
:ArgumentTypes:	{Real, Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fcdb0
:Pattern:	DB0[p_?complexQ, m1_?complexQ, m2_?complexQ]
:Arguments:	{N[Re[p]], N[Im[p]], N[Re[m1]], N[Im[m1]],
		  N[Re[m2]], N[Im[m2]]}
:ArgumentTypes:	{Real, Real, Real, Real, Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fb1
:Pattern:	B1[p_?doubleQ, m1_?doubleQ, m2_?doubleQ]
:Arguments:	{N[p], N[m1], N[m2]}
:ArgumentTypes:	{Real, Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fcb1
:Pattern:	B1[p_?complexQ, m1_?complexQ, m2_?doubleQ]
:Arguments:	{N[Re[p]], N[Im[p]], N[Re[m1]], N[Im[m1]],
		  N[Re[m2]], N[Im[m2]]}
:ArgumentTypes:	{Real, Real, Real, Real, Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fdb1
:Pattern:	DB1[p_?doubleQ, m1_?doubleQ, m2_?doubleQ]
:Arguments:	{N[p], N[m1], N[m2]}
:ArgumentTypes:	{Real, Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fcdb1
:Pattern:	DB1[p_?complexQ, m1_?complexQ, m2_?doubleQ]
:Arguments:	{N[Re[p]], N[Im[p]], N[Re[m1]], N[Im[m1]],
		  N[Re[m2]], N[Im[m2]]}
:ArgumentTypes:	{Real, Real, Real, Real, Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fb00
:Pattern:	B00[p_?doubleQ, m1_?doubleQ, m2_?doubleQ]
:Arguments:	{N[p], N[m1], N[m2]}
:ArgumentTypes:	{Real, Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fcb00
:Pattern:	B00[p_?complexQ, m1_?complexQ, m2_?doubleQ]
:Arguments:	{N[Re[p]], N[Im[p]], N[Re[m1]], N[Im[m1]],
		  N[Re[m2]], N[Im[m2]]}
:ArgumentTypes:	{Real, Real, Real, Real, Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fdb00
:Pattern:	DB00[p_?doubleQ, m1_?doubleQ, m2_?doubleQ]
:Arguments:	{N[p], N[m1], N[m2]}
:ArgumentTypes:	{Real, Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fcdb00
:Pattern:	DB00[p_?complexQ, m1_?complexQ, m2_?doubleQ]
:Arguments:	{N[Re[p]], N[Im[p]], N[Re[m1]], N[Im[m1]],
		  N[Re[m2]], N[Im[m2]]}
:ArgumentTypes:	{Real, Real, Real, Real, Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fb11
:Pattern:	B11[p_?doubleQ, m1_?doubleQ, m2_?doubleQ]
:Arguments:	{N[p], N[m1], N[m2]}
:ArgumentTypes:	{Real, Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fcb11
:Pattern:	B11[p_?complexQ, m1_?complexQ, m2_?doubleQ]
:Arguments:	{N[Re[p]], N[Im[p]], N[Re[m1]], N[Im[m1]],
		  N[Re[m2]], N[Im[m2]]}
:ArgumentTypes:	{Real, Real, Real, Real, Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fdb11
:Pattern:	DB11[p_?doubleQ, m1_?doubleQ, m2_?doubleQ]
:Arguments:	{N[p], N[m1], N[m2]}
:ArgumentTypes:	{Real, Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fcdb11
:Pattern:	DB11[p_?complexQ, m1_?complexQ, m2_?doubleQ]
:Arguments:	{N[Re[p]], N[Im[p]], N[Re[m1]], N[Im[m1]],
		  N[Re[m2]], N[Im[m2]]}
:ArgumentTypes:	{Real, Real, Real, Real, Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fc0
:Pattern:	C0[p1_?doubleQ, p2_?doubleQ, p1p2_?doubleQ,
		  m1_?doubleQ, m2_?doubleQ, m3_?doubleQ]
:Arguments:	{N[p1], N[p2], N[p1p2], N[m1], N[m2], N[m3]}
:ArgumentTypes:	{Real, Real, Real, Real, Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fcc0
:Pattern:	C0[p1_?complexQ, p2_?complexQ, p1p2_?complexQ,
		  m1_?complexQ, m2_?complexQ, m3_?complexQ]
:Arguments:	{N[Re[p1]], N[Im[p1]], N[Re[p2]], N[Im[p2]],
		  N[Re[p1p2]], N[Im[p1p2]], N[Re[m1]], N[Im[m1]],
		  N[Re[m2]], N[Im[m2]], N[Re[m3]], N[Im[m3]]}
:ArgumentTypes:	{Real, Real, Real, Real, Real, Real,
		  Real, Real, Real, Real, Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fc0i
:Pattern:	C0i[id_Symbol,
		  p1_?doubleQ, p2_?doubleQ, p1p2_?doubleQ,
		  m1_?doubleQ, m2_?doubleQ, m3_?doubleQ]
:Arguments:	{id, N[p1], N[p2], N[p1p2], N[m1], N[m2], N[m3]}
:ArgumentTypes:	{Symbol, Real, Real, Real, Real, Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fcc0i
:Pattern:	C0i[id_Symbol,
		  p1_?complexQ, p2_?complexQ, p1p2_?complexQ,
		  m1_?complexQ, m2_?complexQ, m3_?complexQ]
:Arguments:	{id, N[Re[p1]], N[Im[p1]], N[Re[p2]], N[Im[p2]],
		  N[Re[p1p2]], N[Im[p1p2]], N[Re[m1]], N[Im[m1]],
		  N[Re[m2]], N[Im[m2]], N[Re[m3]], N[Im[m3]]}
:ArgumentTypes:	{Symbol, Real, Real, Real, Real, Real, Real,
		  Real, Real, Real, Real, Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fcget
:Pattern:	Cget[p1_?doubleQ, p2_?doubleQ, p1p2_?doubleQ,
		  m1_?doubleQ, m2_?doubleQ, m3_?doubleQ]
:Arguments:	{N[p1], N[p2], N[p1p2], N[m1], N[m2], N[m3]}
:ArgumentTypes:	{Real, Real, Real, Real, Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fccget
:Pattern:	Cget[p1_?complexQ, p2_?complexQ, p1p2_?complexQ,
		  m1_?complexQ, m2_?complexQ, m3_?complexQ]
:Arguments:	{N[Re[p1]], N[Im[p1]], N[Re[p2]], N[Im[p2]],
		  N[Re[p1p2]], N[Im[p1p2]], N[Re[m1]], N[Im[m1]],
		  N[Re[m2]], N[Im[m2]], N[Re[m3]], N[Im[m3]]}
:ArgumentTypes:	{Real, Real, Real, Real, Real, Real,
		  Real, Real, Real, Real, Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fd0
:Pattern:	D0[p1_?doubleQ, p2_?doubleQ, p3_?doubleQ, p4_?doubleQ,
		  p1p2_?doubleQ, p2p3_?doubleQ,
		  m1_?doubleQ, m2_?doubleQ, m3_?doubleQ, m4_?doubleQ]
:Arguments:	{N[p1], N[p2], N[p3], N[p4], N[p1p2], N[p2p3],
		  N[m1], N[m2], N[m3], N[m4]}
:ArgumentTypes:	{Real, Real, Real, Real, Real, Real,
		  Real, Real, Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fcd0
:Pattern:	D0[p1_?complexQ, p2_?complexQ, p3_?complexQ, p4_?complexQ,
		  p1p2_?complexQ, p2p3_?complexQ,
		  m1_?complexQ, m2_?complexQ, m3_?complexQ, m4_?complexQ]
:Arguments:	{N[Re[p1]], N[Im[p1]], N[Re[p2]], N[Im[p2]],
		  N[Re[p3]], N[Im[p3]], N[Re[p4]], N[Im[p4]],
		  N[Re[p1p2]], N[Im[p1p2]], N[Re[p2p3]], N[Im[p2p3]],
		  N[Re[m1]], N[Im[m1]], N[Re[m2]], N[Im[m2]],
		  N[Re[m3]], N[Im[m3]], N[Re[m4]], N[Im[m4]]}
:ArgumentTypes:	{Real, Real, Real, Real, Real, Real, Real, Real,
		  Real, Real, Real, Real, Real, Real, Real, Real,
		  Real, Real, Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fd0i
:Pattern:	D0i[id_Symbol,
		  p1_?doubleQ, p2_?doubleQ, p3_?doubleQ, p4_?doubleQ,
		  p1p2_?doubleQ, p2p3_?doubleQ,
		  m1_?doubleQ, m2_?doubleQ, m3_?doubleQ, m4_?doubleQ]
:Arguments:	{id, N[p1], N[p2], N[p3], N[p4], N[p1p2], N[p2p3],
		  N[m1], N[m2], N[m3], N[m4]}
:ArgumentTypes:	{Symbol, Real, Real, Real, Real, Real, Real,
		  Real, Real, Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fcd0i
:Pattern:	D0i[id_Symbol,
		  p1_?complexQ, p2_?complexQ, p3_?complexQ, p4_?complexQ,
		  p1p2_?complexQ, p2p3_?complexQ,
		  m1_?complexQ, m2_?complexQ, m3_?complexQ, m4_?complexQ]
:Arguments:	{id, N[Re[p1]], N[Im[p1]], N[Re[p2]], N[Im[p2]],
		  N[Re[p3]], N[Im[p3]], N[Re[p4]], N[Im[p4]],
		  N[Re[p1p2]], N[Im[p1p2]], N[Re[p2p3]], N[Im[p2p3]],
		  N[Re[m1]], N[Im[m1]], N[Re[m2]], N[Im[m2]],
		  N[Re[m3]], N[Im[m3]], N[Re[m4]], N[Im[m4]]}
:ArgumentTypes:	{Symbol, Real, Real, Real, Real, Real, Real, Real, Real,
		  Real, Real, Real, Real, Real, Real, Real, Real,
		  Real, Real, Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fdget
:Pattern:	Dget[p1_?doubleQ, p2_?doubleQ, p3_?doubleQ, p4_?doubleQ,
		  p1p2_?doubleQ, p2p3_?doubleQ,
		  m1_?doubleQ, m2_?doubleQ, m3_?doubleQ, m4_?doubleQ]
:Arguments:	{N[p1], N[p2], N[p3], N[p4], N[p1p2], N[p2p3],
		  N[m1], N[m2], N[m3], N[m4]}
:ArgumentTypes:	{Real, Real, Real, Real, Real, Real, Real,
		  Real, Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fcdget
:Pattern:	Dget[p1_?complexQ, p2_?complexQ, p3_?complexQ, p4_?complexQ,
		  p1p2_?complexQ, p2p3_?complexQ,
		  m1_?complexQ, m2_?complexQ, m3_?complexQ, m4_?complexQ]
:Arguments:	{N[Re[p1]], N[Im[p1]], N[Re[p2]], N[Im[p2]],
		  N[Re[p3]], N[Im[p3]], N[Re[p4]], N[Im[p4]],
		  N[Re[p1p2]], N[Im[p1p2]], N[Re[p2p3]], N[Im[p2p3]],
		  N[Re[m1]], N[Im[m1]], N[Re[m2]], N[Im[m2]],
		  N[Re[m3]], N[Im[m3]], N[Re[m4]], N[Im[m4]]}
:ArgumentTypes:	{Real, Real, Real, Real, Real, Real, Real, Real,
		  Real, Real, Real, Real, Real, Real, Real, Real,
		  Real, Real, Real, Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fsetmudim
:Pattern:	SetMudim[newmudim_?doubleQ]
:Arguments:	{N[newmudim]}
:ArgumentTypes:	{Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	getmudim
:Pattern:	GetMudim[]
:Arguments:	{}
:ArgumentTypes:	{}
:ReturnType:	Real
:End:

:Begin:
:Function:	fsetlambda
:Pattern:	SetLambda[newlambda_?doubleQ]
:Arguments:	{N[newlambda]}
:ArgumentTypes:	{Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	getlambda
:Pattern:	GetLambda[]
:Arguments:	{}
:ArgumentTypes:	{}
:ReturnType:	Real
:End:

:Begin:
:Function:	fsetdelta
:Pattern:	SetDelta[newdelta_?doubleQ]
:Arguments:	{N[newdelta]}
:ArgumentTypes:	{Real}
:ReturnType:	Manual
:End:

:Begin:
:Function:	getdelta
:Pattern:	GetDelta[]
:Arguments:	{}
:ArgumentTypes:	{}
:ReturnType:	Real
:End:

:Begin:
:Function:	fgetcachelast
:Pattern:	GetCacheLast[buffer_Symbol]
:Arguments:	{buffer}
:ArgumentTypes:	{Symbol}
:ReturnType:	Manual
:End:

:Begin:
:Function:	fsetcachelast
:Pattern:	SetCacheLast[buffer_Symbol, offset_Integer]
:Arguments:	{buffer, offset}
:ArgumentTypes:	{Symbol, Integer}
:ReturnType:	Manual
:End:

:Evaluate: doubleQ = Head[# + 1.] === Real &

:Evaluate: complexQ = Head[# + 1. I] === Complex &

:Evaluate: A0[0] = 0

:Evaluate: LoopTools::undef = "Identifier `1` unknown."

:Evaluate: Derivative[1, 0, 0][B0] = DB0

:Evaluate: Derivative[1, 0, 0][B1] = DB1

:Evaluate: Derivative[1, 0, 0][B00] = DB00

:Evaluate: Derivative[1, 0, 0][B11] = DB11

:Evaluate: PaVe[i__Integer, p_List, {m1_, m2_, m3_, m4_}] := D0i[
	     StringJoin["dd", ToString/@ Sort[{i}]]//ToExpression,
	     Sequence@@ p, m1, m2, m3, m4]

:Evaluate: PaVe[i__Integer, p_List, {m1_, m2_, m3_}] := C0i[
	     StringJoin["cc", ToString/@ Sort[{i}]]//ToExpression,
	     Sequence@@ p, m1, m2, m3]

:Evaluate: End[]

:Evaluate: EndPackage[]


/*
	LoopTools.tm
		provides the LoopTools functions in Mathematica
		this file is part of LoopTools
		last modified 6 Dec 04 th
*/


#include <stdio.h>
#include <math.h>
#include <string.h>
#include <unistd.h>

#include "mathlink.h"
#ifndef MLCONST
#define MLCONST
#endif

#include "ltproto.h"

#ifdef HAVE_UNDERSCORE
#define flush flush_
#endif


int truestdout;

enum { ncids = 13 };
MLCONST char *cids[ncids] = {
  "cc0", "cc1", "cc2", "cc00", "cc11", "cc12", "cc22",
  "cc001", "cc002", "cc111", "cc112", "cc122", "cc222" };

enum { ndids = 46 };
MLCONST char *dids[ndids] = {
  "dd0", "dd1", "dd2", "dd3", "dd00", "dd11", "dd12", "dd13", "dd22",
  "dd23", "dd33", "dd001", "dd002", "dd003", "dd111", "dd112",
  "dd113", "dd122", "dd123", "dd133", "dd222", "dd223", "dd233",
  "dd333", "dd0000", "dd0011", "dd0012", "dd0013", "dd0022",
  "dd0023", "dd0033", "dd1111", "dd1112", "dd1113", "dd1122",
  "dd1123", "dd1133", "dd1222", "dd1223", "dd1233", "dd1333",
  "dd2222", "dd2223", "dd2233", "dd2333", "dd3333" };


void report_error(MLCONST char *id)
{
  MLPutFunction(stdlink, "CompoundExpression", 2);
  MLPutFunction(stdlink, "Message", 2);
  MLPutFunction(stdlink, "MessageName", 2);
  MLPutSymbol(stdlink, "LoopTools");
  MLPutString(stdlink, "undef");
  MLPutSymbol(stdlink, id);
  MLPutSymbol(stdlink, "$Failed");
  MLEndPacket(stdlink);
}


int resolve_id(MLCONST char *id, MLCONST char **ids, const int n)
{
  int i;

  for( i = 0; i < n;  )
    if( strcmp(id, ids[i++]) == 0 ) return i;
  report_error(id);
  return 0;
}


const dcomplex *resolve_cache(MLCONST char *name)
{
  if( strcmp(name, "Ccache") == 0 ) return Ccache;
  if( strcmp(name, "Dcache") == 0 ) return Dcache;
  if( strcmp(name, "CCcache") == 0 ) return CCcache;
  if( strcmp(name, "CDcache") == 0 ) return CDcache;
  report_error(name);
  return NULL;
}


void return_complex(const dcomplex c)
{
  { int fortranstdout = 6; flush(&fortranstdout); }
  fflush(stdout);
  dup2(truestdout, 1);

  if( c.im == 0 ) MLPutReal(stdlink, c.re);
  else {
    MLPutFunction(stdlink, "Complex", 2);
    MLPutReal(stdlink, c.re);
    MLPutReal(stdlink, c.im);
  }
  MLEndPacket(stdlink);
}


void return_list(const dcomplex *c, MLCONST char **ids, const int n)
{
  int i;

  { int fortranstdout = 6; flush(&fortranstdout); }
  dup2(truestdout, 1);
  fflush(stderr);

  MLPutFunction(stdlink, "List", n);
  for( i = 0; i < n; ++i ) {
    MLPutFunction(stdlink, "Rule", 2);
    MLPutSymbol(stdlink, ids[i]);
    if( c[i].im == 0 ) MLPutReal(stdlink, c[i].re);
    else {
      MLPutFunction(stdlink, "Complex", 2);
      MLPutReal(stdlink, c[i].re);
      MLPutReal(stdlink, c[i].im);
    }
  }
  MLEndPacket(stdlink);
}


void fa0(const double m)
{
  dcomplex result;

  dup2(2, 1);
  A0(&result, &m);
  return_complex(result);
}


void fca0(const double re_m, const double im_m)
{
  const dcomplex m = {re_m, im_m};
  dcomplex result;

  dup2(2, 1);
  CA0(&result, &m);
  return_complex(result);
}


void fb0(const double p, const double m1, const double m2)
{
  dcomplex result;

  dup2(2, 1);
  B0(&result, &p, &m1, &m2);
  return_complex(result);
}


void fcb0(const double re_p, const double im_p,
  const double re_m1, const double im_m1,
  const double re_m2, const double im_m2)
{
  const dcomplex p = {re_p, im_p};
  const dcomplex m1 = {re_m1, im_m1};
  const dcomplex m2 = {re_m2, im_m2};
  dcomplex result;

  dup2(2, 1);
  CB0(&result, &p, &m1, &m2);
  return_complex(result);
}


void fdb0(const double p, const double m1, const double m2)
{
  dcomplex result;

  dup2(2, 1);
  DB0(&result, &p, &m1, &m2);
  return_complex(result);
}


void fcdb0(const double re_p, const double im_p,
  const double re_m1, const double im_m1,
  const double re_m2, const double im_m2)
{
  const dcomplex p = {re_p, im_p};
  const dcomplex m1 = {re_m1, im_m1};
  const dcomplex m2 = {re_m2, im_m2};
  dcomplex result;

  dup2(2, 1);
  CDB0(&result, &p, &m1, &m2);
  return_complex(result);
}


void fb1(const double p, const double m1, const double m2)
{
  dcomplex result;

  dup2(2, 1);
  B1(&result, &p, &m1, &m2);
  return_complex(result);
}


void fcb1(const double re_p, const double im_p,
  const double re_m1, const double im_m1,
  const double re_m2, const double im_m2)
{
  const dcomplex p = {re_p, im_p};
  const dcomplex m1 = {re_m1, im_m1};
  const dcomplex m2 = {re_m2, im_m2};
  dcomplex result;

  dup2(2, 1);
  CB1(&result, &p, &m1, &m2);
  return_complex(result);
}


void fdb1(const double p, const double m1, const double m2)
{
  dcomplex result;

  dup2(2, 1);
  DB1(&result, &p, &m1, &m2);
  return_complex(result);
}


void fcdb1(const double re_p, const double im_p,
  const double re_m1, const double im_m1,
  const double re_m2, const double im_m2)
{
  const dcomplex p = {re_p, im_p};
  const dcomplex m1 = {re_m1, im_m1};
  const dcomplex m2 = {re_m2, im_m2};
  dcomplex result;

  dup2(2, 1);
  CDB1(&result, &p, &m1, &m2);
  return_complex(result);
}


void fb00(const double p, const double m1, const double m2)
{
  dcomplex result;

  dup2(2, 1);
  B00(&result, &p, &m1, &m2);
  return_complex(result);
}


void fcb00(const double re_p, const double im_p,
  const double re_m1, const double im_m1,
  const double re_m2, const double im_m2)
{
  const dcomplex p = {re_p, im_p};
  const dcomplex m1 = {re_m1, im_m1};
  const dcomplex m2 = {re_m2, im_m2};
  dcomplex result;

  dup2(2, 1);
  CB00(&result, &p, &m1, &m2);
  return_complex(result);
}


void fdb00(const double p, const double m1, const double m2)
{
  dcomplex result;

  dup2(2, 1);
  DB00(&result, &p, &m1, &m2);
  return_complex(result);
}


void fcdb00(const double re_p, const double im_p,
  const double re_m1, const double im_m1,
  const double re_m2, const double im_m2)
{
  const dcomplex p = {re_p, im_p};
  const dcomplex m1 = {re_m1, im_m1};
  const dcomplex m2 = {re_m2, im_m2};
  dcomplex result;

  dup2(2, 1);
  CDB00(&result, &p, &m1, &m2);
  return_complex(result);
}


void fb11(const double p, const double m1, const double m2)
{
  dcomplex result;

  dup2(2, 1);
  B11(&result, &p, &m1, &m2);
  return_complex(result);
}


void fcb11(const double re_p, const double im_p,
  const double re_m1, const double im_m1,
  const double re_m2, const double im_m2)
{
  const dcomplex p = {re_p, im_p};
  const dcomplex m1 = {re_m1, im_m1};
  const dcomplex m2 = {re_m2, im_m2};
  dcomplex result;

  dup2(2, 1);
  CB11(&result, &p, &m1, &m2);
  return_complex(result);
}


void fdb11(const double p, const double m1, const double m2)
{
  dcomplex result;

  dup2(2, 1);
  DB11(&result, &p, &m1, &m2);
  return_complex(result);
}


void fcdb11(const double re_p, const double im_p,
  const double re_m1, const double im_m1,
  const double re_m2, const double im_m2)
{
  const dcomplex p = {re_p, im_p};
  const dcomplex m1 = {re_m1, im_m1};
  const dcomplex m2 = {re_m2, im_m2};
  dcomplex result;

  dup2(2, 1);
  CDB11(&result, &p, &m1, &m2);
  return_complex(result);
}


void fc0(const double p1, const double p2, const double p1p2,
  const double m1, const double m2, const double m3)
{
  dcomplex result;

  dup2(2, 1);
  C0(&result, &p1, &p2, &p1p2, &m1, &m2, &m3);
  return_complex(result);
}


void fcc0(const double re_p1, const double im_p1,
  const double re_p2, const double im_p2,
  const double re_p1p2, const double im_p1p2,
  const double re_m1, const double im_m1,
  const double re_m2, const double im_m2,
  const double re_m3, const double im_m3)
{
  const dcomplex p1 = {re_p1, im_p1};
  const dcomplex p2 = {re_p2, im_p2};
  const dcomplex p1p2 = {re_p1p2, im_p1p2};
  const dcomplex m1 = {re_m1, im_m1};
  const dcomplex m2 = {re_m2, im_m2};
  const dcomplex m3 = {re_m3, im_m3};
  dcomplex result;

  dup2(2, 1);
  CC0(&result, &p1, &p2, &p1p2, &m1, &m2, &m3);
  return_complex(result);
}


void fc0i(MLCONST char *id,
  const double p1, const double p2, const double p1p2,
  const double m1, const double m2, const double m3)
{
  const int i = resolve_id(id, cids, ncids);
  dcomplex result;

  if( i ) {
    dup2(2, 1);
    C0i(&result, &i, &p1, &p2, &p1p2, &m1, &m2, &m3);
    return_complex(result);
  }
}


void fcc0i(MLCONST char *id,
  const double re_p1, const double im_p1,
  const double re_p2, const double im_p2,
  const double re_p1p2, const double im_p1p2,
  const double re_m1, const double im_m1,
  const double re_m2, const double im_m2,
  const double re_m3, const double im_m3)
{
  const dcomplex p1 = {re_p1, im_p1};
  const dcomplex p2 = {re_p2, im_p2};
  const dcomplex p1p2 = {re_p1p2, im_p1p2};
  const dcomplex m1 = {re_m1, im_m1};
  const dcomplex m2 = {re_m2, im_m2};
  const dcomplex m3 = {re_m3, im_m3};
  const int i = resolve_id(id, cids, ncids);
  dcomplex result;

  if( i ) {
    dup2(2, 1);
    CC0i(&result, &i, &p1, &p2, &p1p2, &m1, &m2, &m3);
    return_complex(result);
  }
}


void fcget(const double p1, const double p2, const double p1p2,
  const double m1, const double m2, const double m3)
{
  int offset;

  dup2(2, 1);
  offset = Cget(&p1, &p2, &p1p2, &m1, &m2, &m3);
  return_list(Ccache + offset, cids, ncids);
}


void fccget(const double re_p1, const double im_p1,
  const double re_p2, const double im_p2,
  const double re_p1p2, const double im_p1p2,
  const double re_m1, const double im_m1,
  const double re_m2, const double im_m2,
  const double re_m3, const double im_m3)
{
  const dcomplex p1 = {re_p1, im_p1};
  const dcomplex p2 = {re_p2, im_p2};
  const dcomplex p1p2 = {re_p1p2, im_p1p2};
  const dcomplex m1 = {re_m1, im_m1};
  const dcomplex m2 = {re_m2, im_m2};
  const dcomplex m3 = {re_m3, im_m3};
  int offset;

  dup2(2, 1);
  offset = CCget(&p1, &p2, &p1p2, &m1, &m2, &m3);
  return_list(CCcache + offset, cids, ncids);
}


void fd0(const double p1, const double p2, const double p3,
  const double p4, const double p1p2, const double p2p3,
  const double m1, const double m2, const double m3, const double m4)
{
  dcomplex result;

  dup2(2, 1);
  D0(&result, &p1, &p2, &p3, &p4, &p1p2, &p2p3, &m1, &m2, &m3, &m4);
  return_complex(result);
}


void fcd0(const double re_p1, const double im_p1,
  const double re_p2, const double im_p2,
  const double re_p3, const double im_p3,
  const double re_p4, const double im_p4,
  const double re_p1p2, const double im_p1p2,
  const double re_p2p3, const double im_p2p3,
  const double re_m1, const double im_m1,
  const double re_m2, const double im_m2,
  const double re_m3, const double im_m3,
  const double re_m4, const double im_m4)
{
  const dcomplex p1 = {re_p1, im_p1};
  const dcomplex p2 = {re_p2, im_p2};
  const dcomplex p3 = {re_p3, im_p3};
  const dcomplex p4 = {re_p4, im_p4};
  const dcomplex p1p2 = {re_p1p2, im_p1p2};
  const dcomplex p2p3 = {re_p2p3, im_p2p3};
  const dcomplex m1 = {re_m1, im_m1};
  const dcomplex m2 = {re_m2, im_m2};
  const dcomplex m3 = {re_m3, im_m3};
  const dcomplex m4 = {re_m4, im_m4};
  dcomplex result;

  dup2(2, 1);
  CD0(&result, &p1, &p2, &p3, &p4, &p1p2, &p2p3, &m1, &m2, &m3, &m4);
  return_complex(result);
}


void fd0i(MLCONST char *id,
  const double p1, const double p2, const double p3,
  const double p4, const double p1p2, const double p2p3,
  const double m1, const double m2, const double m3, const double m4)
{
  const int i = resolve_id(id, dids, ndids);
  dcomplex result;

  if( i ) {
    dup2(2, 1);
    D0i(&result, &i, &p1, &p2, &p3, &p4, &p1p2, &p2p3,
      &m1, &m2, &m3, &m4);
    return_complex(result);
  }
}


void fcd0i(MLCONST char *id,
  const double re_p1, const double im_p1,
  const double re_p2, const double im_p2,
  const double re_p3, const double im_p3,
  const double re_p4, const double im_p4,
  const double re_p1p2, const double im_p1p2,
  const double re_p2p3, const double im_p2p3,
  const double re_m1, const double im_m1,
  const double re_m2, const double im_m2,
  const double re_m3, const double im_m3,
  const double re_m4, const double im_m4)
{
  const dcomplex p1 = {re_p1, im_p1};
  const dcomplex p2 = {re_p2, im_p2};
  const dcomplex p3 = {re_p3, im_p3};
  const dcomplex p4 = {re_p4, im_p4};
  const dcomplex p1p2 = {re_p1p2, im_p1p2};
  const dcomplex p2p3 = {re_p2p3, im_p2p3};
  const dcomplex m1 = {re_m1, im_m1};
  const dcomplex m2 = {re_m2, im_m2};
  const dcomplex m3 = {re_m3, im_m3};
  const dcomplex m4 = {re_m4, im_m4};
  const int i = resolve_id(id, dids, ndids);
  dcomplex result;

  if( i ) {
    dup2(2, 1);
    CD0i(&result, &i, &p1, &p2, &p3, &p4, &p1p2, &p2p3,
      &m1, &m2, &m3, &m4);
    return_complex(result);
  }
}


void fdget(const double p1, const double p2, const double p3,
  const double p4, const double p1p2, const double p2p3,
  const double m1, const double m2, const double m3, const double m4)
{
  int offset;

  dup2(2, 1);
  offset = Dget(&p1, &p2, &p3, &p4, &p1p2, &p2p3, &m1, &m2, &m3, &m4);
  return_list(Dcache + offset, dids, ndids);
}


void fcdget(const double re_p1, const double im_p1,
  const double re_p2, const double im_p2,
  const double re_p3, const double im_p3,
  const double re_p4, const double im_p4,
  const double re_p1p2, const double im_p1p2,
  const double re_p2p3, const double im_p2p3,
  const double re_m1, const double im_m1,
  const double re_m2, const double im_m2,
  const double re_m3, const double im_m3,
  const double re_m4, const double im_m4)
{
  const dcomplex p1 = {re_p1, im_p1};
  const dcomplex p2 = {re_p2, im_p2};
  const dcomplex p3 = {re_p3, im_p3};
  const dcomplex p4 = {re_p4, im_p4};
  const dcomplex p1p2 = {re_p1p2, im_p1p2};
  const dcomplex p2p3 = {re_p2p3, im_p2p3};
  const dcomplex m1 = {re_m1, im_m1};
  const dcomplex m2 = {re_m2, im_m2};
  const dcomplex m3 = {re_m3, im_m3};
  const dcomplex m4 = {re_m4, im_m4};
  int offset;

  dup2(2, 1);
  offset = CDget(&p1, &p2, &p3, &p4, &p1p2, &p2p3, &m1, &m2, &m3, &m4);
  return_list(CDcache + offset, dids, ndids);
}


void fsetmudim(const double newmudim2)
{
  setmudim(&newmudim2);
  MLPutSymbol(stdlink, "Null");
  MLEndPacket(stdlink);
}


void fsetdelta(const double newdivergence)
{
  setdelta(&newdivergence);
  MLPutSymbol(stdlink, "Null");
  MLEndPacket(stdlink);
}


void fsetlambda(const double newlambda2)
{
  setlambda(&newlambda2);
  MLPutSymbol(stdlink, "Null");
  MLEndPacket(stdlink);
}


void fgetcachelast(MLCONST char *cache)
{
  const dcomplex *buf = resolve_cache(cache);

  if( buf ) {
    MLPutInteger(stdlink, getcachelast(buf));
    MLEndPacket(stdlink);
  }
}


void fsetcachelast(MLCONST char *cache, const int offset)
{
  const dcomplex *buf = resolve_cache(cache);

  if( buf ) {
    setcachelast(buf, &offset);
    MLPutSymbol(stdlink, "Null");
    MLEndPacket(stdlink);
  }
}


int main(int argc, char **argv)
{
  int ret;

  truestdout = dup(1);
  dup2(2, 1);
  ffini();
  dup2(truestdout, 1);

  ret = MLMain(argc, argv);

  dup2(2, 1);
  ffexi();
  dup2(truestdout, 1);
  fflush(stderr);

  return ret;
}

