#include "LKH.h"
#include "GainType.h"
#include "Heap.h"
#include "GeoConversion.h"

/*
 * Given a tour and a partitioning of the problem into subproblems, the 
 * SolveSubproblemBorderProblems function attempt to improve the tour by 
 * means of a new partitioning given by the borders of these subproblems.
 *
 * For each of the original subproblems a new subproblem is defined by
 * the SubproblemSize points that are closest to its windowing border.
 * These border points together with the given tour induces a subproblem 
 * consisting of all border points, and with edges fixed between points 
 * that are connected by tour segments whose interior points are outside 
 * the border.  
 *  
 * If an improvement is found, the new tour is written to TourFile. 
 * The original tour is given by the SubproblemSuc references of the nodes.
 *
 * The parameter Subproblems specifies the number of subproblems.
 * The parameter GlobalBestCost references the current best cost of the
 * whole problem.
 */

static void MarkBorderPoints(int CurrentSubproblem);
static void QuickSelect(Node ** A, int Low, int High, int k);

void SolveSubproblemBorderProblems(int Subproblems, GainType * GlobalBestCost)
{
    Node *N;
    GainType OldGlobalBestCost;
    int CurrentSubproblem, RestrictedSearchSaved = RestrictedSearch;
    int *SubproblemSaved;
    double EntryTime = GetTime();

    assert(SubproblemSaved =
	   (int *) malloc((DimensionSaved + 1) * sizeof(int)));
    /* Compute upper bound for the original problem */
    N = FirstNode;
    do {
	N->Suc = N->SubproblemSuc;
	N->Suc->Pred = N;
	if (N->Subproblem > Subproblems)
	    N->Subproblem -= Subproblems;
	SubproblemSaved[N->Id] = N->Subproblem;
        N->FixedTo1Saved = N->FixedTo2Saved = 0;
	N->SubBestPred = N->SubBestSuc = 0;
    }
    while ((N = N->SubproblemSuc) != FirstNode);
    if (TraceLevel >= 1)
	printff("\n*** Solve subproblem border problems *** [" GainFormat
		"]\n", *GlobalBestCost);
    for (CurrentSubproblem = 1;
	 CurrentSubproblem <= Subproblems; CurrentSubproblem++) {
	MarkBorderPoints(CurrentSubproblem);
	OldGlobalBestCost = *GlobalBestCost;
	SolveSubproblem(CurrentSubproblem, Subproblems, GlobalBestCost);
	if (SubproblemsCompressed && *GlobalBestCost == OldGlobalBestCost) {
	    if (TraceLevel >= 1)
		printff("\nCompress subproblem %d:\n", CurrentSubproblem);
	    RestrictedSearch = 0;
	    SolveSubproblem(CurrentSubproblem, Subproblems, GlobalBestCost);
	    RestrictedSearch = RestrictedSearchSaved;
	}
	N = FirstNode;
	do
	    N->Subproblem = SubproblemSaved[N->Id];
	while ((N = N->SubproblemSuc) != FirstNode);
    }
    free(SubproblemSaved);
    printff("\nCost = " GainFormat, *GlobalBestCost);
    if (Optimum != MINUS_INFINITY && Optimum != 0)
	printff(", Gap = %0.3f%%",
		100.0 * (*GlobalBestCost - Optimum) / Optimum);
    printff(", Time = %0.1f sec. %s\n", fabs(GetTime() - EntryTime),
	    *GlobalBestCost < Optimum ? "<" : *GlobalBestCost ==
	    Optimum ? "=" : "");
}

#define Coord(N, axis) (axis == 0 ? (N)->X : axis == 1 ? (N)->Y : (N)->Z)

/*
 * The MarkBorderPoints function marks the border points of a given
 * subproblem (Subproblem >= 1) by setting their Subproblem value to
 * Subproblem. All other points are given a Subproblem value of 0.
 */

static void MarkBorderPoints(int CurrentSubproblem)
{
    double Min[3], Max[3];
    int dMin, dMax, d, i, axis, ActualSubproblemSize = 0, Size = 0;
    Node **A, *N;

    assert(A = (Node **) malloc(DimensionSaved * sizeof(Node *)));
    Min[0] = Min[1] = Min[2] = DBL_MAX;
    Max[0] = Max[1] = Max[2] = -DBL_MAX;
    if (WeightType == GEO || WeightType == GEOM ||
	WeightType == GEO_MEEUS || WeightType == GEOM_MEEUS) {
	N = FirstNode;
	do {
	    N->Xc = N->X;
	    N->Yc = N->Y;
	    N->Zc = N->Z;
	    if (WeightType == GEO || WeightType == GEO_MEEUS)
		GEO2XYZ(N->Xc, N->Yc, &N->X, &N->Y, &N->Z);
	    else
		GEOM2XYZ(N->Xc, N->Yc, &N->X, &N->Y, &N->Z);
	} while ((N = N->SubproblemSuc) != FirstNode);
	CoordType = THREED_COORDS;
    }
    N = FirstNode;
    do {
	if (N->Subproblem == CurrentSubproblem) {
	    for (i = CoordType == THREED_COORDS ? 2 : 1; i >= 0; i--) {
		if (Coord(N, i) < Min[i])
		    Min[i] = Coord(N, i);
		if (Coord(N, i) > Max[i])
		    Max[i] = Coord(N, i);
	    }
	    ActualSubproblemSize++;
	}
    } while ((N = N->SubproblemSuc) != FirstNode);
    do {
	if (N->Subproblem == CurrentSubproblem ||
	    (N->X >= Min[0] && N->X <= Max[0] &&
	     N->Y >= Min[1] && N->Y <= Max[1] &&
	     (CoordType == TWOD_COORDS ||
	      (N->Z >= Min[2] && N->Z <= Max[2])))) {
	    N->Rank = INT_MAX;
	    for (i = CoordType == THREED_COORDS ? 2 : 1; i >= 0; i--) {
		dMin = (int) (fabs(Coord(N, i) - Min[i]) + 0.5);
		dMax = (int) (fabs(Coord(N, i) - Max[i]) + 0.5);
		d = dMin < dMax ? dMin : dMax;
		if (d < N->Rank)
		    N->Rank = d;
	    }
	} else {
	    axis = -1;
	    if (CoordType == TWOD_COORDS) {
		if (N->X >= Min[0] && N->X <= Max[0])
		    axis = 1;
		else if (N->Y >= Min[1] && N->Y <= Max[1])
		    axis = 0;
	    } else if (N->X >= Min[0] && N->X <= Max[0]) {
		if (N->Y >= Min[1] && N->Y <= Max[1])
		    axis = 2;
		else if (N->Z >= Min[2] && N->Z <= Max[2])
		    axis = 1;
	    } else if (N->Y >= Min[1] && N->Y <= Max[1] &&
		       N->Z >= Min[2] && N->Z <= Max[2])
		axis = 0;
	    if (axis != -1) {
		dMin = (int) (fabs(Coord(N, axis) - Min[axis]) + 0.5);
		dMax = (int) (fabs(Coord(N, axis) - Max[axis]) + 0.5);
		N->Rank = dMin < dMax ? dMin : dMax;
	    } else {
		N->Rank = 0;
		for (i = CoordType == THREED_COORDS ? 2 : 1; i >= 0; i--) {
		    dMin = (int) (fabs(Coord(N, i) - Min[i]) + 0.5);
		    dMax = (int) (fabs(Coord(N, i) - Max[i]) + 0.5);
		    d = dMin < dMax ? dMin : dMax;
		    if (d > N->Rank)
			N->Rank = d;
		}
	    }
	}
	N->Subproblem = 0;
	if (!SubproblemsCompressed ||
	    ((N->SubproblemPred != N->SubBestPred ||
	      N->SubproblemSuc != N->SubBestSuc) &&
	     (N->SubproblemPred != N->SubBestSuc ||
	      N->SubproblemSuc != N->SubBestPred)))
	    A[Size++] = N;
    } while ((N = N->SubproblemSuc) != FirstNode);
    if (ActualSubproblemSize > Size)
	ActualSubproblemSize = Size;
    else
	QuickSelect(A, 0, Size - 1, ActualSubproblemSize);
    for (Size = 0; Size < ActualSubproblemSize; Size++)
	A[Size]->Subproblem = CurrentSubproblem;
    free(A);
    if (WeightType == GEO || WeightType == GEOM ||
	WeightType == GEO_MEEUS || WeightType == GEOM_MEEUS) {
	N = FirstNode;
	do {
	    N->X = N->Xc;
	    N->Y = N->Yc;
	    N->Z = N->Zc;
	} while ((N = N->SubproblemSuc) != FirstNode);
	CoordType = TWOD_COORDS;
    }
}

/* 
 * The QuickSelect function partitions the array A about the k-th
 * smallest element. A[i]->Rank contains the value of A[i].
 */

static void QuickSelect(Node ** A, int Low, int High, int k)
{
    while (Low < High) {
	int V = A[High]->Rank;
	Node *Temp;
	int i = Low - 1, j = High;
	while (1) {
	    while (A[++i]->Rank < V);
	    while (V < A[--j]->Rank)
		if (j == Low)
		    break;
	    if (i >= j)
		break;
	    Temp = A[i];
	    A[i] = A[j];
	    A[j] = Temp;
	}
	Temp = A[i];
	A[i] = A[High];
	A[High] = Temp;
	if (i >= k)
	    High = i - 1;
	if (i <= k)
	    Low = i + 1;
    }
}
