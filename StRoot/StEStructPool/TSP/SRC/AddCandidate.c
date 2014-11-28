#include "LKH.h"

/*
 * The AddCandidate function adds a given edge (From, To) to the set
 * of candidate edges associated with the node From. The cost and 
 * alpha-value of the edge are passed as parameters to the function.
 *
 * The function has no effect if the edge is already in the candidate
 * set.
 *
 * If the edge was added, the function returns 1; otherwise 0.
 *    
 * The function is called from the functions CreateDelaunaySet and
 * OrderCandidateSet.
 */

static int PossibleCandidate(Node * From, Node * To);

int AddCandidate(Node * From, Node * To, int Cost, int Alpha)
{
    int Count;
    Candidate *NFrom;

    if (From->Subproblem != FirstNode->Subproblem)
        return 0;
    if (From->CandidateSet == 0)
        assert(From->CandidateSet =
               (Candidate *) calloc(3, sizeof(Candidate)));
    if (From == To || To->Subproblem != FirstNode->Subproblem ||
        !PossibleCandidate(From, To))
        return 0;
    Count = 0;
    for (NFrom = From->CandidateSet; NFrom->To && NFrom->To != To; NFrom++)
        Count++;
    if (NFrom->To)
        return 0;
    NFrom->Cost = Cost;
    NFrom->Alpha = Alpha;
    NFrom->To = To;
    assert(From->CandidateSet =
           (Candidate *) realloc(From->CandidateSet,
                                 (Count + 2) * sizeof(Candidate)));
    From->CandidateSet[Count + 1].To = 0;
    return 1;
}

/* 
 * The PossibleCandidate function is used to test if an edge, (From,To), 
 * may be a solution edge together with all fixed or common edges.  
 *
 * If the edge is possible, the function returns 1; otherwise 0.
 */

static int PossibleCandidate(Node * From, Node * To)
{
    Node *Na, *Nb, *Nc, *N;
    Candidate *NN;
    int Count, i;

    if (InInitialTour(From, To) ||
        From->SubproblemSuc == To || To->SubproblemSuc == From ||
        FixedOrCommon(From, To))
        return 1;
    if (From->FixedTo2 || To->FixedTo2)
        return 0;
    if (MergeTourFiles < 2)
        return 1;
    if (!From->Head) {
        Nb = FirstNode;
        do {
            Na = Nb;
            Nb = Na->MergeSuc[0];
        } while (Nb != FirstNode && FixedOrCommon(Na, Nb));
        if (Nb != FirstNode) {
            N = Nb;
            do {
                Nc = Nb;
                do {
                    Na = Nb;
                    Na->Head = Nc;
                    Nb = Na->MergeSuc[0];
                } while (FixedOrCommon(Na, Nb));
                do
                    Nc->Tail = Na;
                while ((Nc = Nc->MergeSuc[0]) != Nb);
            } while (Nb != N);
        } else {
            do
                Nb->Head = Nb->Tail = FirstNode;
            while ((Nb = Nb->Suc) != FirstNode);
        }
    }
    if (From->Head == To->Head ||
        (From->Head != From && From->Tail != From) ||
        (To->Head != To && To->Tail != To))
        return 0;
    for (i = 1; i <= 2; i++) {
        N = i == 1 ? From : To;
        Count = 0;
        for (NN = N->CandidateSet; NN && NN->To; NN++)
            if (FixedOrCommon(N, NN->To))
                if (++Count == 2)
                    return 0;
    }
    return 1;
}
