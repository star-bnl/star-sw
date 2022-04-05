#include "LKH.h"
#include "Delaunay.h"

/*
 * The CreateDelaunayCandidateSet function determines for each node its set 
 * of incident candidate edges. The edges are found by Delaunay triangulation. 
 *
 * The function is called from CreateCandidateSet.
 */

static int Level = 0;

void CreateDelaunayCandidateSet()
{
    edge_ref *edges, e;
    Node *From, *To;
    int d, i;

    if (TraceLevel >= 2)
        printff("Creating Delaunay candidate set ... ");
    AddTourCandidates();

    if (MaxCandidates == 0) {
        From = FirstNode;
        do {
            if (!From->CandidateSet)
                eprintf("MAX_CANDIDATES = 0: No candidates");
        } while ((From = From->Suc) != FirstNode);
        if (TraceLevel >= 2)
            printff("done\n");
        return;
    }

    /* Find the Delaunay edges */
    edges = delaunay_edges(Dimension);

    /* Add the Delaunay edges to the candidate set */
    for (i = 0; edges && (e = edges[i]); i++) {
        From = (Node *) ODATA(e);
        To = (Node *) DDATA(e);
        destroy_edge(e);
        d = D(From, To);
        AddCandidate(From, To, d, 1);
        AddCandidate(To, From, d, 1);
    }
    free(edges);
    if (Level == 0 && (WeightType == GEO || WeightType == GEOM ||
                       WeightType == GEO_MEEUS
                       || WeightType == GEOM_MEEUS)) {
        if (TraceLevel >= 2)
            printff("done\n");
        From = FirstNode;
        while ((From = From->Suc) != FirstNode)
            if ((From->Y > 0) != (FirstNode->Y > 0))
                break;
        if (From != FirstNode) {
            /* Transform longitude (180 and -180 map to 0) */
            From = FirstNode;
            do {
                From->Zc = From->Y;
                From->Y += From->Y > 0 ? -180 : 180;
            } while ((From = From->Suc) != FirstNode);
            Level++;
            CreateDelaunayCandidateSet();
            Level--;
            From = FirstNode;
            do
                From->Y = From->Zc;
            while ((From = From->Suc) != FirstNode);
        }
    }
    if (Level == 0) {
        /* Add quadrant neighbors if any node has less than two candidates. 
           That is, if it should happen that delaunay_edges fails. */
        From = FirstNode;
        do {
            if (From->CandidateSet == 0 ||
                From->CandidateSet[0].To == 0
                || From->CandidateSet[1].To == 0) {
                if (TraceLevel >= 2)
                    printff("not complete\n");
                AddExtraCandidates(CoordType == THREED_COORDS ? 8 : 4,
                                   QUADRANT, 1);
                break;
            }
        } while ((From = From->Suc) != FirstNode);
        if (TraceLevel >= 2)
            printff("done\n");
    }
}
