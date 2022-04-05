/**********************************************************************
 *
 * $Id: StarTSP.cxx,v 1.2 2011/08/02 20:43:55 prindle Exp $
 *
 * Author: Duncan Prindle
 *
 **********************************************************************
 *
 * Description:  Class to create an index for reading MuDst events so
 *               consecutive events are close in (multiplicity,zvertex).
 *
 ***********************************************************************/
extern "C" {
    #include "SRC/INCLUDE/LKH.h"
    #include "SRC/INCLUDE/Genetic.h"
    #include "SRC/INCLUDE/Heap.h"
}
extern "C" {
    void EStructTSP_CreateNodes();
}
#include "StarTSP.h"

ClassImp(StarTSP);

StarTSP::StarTSP(int iDimension) {
    dimension = iDimension;
    mMultScale =  1.0;
    mZScale    = 10.0;
    mZDCScale  =  0.01;

    // Following are variables from our TSP solver.
    // Default values first.
    ProblemFileName = PiFileName = InputTourFileName =
        OutputTourFileName = TourFileName = 0;
    CandidateFiles = MergeTourFiles = 0;
    AscentCandidates = 50;
    BackboneTrials = 0;
    Backtracking = 0;
    CandidateSetSymmetric = 0;
    CandidateSetType = ALPHA;
    Crossover = ERXT;
    DelaunayPartitioning = 0;
    DelaunayPure = 0;
    Excess = -1;
    ExtraCandidates = 0;
    ExtraCandidateSetSymmetric = 0;
    ExtraCandidateSetType = QUADRANT;
    Gain23Used = 1;
    GainCriterionUsed = 1;
    InitialPeriod = -1;
    InitialStepSize = 0;
    InitialTourAlgorithm = WALK;
    InitialTourFraction = 1.0;
    KarpPartitioning = 0;
    KMeansPartitioning = 0;
    Kicks = 1;
    KickType = 0;
    MaxBreadth = INT_MAX;
    MaxCandidates = 5;
    MaxPopulationSize = 0;
    MaxSwaps = -1;
    MaxTrials = -1;
    MoorePartitioning = 0;
    MoveType = 5;
    NonsequentialMoveType = -1;
    Optimum = MINUS_INFINITY;
    PatchingA = 1;
    PatchingC = 0;
    PatchingAExtended = 0;
    PatchingARestricted = 0;
    PatchingCExtended = 0;
    PatchingCRestricted = 0;
    Precision = 100;
    RestrictedSearch = 1;
    RohePartitioning = 0;
    Runs = 0;
    Seed = 1;
    SierpinskiPartitioning = 0;
    StopAtOptimum = 1;
    Subgradient = 1;
    SubproblemBorders = 0;
    SubproblemsCompressed = 0;
    SubproblemSize = 0;
    SubsequentMoveType = 0;
    SubsequentPatching = 1;
    TimeLimit = DBL_MAX;
    TraceLevel = 1;

    
    // For faster (approximate) solution I modify a few of the parameters.
    Runs = 1;
    AscentCandidates = 15;
    InitialPeriod = 150;
    Subgradient = 0;
    TimeLimit = 240;
// Next are under testing
    PatchingA = 2;
    PatchingC = 3;
    MoveType = 5;
}
StarTSP::~StarTSP() {
}

int* StarTSP::getFileNumbers(TChain *ch, int nGood, int *index) {

    // Get number of events in each file, total number of events.
    // Seems that depending on how files are entered into TChain root may not be honest about
    // how many entries are in an element. Try forcing it the ch->GetEntries.
    int nEntries = ch->GetEntries();
    TObjArray *fFiles = ch->GetListOfFiles();
    int nFiles = fFiles->GetEntries();
    int *jOffset = (int *) malloc(sizeof(int)*nFiles);
    int jf=0;
    int ntot = 0;
    TChainElement *element;
    TIter next(fFiles);
    while ((element = (TChainElement *) next())) {
        ntot += element->GetEntries();
        jOffset[jf] = ntot;
        jf++;
    }
    // Now jOffset[if] is number of events in all files before file if.
    int   *F = (int   *) malloc(sizeof(int)*nEntries);
    jf = 0;
    for (int ig=0;ig<nGood;ig++) {
        if (index[ig] > jOffset[jf]) {
            jf++;
        }
        F[ig] = jf;
    }
    return F;
}

// This code is essentially LKHmain.c
// I have in-lined SRC/ReadProblem.c and 
int* StarTSP::sortLists(float *mult, float *z, float *coincidence) {
    // Normally fileIndex is file number + (event number in chain / 10^6).
    // Create combination outside of this method for run-time flexibility.
    GainType Cost;
    double Time, LastTime = GetTime();

    MaxMatrixDimension = 10000;

    // Here is code from SRC/ReadProblem.c
    int i, K;

    if (TraceLevel >= 1)
        printff("TSP Problem generated is StarTSP\n");
    FreeStructures();
    FirstNode = 0;
    WeightType = WeightFormat = -1;
    CoordType = NO_COORDS;
    Name = "Unnamed";
    Type = EdgeWeightType = EdgeWeightFormat = 0;
    EdgeDataFormat = NodeCoordType = DisplayDataType = 0;
    Distance = 0;
    C = 0;
    c = 0;

    // Here is code from SRC/ReadProblem.c
    Name = "";
    Dimension = dimension;
    DimensionSaved = dimension;
    ProblemType = TSP;
    WeightType = SPECIAL;
    Distance = Distance_SPECIAL;
    CoordType = THREED_COORDS;

    // Read_NODE_COORD_SECTION();
    Node *N;
    EStructTSP_CreateNodes();
    N = FirstNode;
    for (int i = 1; i <= Dimension; i++) {
        N = &NodeSet[i];
        N->V = 1;
        N->X = mMultScale*mult[i-1];
        N->Y = mZScale*z[i-1];
        N->Z = mZDCScale*coincidence[i-1];
    }
    // end of Read_NODE_COORD_SECTION();

    Swaps = 0;

    /* Adjust parameters */
    if (Seed == 0)
        Seed = 1;
    if (Precision == 0)
        Precision = 100;
    if (InitialStepSize == 0)
        InitialStepSize = 1;
    if (MaxSwaps < 0)
        MaxSwaps = Dimension;
    if (KickType > Dimension / 2)
        KickType = Dimension / 2;
    if (Runs == 0)
        Runs = 10;
    if (MaxCandidates > Dimension - 1)
        MaxCandidates = Dimension - 1;
    if (ExtraCandidates > Dimension - 1)
        ExtraCandidates = Dimension - 1;
    if (SubproblemSize >= Dimension)
        SubproblemSize = Dimension;
    else if (SubproblemSize == 0) {
        if (AscentCandidates > Dimension - 1)
            AscentCandidates = Dimension - 1;
        if (InitialPeriod < 0) {
            InitialPeriod = Dimension / 2;
            if (InitialPeriod < 100)
                InitialPeriod = 100;
        }
        if (Excess < 0)
            Excess = 1.0 / Dimension;
        if (MaxTrials == -1)
            MaxTrials = Dimension;
        MakeHeap(Dimension);
    }

    if (CostMatrix == 0 && Dimension <= MaxMatrixDimension && Distance != 0
        && Distance != Distance_1 && Distance != Distance_ATSP &&
        WeightType != GEO && WeightType != GEOM &&
        WeightType != GEO_MEEUS && WeightType != GEOM_MEEUS) {
        Node *Ni, *Nj;
        assert(CostMatrix =
               (int *) calloc(Dimension * (Dimension - 1) / 2,
                              sizeof(int)));
        Ni = FirstNode->Suc;
        do {
            Ni->C = &CostMatrix[(Ni->Id - 1) * (Ni->Id - 2) / 2] - 1;
            if (ProblemType != HPP || Ni->Id < Dimension)
                for (Nj = FirstNode; Nj != Ni; Nj = Nj->Suc)
                    Ni->C[Nj->Id] = Fixed(Ni, Nj) ? 0 : Distance(Ni, Nj);
            else
                for (Nj = FirstNode; Nj != Ni; Nj = Nj->Suc)
                    Ni->C[Nj->Id] = 0;
        }
        while ((Ni = Ni->Suc) != FirstNode);
        WeightType = EXPLICIT;
        c = 0;
    }
    if (Precision > 1 && (WeightType == EXPLICIT || ProblemType == ATSP)) {
        int j, n = ProblemType == ATSP ? Dimension / 2 : Dimension;
        for (i = 2; i <= n; i++) {
            Node *N = &NodeSet[i];
            for (j = 1; j < i; j++)
                if (N->C[j] * Precision / Precision != N->C[j])
                    eprintf("PRECISION (= %d) is too large", Precision);
        }
    }
    C = WeightType == EXPLICIT ? C_EXPLICIT : C_FUNCTION;
    D = WeightType == EXPLICIT ? D_EXPLICIT : D_FUNCTION;
    if (SubsequentMoveType == 0)
        SubsequentMoveType = MoveType;
    K = MoveType >= SubsequentMoveType
        || !SubsequentPatching ? MoveType : SubsequentMoveType;
    if (PatchingC > K)
        PatchingC = K;
    if (PatchingA > 1 && PatchingA >= PatchingC)
        PatchingA = PatchingC > 2 ? PatchingC - 1 : 1;
    if (NonsequentialMoveType == -1 ||
        NonsequentialMoveType > K + PatchingC + PatchingA - 1)
        NonsequentialMoveType = K + PatchingC + PatchingA - 1;
    if (PatchingC >= 1 && NonsequentialMoveType >= 4) {
        BestMove = BestSubsequentMove = BestKOptMove;
        if (!SubsequentPatching && SubsequentMoveType <= 5) {
            MoveFunction BestOptMove[] =
                { 0, 0, Best2OptMove, Best3OptMove,
                Best4OptMove, Best5OptMove
            };
            BestSubsequentMove = BestOptMove[SubsequentMoveType];
        }
    } else {
        MoveFunction BestOptMove[] = { 0, 0, Best2OptMove, Best3OptMove,
            Best4OptMove, Best5OptMove
        };
        BestMove = MoveType <= 5 ? BestOptMove[MoveType] : BestKOptMove;
        BestSubsequentMove = SubsequentMoveType <= 5 ?
            BestOptMove[SubsequentMoveType] : BestKOptMove;
    }
    if (TraceLevel >= 1) {
        printff("done\n");
        PrintParameters();
    }
    if (InitialTourFileName)
        ReadTour(InitialTourFileName, &InitialTourFile);
    if (InputTourFileName)
        ReadTour(InputTourFileName, &InputTourFile);
    if (SubproblemTourFileName && SubproblemSize > 0)
        ReadTour(SubproblemTourFileName, &SubproblemTourFile);
    if (MergeTourFiles >= 1) {
        free(MergeTourFile);
        assert(MergeTourFile =
               (FILE **) malloc(MergeTourFiles * sizeof(FILE *)));
        for (i = 0; i < MergeTourFiles; i++)
            ReadTour(MergeTourFileName[i], &MergeTourFile[i]);
    }
    // end of ReadProblem code

    if (SubproblemSize > 0) {
        if (DelaunayPartitioning)
            SolveDelaunaySubproblems();
        else if (KarpPartitioning)
            SolveKarpSubproblems();
        else if (KMeansPartitioning)
            SolveKMeansSubproblems();
        else if (RohePartitioning)
            SolveRoheSubproblems();
        else if (SierpinskiPartitioning || MoorePartitioning)
            SolveSFCSubproblems();
        else
            SolveTourSegmentSubproblems();
        return EXIT_SUCCESS;
    }
    AllocateStructures();
    CreateCandidateSet();
    InitializeStatistics();

    if (Norm != 0)
        BestCost = PLUS_INFINITY;
    else {
        /* The ascent has solved the problem! */
        Optimum = BestCost = (GainType) LowerBound;
        UpdateStatistics(Optimum, GetTime() - LastTime);
        RecordBetterTour();
        RecordBestTour();
        /*
        WriteTour(OutputTourFileName, BestTour, BestCost);
        WriteTour(TourFileName, BestTour, BestCost);
        */
        Runs = 0;
    }

    /* Find a specified number (Runs) of local optima */
    for (Run = 1; Run <= Runs; Run++) {
        LastTime = GetTime();
        Cost = FindTour();      /* using the Lin-Kernighan heuristic */
        if (MaxPopulationSize > 0) {
            /* Genetic algorithm */
            int i;
            for (i = 0; i < PopulationSize; i++)
                Cost = MergeTourWithIndividual(i);
            if (!HasFitness(Cost)) {
                if (PopulationSize < MaxPopulationSize) {
                    AddToPopulation(Cost);
                    if (TraceLevel >= 1)
                        PrintPopulation();
                } else if (Cost < Fitness[PopulationSize - 1]) {
                    ReplaceIndividualWithTour(PopulationSize - 1, Cost);
                    if (TraceLevel >= 1)
                        PrintPopulation();
                }
            }
        } else if (Run > 1)
            Cost = MergeBetterTourWithBestTour();
        if (Cost < BestCost) {
            BestCost = Cost;
            RecordBetterTour();
            RecordBestTour();
            /*
            WriteTour(OutputTourFileName, BestTour, BestCost);
            WriteTour(TourFileName, BestTour, BestCost);
            */
        }
        if (Cost < Optimum) {
            if (FirstNode->InputSuc) {
                Node *N = FirstNode;
                while ((N = N->InputSuc = N->Suc) != FirstNode);
            }
            Optimum = Cost;
            printff("*** New optimum = " GainFormat " ***\n\n", Optimum);
        }
        Time = fabs(GetTime() - LastTime);
        UpdateStatistics(Cost, Time);
        if (TraceLevel >= 1 && Cost != PLUS_INFINITY) {
            printff("Run %d: Cost = " GainFormat, Run, Cost);
            if (Optimum != MINUS_INFINITY && Optimum != 0)
                printff(", Gap = %0.3f%%",
                        100.0 * (Cost - Optimum) / Optimum);
            printff(", Time = %0.1f sec. %s\n\n", Time,
                    Cost < Optimum ? "<" : Cost == Optimum ? "=" : "");
        }
        if (PopulationSize >= 2 && Run >= MaxPopulationSize &&
            Run < Runs) {
            Node *N;
            int Parent1, Parent2;
            Parent1 = LinearSelection(PopulationSize, 1.25);
            do
                Parent2 = LinearSelection(PopulationSize, 1.25);
            while (Parent1 == Parent2);
            ApplyCrossover(Parent1, Parent2);
            N = FirstNode;
            do {
                int d = C(N, N->Suc);
                AddCandidate(N, N->Suc, d, INT_MAX);
                AddCandidate(N->Suc, N, d, INT_MAX);
                N = N->InitialSuc = N->Suc; 
            } 
            while (N != FirstNode);
        }
        SRandom(++Seed);
    }

    // Now allocate space for node order, fill the numbers and return it.
    int *order = (int *) malloc(sizeof(int)*Dimension);
    for (int in=0;in<Dimension;in++) {
        order[in] = BestTour[in]-1;
    }
    return order;
}
