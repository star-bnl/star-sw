#include "LKH.h"
#include "Delaunay.h"

/* 
 * Delaunay triangulation by straightline divide-and-conquer.   
 *
 * Implementation based on the code of Guibas and Stolfi:
 *
 *   L. Guibas and J. Stolfi,
 *   "Primitives for the Manipulation of General Subdivisions    
 *    and the Computation of Voronoi Diagrams".
 *   ACM Transcations on Graphics, April 1985.
 *
 *   http://www.ic.unicamp.br/~stolfi       
 *
 * See the copyright notice at the end of this file.
 */

#define HAVE_NEXTAFTERF

/* Undefine if you don't have the nextafterf function */
/* #undef HAVE_NEXTAFTERF */

#define HAVE_SCALBNF

/* Undefine if you don't have the scalbnf function */
/* #undef HAVE_SCALBNF */

#ifndef HAVE_NEXTAFTERF
float nextafterf(const float x, const float y);
#endif

typedef Node site_struct;

#define ORG(e) ((Node *) ODATA(e))
#define DEST(e) ((Node *) DDATA(e))

static int compare(const void *s1, const void *s2);
static void splice(edge_ref a, edge_ref b);
static void rec_delaunay(site_struct * sites[], int sl, int sh,
                         edge_ref * le, edge_ref * re);

edge_ref delaunay_build(int nsites)
{
    edge_ref le, re;
    site_struct **sites;
    Node *N;
    int i, j;

    assert(sites =
           (site_struct **) malloc(nsites * sizeof(site_struct *)));
    for (i = 0, N = FirstNode; i < nsites; i++, N = N->Suc) {
        sites[i] = N;
        N->Yc = N->Y;
    }
    qsort(sites, nsites, sizeof(site_struct *), compare);
    /* Perturb duplicate points a bit in their Y-coordinate */
    for (i = 0; i < nsites; i++) {
        for (j = i + 1; j < nsites; j++)
            if ((float) sites[j]->X != (float) sites[i]->X ||
                (float) sites[j]->Y != (float) sites[i]->Y)
                break;
        for (j--; i < j; i++)
            sites[i + 1]->Y = nextafterf((float) sites[i]->Y, FLT_MAX);
    }
    rec_delaunay(sites, 0, nsites, &le, &re);
    for (i = 0; i < nsites; i++)
        sites[i]->Y = sites[i]->Yc;
    free(sites);
    return le;
}

static unsigned next_mark = 1;

edge_ref *delaunay_edges(int nsites)
{
    edge_ref *array, *stack;
    int edges = 0, top = -1;
    unsigned mark = next_mark;

    assert(array =
           (edge_ref *) malloc((3 * nsites - 5) * sizeof(edge_ref)));
    assert(stack =
           (edge_ref *) malloc((3 * nsites - 6) * sizeof(edge_ref)));
    if (++next_mark == 0)
        next_mark = 1;
    stack[++top] = delaunay_build(nsites);
    while (top != -1) {
        edge_ref e = stack[top--];
        while (MARK(e) != mark) {
            MARK(e) = mark;
            array[edges++] = e;
            stack[++top] = ONEXT(e);
            e = ONEXT(SYM(e));
        }
    }
    array[edges] = 0;
    free(stack);
    return array;
}

void destroy_edge(edge_ref e)
{
    edge_ref f = SYM(e);
    if (ONEXT(e) != e)
        splice(e, OPREV(e));
    if (ONEXT(f) != f)
        splice(f, OPREV(f));
    free((char *) (e & MASK));
}

static edge_ref make_edge(void)
{
    edge_ref e;

    assert(e = (edge_ref) malloc(sizeof(edge_struct)));
    ONEXT(e) = e;
    ROTRNEXT(e) = TOR(e);
    SYMDNEXT(e) = SYM(e);
    TORLNEXT(e) = ROT(e);
    MARK(e) = 0;
    return e;
}

static void splice(edge_ref a, edge_ref b)
{
    edge_ref ta, tb;
    edge_ref alpha = ROT(ONEXT(a));
    edge_ref beta = ROT(ONEXT(b));

    ta = ONEXT(a);
    tb = ONEXT(b);
    ONEXT(a) = tb;
    ONEXT(b) = ta;
    ta = ONEXT(alpha);
    tb = ONEXT(beta);
    ONEXT(alpha) = tb;
    ONEXT(beta) = ta;
}

static double ccw(site_struct * a, site_struct * b, site_struct * c)
{
    double x1 = (float) a->X, y1 = (float) a->Y;
    double x2 = (float) b->X, y2 = (float) b->Y;
    double x3 = (float) c->X, y3 = (float) c->Y;

    return (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1);
}

static int rightof(site_struct * s, edge_ref e)
{
    return ccw(s, DEST(e), ORG(e)) > 0.0;
}

static int leftof(site_struct * s, edge_ref e)
{
    return ccw(s, ORG(e), DEST(e)) > 0.0;
}

static int
incircle(site_struct * a, site_struct * b, site_struct * c,
         site_struct * d)
{
    double x1 = (float) a->X, y1 = (float) a->Y;
    double x2 = (float) b->X, y2 = (float) b->Y;
    double x3 = (float) c->X, y3 = (float) c->Y;
    double x4 = (float) d->X, y4 = (float) d->Y;

    return ((y4 - y1) * (x2 - x3) +
            (x4 - x1) * (y2 - y3)) * ((x4 - x3) * (x2 - x1) - (y4 -
                                                               y3) * (y2 -
                                                                      y1))
        > ((y4 - y3) * (x2 - x1) +
           (x4 - x3) * (y2 - y1)) * ((x4 - x1) * (x2 - x3) - (y4 -
                                                              y1) * (y2 -
                                                                     y3));
}

static int compare(const void *s1, const void *s2)
{
    double x1 = (*((site_struct **) s1))->X;
    double y1 = (*((site_struct **) s1))->Y;
    double x2 = (*((site_struct **) s2))->X;
    double y2 = (*((site_struct **) s2))->Y;

    return x1 < x2 ? -1 : x1 > x2 ? 1 : y1 < y2 ? -1 : y1 > y2 ? 1 : 0;
}

static edge_ref connect(edge_ref a, edge_ref b)
{
    edge_ref e = make_edge();
    ODATA(e) = DEST(a);
    DDATA(e) = ORG(b);
    splice(e, LNEXT(a));
    splice(SYM(e), b);
    return e;
}

static void
rec_delaunay(site_struct * sites[],
             int sl, int sh, edge_ref * le, edge_ref * re)
{
    if (sh == sl + 2) {
        edge_ref a = make_edge();
        ODATA(a) = sites[sl];
        DDATA(a) = sites[sl + 1];
        *le = a;
        *re = SYM(a);
    } else if (sh == sl + 3) {
        edge_ref a = make_edge();
        edge_ref b = make_edge();
        double ct = ccw(sites[sl], sites[sl + 1], sites[sl + 2]);
        splice(SYM(a), b);
        ODATA(a) = sites[sl];
        DDATA(a) = sites[sl + 1];
        ODATA(b) = sites[sl + 1];
        DDATA(b) = sites[sl + 2];
        if (ct == 0.0) {
            *le = a;
            *re = SYM(b);
        } else {
            edge_ref c = connect(b, a);
            if (ct > 0.0) {
                *le = a;
                *re = SYM(b);
            } else {
                *le = SYM(c);
                *re = c;
            }
        }
    } else {
        edge_ref ldo, ldi, rdi, rdo;
        edge_ref basel, lcand, rcand;

        int sm = (sl + sh) / 2;

        rec_delaunay(sites, sl, sm, &ldo, &ldi);
        rec_delaunay(sites, sm, sh, &rdi, &rdo);

        while (1) {
            if (leftof(ORG(rdi), ldi))
                ldi = LNEXT(ldi);
            else if (rightof(ORG(ldi), rdi))
                rdi = ONEXT(SYM(rdi));
            else
                break;
        }

        basel = connect(SYM(rdi), ldi);
        if (ORG(ldi) == ORG(ldo))
            ldo = SYM(basel);
        if (ORG(rdi) == ORG(rdo))
            rdo = basel;

        while (1) {
            lcand = ONEXT(SYM(basel));
            if (rightof(DEST(lcand), basel))
                while (incircle
                       (DEST(basel), ORG(basel), DEST(lcand),
                        DEST(ONEXT(lcand)))) {
                    edge_ref t = ONEXT(lcand);
                    destroy_edge(lcand);
                    lcand = t;
                }

            rcand = OPREV(basel);
            if (rightof(DEST(rcand), basel))
                while (incircle
                       (DEST(basel), ORG(basel), DEST(rcand),
                        DEST(OPREV(rcand)))) {
                    edge_ref t = OPREV(rcand);
                    destroy_edge(rcand);
                    rcand = t;
                }

            if (!rightof(DEST(lcand), basel)
                && !rightof(DEST(rcand), basel))
                break;

            if (!rightof(DEST(lcand), basel) ||
                (rightof(DEST(rcand), basel) &&
                 incircle(DEST(lcand), ORG(lcand), ORG(rcand),
                          DEST(rcand))))
                basel = connect(rcand, SYM(basel));
            else
                basel = connect(SYM(basel), SYM(lcand));
        }
        *le = ldo;
        *re = rdo;
    }
}

/*
** Copyright notice:
**
** Copyright 1996 Institute of Computing, Unicamp.
**
** Permission to use this software for any purpose is hereby granted,
** provided that any substantial copy or mechanically derived version
** of this file that is made available to other parties is accompanied
** by this copyright notice in full, and is distributed under these same
** terms. 
**
** NOTE: this copyright notice does not claim to supersede any copyrights
** that may apply to the original DEC implementation of the quad-edge
** data structure.
**
** DISCLAIMER: This software is provided "as is" with no explicit or
** implicit warranty of any kind.  Neither the authors nor their
** employers can be held responsible for any losses or damages
** that might be attributed to its use.
**
** End of copyright notice.
*/

#ifndef HAVE_NEXTAFTERF

/* This is a portable implementation of nextafterf that is intended to be
 * independent of the floating point format or its in memory representation.
 * This implementation skips denormalized values, for example returning
 * FLT_MIN as the next value after zero, as many target's frexpf, scalbnf
 * and ldexpf functions don't work as expected with denormalized values.
 *
 * Algorithm by Steven G. Kargl.
 */

#ifndef HAVE_SCALBNF
float scalbnf(const float x, const int n)
{
    return (float) (x * pow(2.0, n));
}
#endif

float nextafterf(const float x, const float y)
{
    int origexp, newexp;
#ifdef isnan
    if (isnan(x) || isnan(y))
        return x + y;
#endif
    if (x == y)
        return x;
    if (x == 0.0f)
        return y > 0.0 ? FLT_MIN : -FLT_MIN;
    frexpf(x, &origexp);
    if (x >= 0.0f) {
        if (y > x) {
            if (x < FLT_MIN)
                return FLT_MIN;
            return x + scalbnf(FLT_EPSILON, origexp - 1);
        } else if (x > FLT_MIN) {
            float temp = x - scalbnf(FLT_EPSILON, origexp - 1);
            frexpf(temp, &newexp);
            if (newexp == origexp)
                return temp;
            return x - scalbnf(FLT_EPSILON, origexp - 2);
        } else
            return 0.0f;
    } else {
        if (y < x) {
            if (x > -FLT_MIN)
                return -FLT_MIN;
            return x - scalbnf(FLT_EPSILON, origexp - 1);
        } else if (x < -FLT_MIN) {
            float temp = x + scalbnf(FLT_EPSILON, origexp - 1);
            frexpf(temp, &newexp);
            if (newexp == origexp)
                return temp;
            return x + scalbnf(FLT_EPSILON, origexp - 2);
        } else
            return 0.0f;
    }
}
#endif
