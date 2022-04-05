#include "LKH.h"

/*
 * The Distance_SPECIAL function may be used to specify a user defined
 * distance fuction. The function is used when the EDGE_WEIGHT_TYPE is
 * SPECIAL. 
 * 
 * Example:
 *  
 *      int Distance_SPECIAL(Node * Na, Node * Nb) 
 *      {
 *           double dx = Na->X - Nb->X;
 *           double dy = Na->Y - Nb->Y;
 *           return (int) (1000 * sqrt(dx * dx + dy * dy));
 *      }           
 */

/*
int Distance_SPECIAL(Node * Na, Node * Nb)
{
    const double GridSize = 100000000;
    double dx = Na->X - Nb->X;
    double dy = Na->Y - Nb->Y;
    if (dx < 0)
        dx = -dx;
    if (dy < 0)
        dy = -dy;
    if (GridSize - dx < dx)
        dx = GridSize - dx;
    if (GridSize - dy < dy)
        dy = GridSize - dy;
    return (int) (sqrt(dx * dx + dy * dy) + 0.5);
}
 */

int Distance_SPECIAL(Node * Na, Node * Nb)
{
    double penalty = 0;
    /*
    f1 and f2 are file indices in chain.
     */
    int f1 = (int) Na->Z;
    int f2 = (int) Nb->Z;
    /*
    ie1 and ie2 are event indices in chain.
     */
    int ie1 = (int) 1000000.0*(Na->Z-f1)+0.5;
    int ie2 = (int) 1000000.0*(Nb->Z-f2)+0.5;
    /*
      If we only weight against mixing events in separate files then reading
      in sorted order takes longer than I want.
      Including a preference for nearby events helps reading speed and doesn't
      hurt sorting too much. I think bouncing around between a few files is
      better than  completely random file selection.
     */
    if (abs(ie1-ie2) > Dimension/2) {
        penalty = Dimension - fabs( (double) (ie1-ie2) );
    } else {
        penalty = fabs( (double) (ie1-ie2) );
    }
    /*
     Moving around within a file is faster than jumping to a different file.
     There is a cost if we move too far away though.
     */
    if (f1 == f2) {
        penalty *= 0.1;
        penalty += 10;
    }

    return (int) (fabs(Na->X - Nb->X) +
                  fabs(Na->Y - Nb->Y) + penalty);
}
