/* Structures to steer the desired Micky Mouse Generators for GEANT */

typedef struct phase_space_t
{
int     ipart;
char    name[20];
int     itrtyp;
float   mass;
float   charge;
float   tlife;
float   y_low;
float   y_high;
float   pt_low;
float   pt_high;
int     n_track_per_event;
float   phi_low;
float   phi_high;
} phase_space_t;

typedef struct  momentum_bin_t
{
int     ipart;
char    name[20];
int     itrtyp;
float   mass;
float   charge;
float   tlife;
float   px_low;
float   px_high;
float   py_low;
float   py_high;
float   pz_low;
float   pz_high;
int     n_track_per_event;
} momentum_bin_t;

