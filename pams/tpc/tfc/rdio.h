/* prototypes for rdio package */

int     rdio_wrt_event (char *path, int event, 
                        int n_seq, int *is_start, int *is_stop,
                        int *data);
int     rdio_rd_event (char *path, int *event, int notimeout, 
                       int *n_seq, int *is_start, int *is_stop, 
                       int *data);
