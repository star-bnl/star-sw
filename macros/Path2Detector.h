struct VolumeMap_t {
  Char_t *name;
  Char_t *comment;
  Char_t *path;
  Char_t *set;
  Char_t *det;
};
static VolumeMap_t VolumesToBePut[] = {
{"SVTD", "an active wafer volume","HALL_1/CAVE_1/SVTT_1/SLY*/SLS*/SLD*/STL*/STS*/SVTD_1","svt","SVTD"}, // <+++
