#ifndef __SOCDL_H__
#define __SOCDL_H__

#ifdef __cplusplus
extern "C" {
#endif

int soc_dl_load(char *pkgName);
int soc_dl_init(char *pkgName);
int soc_dl_start(char *pkgName);
int soc_dl_stop(char *pkgName);

#ifdef __cplusplus
}
#endif

#endif /* __SOCDL_H__ */
