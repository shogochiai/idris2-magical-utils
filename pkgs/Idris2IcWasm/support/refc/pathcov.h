// Path-coverage runtime prototypes (see pathcov.c).
// Force-included into RefC-generated C so idris2_recordPathHit (emitted by the
// RefC backend for prim__recordPathHit under --dumppathshits) has a declaration.
#ifndef IDRIS2_PATHCOV_H
#define IDRIS2_PATHCOV_H
#include <stdint.h>
void *idris2_recordPathHit(const char *pathId);
uint32_t __dfxcov_path_hit_count(void);
uint32_t __dfxcov_path_hit_saturated(void);
void __dfxcov_reset_path_hits(void);
const char *__dfxcov_format_path_hits(void);
#endif
