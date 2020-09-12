#ifndef __PCG_RANDOM_H__
#define __PCG_RANDOM_H__

#include <stdint.h>

typedef uint64_t u64;
typedef float    f32;

void set_seed(u64, u64);

f32 get_random_f32(void);

#endif
