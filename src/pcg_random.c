#include "pcg_random.h"

#include <math.h>

typedef uint32_t u32;

typedef struct {
    u64 state;
    u64 increment;
} PcgRng;

static PcgRng RNG = {
    .state = 9600629759793949339ull,
    .increment = 15726070495360670683ull,
};

static u32 get_random_u32(PcgRng* rng) {
    const u64 state = rng->state;
    rng->state = (state * 6364136223846793005ull) + (rng->increment | 1u);
    const u32 xor_shift = (u32)(((state >> 18u) ^ state) >> 27u);
    const u32 rotate = (u32)(state >> 59u);
    return (xor_shift >> rotate) | (xor_shift << ((-rotate) & 31u));
}

f32 get_random_f32(void) {
    return ldexpf((f32)get_random_u32(&RNG), -32);
}

void set_seed(u64 state, u64 increment) {
    RNG.state = 0u;
    RNG.increment = (increment << 1u) | 1u;
    get_random_u32(&RNG);
    RNG.state += state;
    get_random_u32(&RNG);
}
