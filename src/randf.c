#include <stdlib.h>

#include "randf.h"

/* NOTE: See `https://github.com/imneme/pcg-c-basic/blob/master/pcg_basic.c`
 * for a more advanced version of this bounding maneuver.
 */
float randf(void) {
    for (;;) {
        unsigned int x = rand();
        if (x != RAND_MAX) {
            return (float)x / (float)RAND_MAX;
        }
    }
}
