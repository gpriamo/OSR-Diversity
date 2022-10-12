#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int seed = 0;
double rnd = 0.0;

int toss_coin() {
    if (seed == 0) {
        srand(time(NULL)); // Initialize the seed for toss_coin()
        seed = 1;
    }

    double tmp = rand();
    rnd = tmp;
    
    if (tmp < RAND_MAX/2)
        return 0;

    return 1;
}

int throw_dice(int faces) {
    double toss = (int)rnd % (faces);
    return (int)toss;
}