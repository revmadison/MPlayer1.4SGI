#ifndef _STDATOMIC_H
#define _STDATOMIC_H
/* OLD WAY
#include <pthread.h>

typedef struct _atomic_int
{
	//int initialized = 0;
	pthread_mutex_t mutex;
	int value;
} atomic_int;
#define atomic_uint atomic_int
*/

// NEW WAY
typedef int atomic_int;
typedef unsigned int atomic_uint;

void atomic_init(atomic_int *at, int value);
int atomic_load(atomic_int *at);
void atomic_store(atomic_int *at, int v);
int atomic_fetch_add(atomic_int *at, int v);

#define atomic_load_explicit(a, b) atomic_load(a)
#define atomic_store_explicit(a, b, c) atomic_store(a, b)
#define atomic_fetch_add_explicit(a, b, c) atomic_fetch_add(a, b)

#define ATOMIC_VAR_INIT(v) v
// OLD WAY static inline atomic_int ATOMIC_VAR_INIT(int v) { atomic_int at; atomic_init(&at, v); return at; }

#endif
